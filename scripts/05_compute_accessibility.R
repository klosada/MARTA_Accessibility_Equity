source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Door-to-door transit travel times from every tract to each destination,
# rail only and multimodal (bus + rail).

origins <- readRDS(here::here("data_processed", "origins.rds"))
destinations <- readRDS(here::here("data_processed", "destinations.rds"))
all_stops <- readRDS(here::here("data_processed", "all_stops.rds"))
rail_stops <- readRDS(here::here("data_processed", "rail_stops.rds"))
stop_times_full <- readRDS(here::here("data_processed", "stop_times_full.rds"))
stop_times_rail <- readRDS(here::here("data_processed", "stop_times_rail.rds"))
walk_net <- readRDS(here::here("data_processed", "walk_net.rds"))
partial_path <- here::here("data_processed", "travel_times_long_partial.rds")

# Walk links from each tract origin to nearby stops.
compute_origin_stop_links <- function(origins_sf, stops_sf) {
  compute_network_links(
    network = walk_net,
    from_sf = origins_sf,
    to_sf = stops_sf,
    from_cols = c("GEOID"),
    to_cols = c("stop_id", "stop_name"),
    max_dist = max_walk_meters
  )
}

message("Building origin-stop lookup...")
origin_links <- bind_rows(
  compute_origin_stop_links(origins, rail_stops) %>% mutate(scenario = "rail_only"),
  compute_origin_stop_links(origins, all_stops) %>% mutate(scenario = "bus_rail")
)

# Walk links from each destination to nearby stops (the final walk leg).
build_destination_lookup <- function(destinations_sf, stops_sf, scenario_name) {
  lookup <- compute_network_links(
    network = walk_net,
    from_sf = destinations_sf,
    to_sf = stops_sf,
    from_cols = c("dest_id", "dest_name", "dest_type"),
    to_cols = c("stop_id", "stop_name"),
    max_dist = max_walk_meters
  ) %>%
    transmute(
      dest_id,
      dest_name,
      dest_type,
      scenario = scenario_name,
      dest_stop_id = stop_id,
      dest_stop_name = stop_name,
      dest_walk_time_min = walk_time_min
    )

  if (!nrow(lookup)) {
    stop(glue("No destination stops found for scenario '{scenario_name}'. Increase max_walk_meters or inspect stop coverage."))
  }

  lookup
}

message("Building destination-stop lookup...")
destination_stop_lookup <- bind_rows(
  build_destination_lookup(destinations, rail_stops, "rail_only"),
  build_destination_lookup(destinations, all_stops, "bus_rail")
)

save_rds(destination_stop_lookup, here::here("data_processed", "destination_stop_lookup.rds"))
save_rds(origin_links, here::here("data_processed", "origin_stop_lookup.rds"))

# Builds walking transfers between nearby stops, since the MARTA feed has no
# transfers.txt. Transfer time is the network walk time, at least 2 minutes.
build_stop_transfers <- function(stops_sf) {
  study_area <- readRDS(here::here("data_processed", "tracts_acs.rds")) %>%
    st_union() %>%
    st_buffer(3000)
  stops_in_area <- stops_sf[lengths(st_intersects(stops_sf, study_area)) > 0, ]

  walk_m <- sfnetworks::st_network_cost(walk_net, from = stops_in_area, to = stops_in_area, weights = "weight")
  pairs <- which(walk_m <= max_transfer_walk_meters, arr.ind = TRUE)

  tibble(
    from_stop_id = stops_in_area$stop_id[pairs[, 1]],
    to_stop_id = stops_in_area$stop_id[pairs[, 2]],
    walk_dist_m = as.numeric(walk_m[pairs])
  ) %>%
    filter(from_stop_id != to_stop_id) %>%
    transmute(
      from_stop_id,
      to_stop_id,
      transfer_type = 2L,
      min_transfer_time = pmax(min_transfer_seconds, round(walk_dist_m / walk_speed_m_per_min * 60))
    )
}

message("Building stop-to-stop walking transfers...")
stop_transfers <- build_stop_transfers(all_stops)
attr(stop_times_full, "transfers") <- stop_transfers
attr(stop_times_rail, "transfers") <- stop_transfers %>%
  filter(from_stop_id %in% rail_stops$stop_id, to_stop_id %in% rail_stops$stop_id)
save_rds(stop_transfers, here::here("data_processed", "stop_transfers.rds"))

# Scheduled departure and arrival times at each stop. Transfer walks are only
# used between two rides, so a trip must board and alight on a scheduled
# vehicle; walks to and from stops stay within the 800 m limit.
scheduled_keys <- function(stop_times_obj, time_col) {
  paste(stop_times_obj$stop_id, as.numeric(stop_times_obj[[time_col]]))
}
for (scenario_st in c("stop_times_full", "stop_times_rail")) {
  st_obj <- get(scenario_st)
  attr(st_obj, "scheduled_departures") <- unique(scheduled_keys(st_obj, "departure_time"))
  attr(st_obj, "scheduled_arrivals") <- unique(scheduled_keys(st_obj, "arrival_time"))
  assign(scenario_st, st_obj)
}

# Home departure times, one per minute across the departure window.
start_times <- seq(
  as.numeric(hms::as_hms(departure_start)),
  as.numeric(hms::as_hms(departure_end)),
  by = 60
)
# Transit departures run until the last rider (leaving at 8:00) reaches a stop.
raptor_time_range <- c(departure_start, format(hms::as_hms(max(start_times) + max_walk_meters / walk_speed_m_per_min * 60)))

# Door-to-door time for each home departure time: walk to the stop, wait for
# the next scheduled departure, ride, and walk to the destination. Uses the
# earliest arrival among trips the rider can still catch.
door_to_door_by_start <- function(leave_by, arrive) {
  ord <- order(leave_by)
  leave_by <- leave_by[ord]
  best_arrival <- rev(cummin(rev(arrive[ord])))
  next_trip <- findInterval(start_times, leave_by, left.open = TRUE) + 1
  tt <- rep(Inf, length(start_times))
  catchable <- next_trip <= length(leave_by)
  tt[catchable] <- (best_arrival[next_trip[catchable]] - start_times[catchable]) / 60
  tt
}

# Median door-to-door travel time from one tract to every destination across
# home departure times. Trips over the 2-hour cap count as unreachable.
calculate_origin_access <- function(origin_geoid, scenario_name, origin_link_tbl, stop_times_obj, dest_lookup_tbl) {
  candidate_stops <- origin_link_tbl %>%
    filter(GEOID == origin_geoid, scenario == scenario_name)

  if (!nrow(candidate_stops)) {
    return(destinations %>%
      st_drop_geometry() %>%
      transmute(
        GEOID = origin_geoid,
        scenario = scenario_name,
        dest_id,
        dest_name,
        dest_type,
        travel_time_min = NA_real_,
        best_time_min = NA_real_,
        reachable = FALSE
      ))
  }

  dest_links <- dest_lookup_tbl %>%
    filter(scenario == scenario_name)

  raptor_res <- tidytransit::raptor(
    stop_times = stop_times_obj,
    transfers = attr(stop_times_obj, "transfers"),
    stop_ids = candidate_stops$stop_id,
    time_range = raptor_time_range,
    max_transfers = max_transfers,
    keep = "all"
  )

  # Keeps every scheduled journey that ends at a destination stop.
  journeys <- raptor_res[raptor_res$to_stop_id %in% dest_links$dest_stop_id, ] %>%
    as_tibble() %>%
    inner_join(candidate_stops %>% select(stop_id, walk_time_min), by = c("from_stop_id" = "stop_id")) %>%
    inner_join(
      dest_links %>% select(dest_id, dest_stop_id, dest_walk_time_min),
      by = c("to_stop_id" = "dest_stop_id"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      leave_by = journey_departure_time - walk_time_min * 60,
      arrive = journey_arrival_time + dest_walk_time_min * 60
    )

  # Destinations sharing a stop with the tract can be reached on foot at any time.
  walk_only <- journeys %>%
    filter(from_stop_id == to_stop_id) %>%
    group_by(dest_id) %>%
    summarise(walk_only_min = suppressWarnings(min(walk_time_min + dest_walk_time_min)), .groups = "drop")

  dest_summary <- journeys %>%
    filter(
      from_stop_id != to_stop_id,
      paste(from_stop_id, journey_departure_time) %in% attr(stop_times_obj, "scheduled_departures"),
      paste(to_stop_id, journey_arrival_time) %in% attr(stop_times_obj, "scheduled_arrivals")
    ) %>%
    group_by(dest_id) %>%
    summarise(tt = list(door_to_door_by_start(leave_by, arrive)), .groups = "drop") %>%
    full_join(walk_only, by = "dest_id") %>%
    mutate(
      tt = purrr::map2(tt, walk_only_min, function(tt, walk_min) {
        if (is.null(tt)) tt <- rep(Inf, length(start_times))
        if (!is.na(walk_min)) tt <- pmin(tt, walk_min)
        tt[tt > max_trip_minutes] <- Inf
        tt
      }),
      travel_time_min = purrr::map_dbl(tt, median),
      best_time_min = purrr::map_dbl(tt, min),
      across(c(travel_time_min, best_time_min), ~ if_else(is.infinite(.x), NA_real_, .x))
    ) %>%
    left_join(dest_links %>% distinct(dest_id, dest_name, dest_type), by = "dest_id") %>%
    mutate(
      GEOID = origin_geoid,
      scenario = scenario_name,
      reachable = !is.na(travel_time_min)
    ) %>%
    select(GEOID, scenario, dest_id, dest_name, dest_type, travel_time_min, best_time_min, reachable)

  destinations %>%
    st_drop_geometry() %>%
    select(dest_id, dest_name, dest_type) %>%
    left_join(dest_summary, by = c("dest_id", "dest_name", "dest_type")) %>%
    mutate(
      GEOID = origin_geoid,
      scenario = scenario_name,
      reachable = replace_na(reachable, FALSE)
    ) %>%
    select(GEOID, scenario, dest_id, dest_name, dest_type, travel_time_min, best_time_min, reachable)
}

# Saves progress every 25 tracts so an interrupted run can resume.
start_index <- 1L
results_list <- vector("list", length(origins$GEOID))

if (file.exists(partial_path)) {
  partial_existing <- readRDS(partial_path)
  partial_existing <- partial_existing %>%
    filter(GEOID %in% origins$GEOID, dest_id %in% destinations$dest_id)

  completed_geoids <- unique(partial_existing$GEOID)
  if (length(completed_geoids)) {
    completed_positions <- match(completed_geoids, origins$GEOID)
    valid_positions <- which(!is.na(completed_positions))
    if (length(valid_positions)) {
      results_list[completed_positions[valid_positions]] <- split(partial_existing, partial_existing$GEOID)[valid_positions]
      start_index <- max(completed_positions[valid_positions]) + 1L
      message(glue("Resuming accessibility routing at tract {start_index} of {length(origins$GEOID)}..."))
    }
  }
}

message(glue("Computing accessibility for {length(origins$GEOID)} tracts..."))
for (i in seq.int(start_index, length(origins$GEOID))) {
  geoid <- origins$GEOID[[i]]
  results_list[[i]] <- bind_rows(
    calculate_origin_access(geoid, "rail_only", origin_links, stop_times_rail, destination_stop_lookup),
    calculate_origin_access(geoid, "bus_rail", origin_links, stop_times_full, destination_stop_lookup)
  )

  if (i %% 25 == 0 || i == length(origins$GEOID)) {
    partial_tbl <- bind_rows(results_list[seq_len(i)])
    save_rds(partial_tbl, partial_path)
    message(glue("Completed {i} of {length(origins$GEOID)} tracts."))
  }
}

travel_times_long <- bind_rows(results_list)
save_rds(travel_times_long, here::here("data_processed", "travel_times_long.rds"))
unlink(partial_path)  # the next run starts fresh
readr::write_csv(travel_times_long, here::here("outputs", "tables", "travel_times_long.csv"))
