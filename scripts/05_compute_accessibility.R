source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Computes door-to-door transit travel times from every census tract to each
# destination under two scenarios: rail only and full bus + rail network.

origins <- readRDS(here::here("data_processed", "origins.rds"))
destinations <- readRDS(here::here("data_processed", "destinations.rds"))
all_stops <- readRDS(here::here("data_processed", "all_stops.rds"))
rail_stops <- readRDS(here::here("data_processed", "rail_stops.rds"))
stop_times_full <- readRDS(here::here("data_processed", "stop_times_full.rds"))
stop_times_rail <- readRDS(here::here("data_processed", "stop_times_rail.rds"))
walk_net <- readRDS(here::here("data_processed", "walk_net.rds"))
partial_path <- here::here("data_processed", "travel_times_long_partial.rds")

# Computes walk links from each tract origin to nearby transit stops.
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

# Computes walk time from each policy-relevant destination to nearest 
# transit stops, providing the final walk leg needed for travel time estimates.
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

# Computes fastest scheduled transit trip from each tract's walkable
# stops to every destination, adding walk time at both the start and end
# of the trip for a complete travel time estimate.
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
        reachable = FALSE
      ))
  }

  raptor_res <- tidytransit::raptor(
    stop_times = stop_times_obj,
    transfers = attr(stop_times_obj, "transfers"),
    stop_ids = candidate_stops$stop_id,
    time_range = peak_time_range,
    max_transfers = max_transfers,
    keep = "all"
  ) %>%
    as_tibble() %>%
    left_join(candidate_stops %>% select(stop_id, walk_time_min), by = c("from_stop_id" = "stop_id")) %>%
    mutate(total_time_min = seconds_to_minutes(travel_time) + walk_time_min)

  dest_summary <- dest_lookup_tbl %>%
    filter(scenario == scenario_name) %>%
    left_join(
      raptor_res,
      by = c("dest_stop_id" = "to_stop_id")
    ) %>%
    mutate(total_time_min = total_time_min + dest_walk_time_min) %>%
    group_by(dest_id, dest_name, dest_type) %>%
    summarise(
      travel_time_min = suppressWarnings(min(total_time_min, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      travel_time_min = if_else(is.infinite(travel_time_min), NA_real_, travel_time_min),
      GEOID = origin_geoid,
      scenario = scenario_name,
      reachable = !is.na(travel_time_min)
    ) %>%
    select(GEOID, scenario, dest_id, dest_name, dest_type, travel_time_min, reachable)

  destinations %>%
    st_drop_geometry() %>%
    select(dest_id, dest_name, dest_type) %>%
    left_join(dest_summary, by = c("dest_id", "dest_name", "dest_type")) %>%
    mutate(
      GEOID = origin_geoid,
      scenario = scenario_name,
      reachable = replace_na(reachable, FALSE)
    ) %>%
    select(GEOID, scenario, dest_id, dest_name, dest_type, travel_time_min, reachable)
}

# Saves progress every 25 tracts so interrupted runs can resume from last checkpoint.
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

# Combines all tract results and saves the full travel times table.
travel_times_long <- bind_rows(results_list)
save_rds(travel_times_long, here::here("data_processed", "travel_times_long.rds"))
readr::write_csv(travel_times_long, here::here("outputs", "tables", "travel_times_long.csv"))
