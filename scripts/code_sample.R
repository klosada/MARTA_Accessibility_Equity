# ============================================================
# MARTA Accessibility and Equity Analysis
# Code sample - Katherine Losada
#
# Measures how well MARTA rail only vs. bus + rail connects Atlanta census tracts
# to 7 key destinations (job centers, universities, hospitals, airport) during the
# weekday morning peak, and whether access gaps fall on higher-need neighborhoods.
#
# Full code: https://github.com/klosada/MARTA_Accessibility_Equity
# Report: https://klosada.github.io/MARTA_Accessibility_Equity
#
# Excerpt from a 9-script R pipeline run by 99_run_all.R (01-04 prepare inputs):
#   1. 00_utils.R helpers: choose_gtfs_analysis_date(), compute_network_links()
#   2. 05_compute_accessibility.R: walk links and tidytransit::raptor() routing
#   3. 06_summarize_metrics.R: destinations within 20/30/45 min, travel time stats
# ============================================================


# ------------------------------------------------------------
# Helper functions (from 00_utils.R)
# ------------------------------------------------------------
# Picks a representative weekday GTFS service date with no calendar exceptions, so the 
# analysis reflects a standard weekday schedule rather than a holiday or reduced-service day.

choose_gtfs_analysis_date <- function(gtfs, target_weekday = analysis_weekday) {
  if (!"calendar" %in% names(gtfs)) {
    stop("GTFS feed must include a calendar table.")
  }

  cal <- gtfs$calendar %>%
    transmute(
      service_id,
      start_date = as.Date(start_date, "%Y%m%d"),
      end_date = as.Date(end_date, "%Y%m%d"),
      active = .data[[tolower(target_weekday)]]
    ) %>%
    filter(active == 1)

  if (!nrow(cal)) {
    stop(glue("No active {target_weekday} service found in GTFS calendar table."))
  }

  candidate_dates <- seq(min(cal$start_date), max(cal$end_date), by = "day")
  candidate_dates <- candidate_dates[weekdays(candidate_dates) == stringr::str_to_title(target_weekday)]

  exceptions <- if ("calendar_dates" %in% names(gtfs) && nrow(gtfs$calendar_dates)) {
    gtfs$calendar_dates %>% mutate(date = as.Date(date, "%Y%m%d"))
  } else {
    tibble(service_id = character(), date = as.Date(character()), exception_type = integer())
  }

  for (dt in candidate_dates) {
    base_ids <- cal %>%
      filter(start_date <= dt, end_date >= dt) %>%
      pull(service_id)

    added_ids <- exceptions %>%
      filter(date == dt, exception_type == 1) %>%
      pull(service_id)

    removed_ids <- exceptions %>%
      filter(date == dt, exception_type == 2) %>%
      pull(service_id)

    active_ids <- union(setdiff(base_ids, removed_ids), added_ids)

    if (length(active_ids) && !length(added_ids) && !length(removed_ids)) {
      return(dt)
    }
  }

  stop(glue("No regular {target_weekday} date found in GTFS feed without calendar exceptions."))
}

# Calculates walking distance between spatial features (census tract centroids to nearby
# transit stops) using the OpenStreetMap pedestrian network and a shortest-path algorithm, 
# retaining only connections within the 800-meter maximum walkable distance threshold.

compute_network_links <- function(network, from_sf, to_sf, from_cols, to_cols, max_dist) {
  candidate_rows <- sf::st_is_within_distance(from_sf, to_sf, dist = max_dist * 2)

  purrr::imap_dfr(candidate_rows, function(to_rows, from_row) {
    if (!length(to_rows)) {
      return(tibble())
    }

    net_dists <- sfnetworks::st_network_cost(
      network,
      from = from_sf[from_row, ],
      to = to_sf[to_rows, ],
      weights = "weight"
    )

    keep_idx <- which(net_dists[1, ] <= max_dist)
    if (!length(keep_idx)) {
      return(tibble())
    }

    tibble(
      row_id = from_row,
      stop_row = to_rows[keep_idx],
      walk_dist_m = as.numeric(net_dists[1, keep_idx]),
      walk_time_min = walk_dist_m / walk_speed_m_per_min
    ) %>%
      mutate(
        from_data = purrr::map(row_id, ~ from_sf %>% st_drop_geometry() %>% slice(.x) %>% select(all_of(from_cols))),
        to_data = purrr::map(stop_row, ~ to_sf %>% st_drop_geometry() %>% slice(.x) %>% select(all_of(to_cols)))
      ) %>%
      tidyr::unnest(c(from_data, to_data))
  })
}


# ------------------------------------------------------------
# 05_compute_accessibility.R
# Core accessibility computation: multimodal transit routing
# ------------------------------------------------------------
# Inputs produced by scripts 01-04:

origins <- readRDS(here::here("data_processed", "origins.rds"))
destinations <- readRDS(here::here("data_processed", "destinations.rds"))
all_stops <- readRDS(here::here("data_processed", "all_stops.rds"))
rail_stops <- readRDS(here::here("data_processed", "rail_stops.rds"))
stop_times_full <- readRDS(here::here("data_processed", "stop_times_full.rds"))
stop_times_rail <- readRDS(here::here("data_processed", "stop_times_rail.rds"))
walk_net <- readRDS(here::here("data_processed", "walk_net.rds"))
partial_path <- here::here("data_processed", "travel_times_long_partial.rds")

# Computes walk time from each POI, policy-relevant destination, to its nearest transit stops, 
# providing the final walking segment needed for full travel time estimates.
build_destination_lookup <- function(destinations_sf, stops_sf, scenario_name) {
  compute_network_links(
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
}

message("Building destination-stop lookup...")
destination_stop_lookup <- bind_rows(
  build_destination_lookup(destinations, rail_stops, "rail_only"),
  build_destination_lookup(destinations, all_stops, "bus_rail")
)

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

save_rds(destination_stop_lookup, here::here("data_processed", "destination_stop_lookup.rds"))
save_rds(origin_links, here::here("data_processed", "origin_stop_lookup.rds"))

# Computes the fastest scheduled transit trip from each tract's walkable stops to every destination, 
# adding walk time at both the start and end of the trip for a complete travel time estimate.
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

# Saves progress every 25 tracts so interrupted runs can resume from the last checkpoint.
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
readr::write_csv(travel_times_long, here::here("outputs", "tables", "travel_times_long.csv"))


# ------------------------------------------------------------
# 06_summarize_metrics.R
# Aggregating raw travel times into tract-level metrics
# ------------------------------------------------------------

tracts_acs <- readRDS(here::here("data_processed", "tracts_acs.rds"))
travel_times_long <- readRDS(here::here("data_processed", "travel_times_long.rds"))

# Summarizing outputs by tract and scenario: mean/min/max travel time, plus cumulative counts
# of destinations reachable within 20, 30, and 45 minutes to measure accessibility.
accessibility_summary <- travel_times_long %>%
  group_by(GEOID, scenario) %>%
  summarise(
    mean_tt = mean(travel_time_min, na.rm = TRUE),
    min_tt = min(travel_time_min, na.rm = TRUE),
    max_tt = max(travel_time_min, na.rm = TRUE),
    n_20 = sum(travel_time_min <= 20, na.rm = TRUE),
    n_30 = sum(travel_time_min <= 30, na.rm = TRUE),
    n_45 = sum(travel_time_min <= 45, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_tt, min_tt, max_tt), ~ if_else(is.infinite(.x), NA_real_, .x)))

# Travel times to each individual destination, for destination-specific maps.
travel_time_wide <- travel_times_long %>%
  select(GEOID, scenario, dest_name, travel_time_min) %>%
  mutate(dest_name = stringr::str_replace_all(tolower(dest_name), "[^a-z0-9]+", "_")) %>%
  tidyr::pivot_wider(
    names_from = c(scenario, dest_name),
    values_from = travel_time_min,
    names_glue = "{scenario}_{dest_name}_tt"
  )

# Scenario-level summaries side by side and computes the rail-only vs. bus+rail differences.
summary_wide <- accessibility_summary %>%
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = c(mean_tt, min_tt, max_tt, n_20, n_30, n_45),
    names_glue = "{scenario}_{.value}"
  ) %>%
  mutate(
    diff_n_20 = bus_rail_n_20 - rail_only_n_20,
    diff_mean_tt = rail_only_mean_tt - bus_rail_mean_tt
  )

# Joins accessibility metrics back onto tract geometry and ACS demographics, 
# making outputs ready for the equity maps/plots and final report.
tracts_final <- tracts_acs %>%
  left_join(summary_wide, by = "GEOID") %>%
  left_join(travel_time_wide, by = "GEOID")

save_rds(accessibility_summary, here::here("data_processed", "accessibility_summary.rds"))
save_rds(tracts_final, here::here("data_processed", "tracts_final.rds"))
write_gpkg(tracts_final, here::here("data_processed", "tracts_final.gpkg"), "tracts_final")
readr::write_csv(accessibility_summary, here::here("outputs", "tables", "accessibility_summary.csv"))