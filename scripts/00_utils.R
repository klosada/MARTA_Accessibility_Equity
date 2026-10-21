source(here::here("scripts", "00_config.R"))

# Creates output directories if they don't exist yet.
ensure_directories <- function() {
  walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

# Saves an R object to an RDS file.
save_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path)
  invisible(path)
}

# Saves an sf object to a GeoPackage layer (for external GIS use).
write_gpkg <- function(x, path, layer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(sf::st_write(x, path, layer = layer, delete_layer = TRUE, quiet = TRUE))
  invisible(path)
}

# Converts seconds to minutes.
seconds_to_minutes <- function(x) {
  x / 60
}

# Converts weekdays to numeric index for accurate chronological order in analysis and visuals.
weekday_index <- function(x) {
  c(
    monday = 1L, tuesday = 2L, wednesday = 3L, thursday = 4L,
    friday = 5L, saturday = 6L, sunday = 7L
  )[[tolower(x)]]
}

# Selects a representative weekday from the GTFS feed with no service exceptions.
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

# Locates the MARTA GTFS zip file in the raw data directory.
find_gtfs_zip <- function() {
  gtfs_zips <- list.files(dirs$raw_gtfs, pattern = "\\.zip$", full.names = TRUE)
  if (!length(gtfs_zips)) {
    stop("Place the MARTA GTFS .zip file in data_raw/gtfs before running the GTFS script.")
  }
  gtfs_zips[[1]]
}

# Converts a GTFS stops table to an sf point object in the study CRS.
build_stops_sf <- function(stops_tbl, crs = study_crs) {
  stops_tbl %>%
    filter(!is.na(stop_lon), !is.na(stop_lat)) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
    st_transform(crs)
}

# Calculates walking distance between spatial features (census tract centroids
# to nearby transit stops) using the OpenStreetMap pedestrian network and a
# shortest-path algorithm, retaining only connections within the 800-meter
# maximum walkable distance threshold.
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

# Adds a quartile column to a data frame. Used to group ACS data into quartiles for equity analysis.
add_quartile <- function(x, n = 4) {
  dplyr::ntile(x, n)
}
