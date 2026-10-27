source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Reads the MARTA GTFS feed and prepares weekday stop times for two scenarios:
# rail only (route_type 1) and multimodal (bus + rail, route_types 1 and 3).

gtfs_path <- find_gtfs_zip()
gtfs_full <- tidytransit::read_gtfs(gtfs_path) %>%
  tidytransit::interpolate_stop_times()

analysis_date <- choose_gtfs_analysis_date(gtfs_full, analysis_weekday)

all_routes <- gtfs_full$routes %>%
  filter(route_type %in% c(1, 3))

rail_routes <- all_routes %>%
  filter(route_type == 1)

all_trip_ids <- gtfs_full$trips %>%
  semi_join(all_routes, by = "route_id") %>%
  pull(trip_id)

rail_trip_ids <- gtfs_full$trips %>%
  semi_join(rail_routes, by = "route_id") %>%
  pull(trip_id)

gtfs_bus_rail <- tidytransit::filter_feed_by_trips(gtfs_full, all_trip_ids)
gtfs_rail <- tidytransit::filter_feed_by_trips(gtfs_full, rail_trip_ids)

all_stops <- build_stops_sf(gtfs_bus_rail$stops)
rail_stops <- build_stops_sf(gtfs_rail$stops)

stop_times_full <- tidytransit::filter_stop_times(
  gtfs_bus_rail,
  extract_date = analysis_date,
  min_departure_time = peak_start,
  max_arrival_time = peak_end
)

stop_times_rail <- tidytransit::filter_stop_times(
  gtfs_rail,
  extract_date = analysis_date,
  min_departure_time = peak_start,
  max_arrival_time = peak_end
)

save_rds(gtfs_bus_rail, here::here("data_processed", "gtfs_full.rds"))
save_rds(stop_times_full, here::here("data_processed", "stop_times_full.rds"))
save_rds(stop_times_rail, here::here("data_processed", "stop_times_rail.rds"))
save_rds(all_stops, here::here("data_processed", "all_stops.rds"))
save_rds(rail_stops, here::here("data_processed", "rail_stops.rds"))
save_rds(
  tibble(analysis_date = analysis_date, weekday = analysis_weekday, peak_start = peak_start, peak_end = peak_end),
  here::here("data_processed", "analysis_window.rds")
)
