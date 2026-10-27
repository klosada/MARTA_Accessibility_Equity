source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Converts destination coordinates to spatial points and saves for routing.

destinations <- destinations_tbl %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(study_crs)

save_rds(destinations, here::here("data_processed", "destinations.rds"))
