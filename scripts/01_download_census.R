source(here::here("scripts", "00_utils.R"))
ensure_directories()

# 2020 Census City of Atlanta (place) boundary, used to select study tracts.
city_boundary <- tigris::places(state = state_fips, year = 2020, progress_bar = FALSE) %>%
  filter(NAME == "Atlanta") %>%
  st_transform(study_crs) %>%
  st_union()
save_rds(city_boundary, here::here("data_processed", "city_boundary.rds"))

# Pulls ACS tract boundaries and equity variables for the three study counties.
tracts_acs <- tidycensus::get_acs(
  geography = "tract",
  variables = acs_vars,
  state = state_fips,
  county = study_counties,
  year = acs_year,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
) %>%
  st_transform(study_crs) %>%
  transmute(
    GEOID,
    NAME,
    hhinc = hhincE,
    pct_minority = 100 * (1 - (white_nonhispE / total_popE)),
    pct_zero_vehicle = 100 * (zero_vehicle_hhE / total_hhE),
    total_pop = total_popE,
    total_hh = total_hhE,
    geometry
  ) %>%
  # Keeps tracts whose interior point falls inside the city.
  mutate(origin_geom = st_point_on_surface(geometry)) %>%
  filter(lengths(st_intersects(origin_geom, city_boundary)) > 0) %>%
  select(-origin_geom)

# Origins: 2020 Census population-weighted tract centroids (Centers of Population,
# www2.census.gov/geo/docs/reference/cenpop2020/tract/), or an interior point
# when the centroid falls outside its tract.
cenpop_path <- list.files(dirs$raw_census, pattern = "^CenPop2020_Mean_TR13\\.txt$", full.names = TRUE)
if (!length(cenpop_path)) {
  stop("CenPop2020_Mean_TR13.txt not found in data_raw/census.")
}

cenpop <- readr::read_csv(cenpop_path[[1]], col_types = "cccddd") %>%
  rename(STATEFP = 1) %>%
  transmute(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE), LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%
  st_transform(study_crs)

tract_geoms <- st_geometry(tracts_acs)
origin_points <- st_geometry(cenpop)[match(tracts_acs$GEOID, cenpop$GEOID)]
point_inside <- diag(st_intersects(origin_points, tract_geoms, sparse = FALSE))
origin_points[!point_inside] <- st_point_on_surface(tract_geoms[!point_inside])

origins <- tracts_acs %>%
  st_set_geometry(origin_points) %>%
  select(GEOID, NAME, hhinc, pct_minority, pct_zero_vehicle)

save_rds(tracts_acs, here::here("data_processed", "tracts_acs.rds"))
save_rds(origins, here::here("data_processed", "origins.rds"))
