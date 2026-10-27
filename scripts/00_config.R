options(tigris_use_cache = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(tidycensus)
  library(tigris)
  library(tidytransit)
  library(sfnetworks)
  library(tidygraph)
  library(here)
  library(glue)
})
# Study area: Clayton, DeKalb, and Fulton counties, GA.
study_crs <- 5070
state_fips <- "13"
study_counties <- c("063", "089", "121")

# Walking speed and maximum distance assumptions for transit access.
walk_speed_m_per_min <- 80
max_walk_meters <- 800  # roughly 0.5 mile
max_transfer_walk_meters <- 400  # roughly 5 minutes
min_transfer_seconds <- 120

# Weekday peak morning commute for a representative transit service period.
analysis_weekday <- "wednesday"
peak_start <- "07:00:00"
peak_end <- "10:00:00"
max_transfers <- 3L

# Riders leave home every minute from 7:00 to 8:00; trips must finish within
# 2 hours, so the 7–10 AM schedule covers every departure.
departure_start <- "07:00:00"
departure_end <- "08:00:00"
max_trip_minutes <- 120

# ACS demographic variables for transit equity analysis.
acs_year <- 2023
acs_vars <- c(
  hhinc = "B19013_001",
  total_pop = "B03002_001",
  white_nonhisp = "B03002_003",
  total_hh = "B08201_001",
  zero_vehicle_hh = "B08201_002"
)

# Policy-relevant destinations representing key access needs across Atlanta:
# major job centers, universities, hospitals, and the airport.
destinations_tbl <- tibble::tribble(
  ~dest_id, ~dest_name, ~dest_type, ~lon, ~lat,
  "midtown", "Midtown", "job_center", -84.3863, 33.7812,
  "downtown", "Downtown", "job_center", -84.3876, 33.7581,
  "buckhead", "Buckhead", "job_center", -84.3677, 33.8479,
  "gatech", "Georgia Tech", "education", -84.3943, 33.7714,
  "gsu", "Georgia State University", "education", -84.3862, 33.7540,
  "auc", "Atlanta University Center", "education", -84.4108, 33.7493,
  "emory", "Emory", "education", -84.3255, 33.7897,
  "grady", "Grady Memorial Hospital", "hospital", -84.3811, 33.7523,
  "piedmont", "Piedmont Atlanta Hospital", "hospital", -84.3942, 33.8070,
  "atl", "Hartsfield-Jackson Atlanta International Airport", "airport", -84.4462, 33.6406
)

# Defining data file paths for consistency and reuse.
dirs <- list(
  raw_census = here::here("data_raw", "census"),
  raw_gtfs = here::here("data_raw", "gtfs"),
  raw_osm = here::here("data_raw", "osm"),
  processed = here::here("data_processed"),
  tables = here::here("outputs", "tables")
)
