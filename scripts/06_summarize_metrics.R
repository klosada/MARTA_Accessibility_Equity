source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Summarizes travel times into tract-level metrics and joins ACS demographics.

# Excludes tracts with no residents (airport, golf course) from the analysis.
tracts_acs <- readRDS(here::here("data_processed", "tracts_acs.rds")) %>%
  filter(total_pop > 0)
travel_times_long <- readRDS(here::here("data_processed", "travel_times_long.rds")) %>%
  filter(GEOID %in% tracts_acs$GEOID)

# Per tract and scenario: min/median/mean/max travel time to reachable destinations,
# destinations reachable within 15-90 min, wait cost (median minus best-departure
# travel time), and transfers per trip, both averaged across destinations.
accessibility_summary <- travel_times_long %>%
  group_by(GEOID, scenario) %>%
  summarise(
    min_tt = suppressWarnings(min(travel_time_min, na.rm = TRUE)),
    median_tt = median(travel_time_min, na.rm = TRUE),
    mean_tt = mean(travel_time_min, na.rm = TRUE),
    max_tt = suppressWarnings(max(travel_time_min, na.rm = TRUE)),
    n_reachable = sum(reachable),
    wait_cost = mean(travel_time_min - best_time_min, na.rm = TRUE),
    mean_transfers = mean(transfers, na.rm = TRUE),
    n_15 = sum(travel_time_min <= 15, na.rm = TRUE),
    n_30 = sum(travel_time_min <= 30, na.rm = TRUE),
    n_45 = sum(travel_time_min <= 45, na.rm = TRUE),
    n_60 = sum(travel_time_min <= 60, na.rm = TRUE),
    n_75 = sum(travel_time_min <= 75, na.rm = TRUE),
    n_90 = sum(travel_time_min <= 90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(min_tt, median_tt, mean_tt, max_tt, wait_cost, mean_transfers), ~ if_else(is.infinite(.x) | is.nan(.x), NA_real_, .x)))

# Travel time to each destination, for destination maps.
travel_time_wide <- travel_times_long %>%
  select(GEOID, scenario, dest_name, travel_time_min) %>%
  mutate(dest_name = stringr::str_replace_all(tolower(dest_name), "[^a-z0-9]+", "_")) %>%
  tidyr::pivot_wider(
    names_from = c(scenario, dest_name),
    values_from = travel_time_min,
    names_glue = "{scenario}_{dest_name}_tt"
  )

# One row per tract, with the rail-only vs. multimodal difference at 45 min.
summary_wide <- accessibility_summary %>%
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = c(min_tt, median_tt, mean_tt, max_tt, n_reachable, wait_cost, mean_transfers, n_15, n_30, n_45, n_60, n_75, n_90),
    names_glue = "{scenario}_{.value}"
  ) %>%
  mutate(
    diff_n_45 = bus_rail_n_45 - rail_only_n_45
  )

tracts_final <- tracts_acs %>%
  left_join(summary_wide, by = "GEOID") %>%
  left_join(travel_time_wide, by = "GEOID")

save_rds(accessibility_summary, here::here("data_processed", "accessibility_summary.rds"))
save_rds(tracts_final, here::here("data_processed", "tracts_final.rds"))
readr::write_csv(accessibility_summary, here::here("outputs", "tables", "accessibility_summary.csv"))
