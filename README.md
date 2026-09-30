# MARTA Accessibility and Equity Analysis

**Author:** Katherine Losada

**Live report:** https://klosada.github.io/MARTA_Accessibility_Equity

## Overview

This project evaluates public transit accessibility and equity across the City of Atlanta using the MARTA multimodal transit network. It compares tract-level access to seven policy-relevant destinations (major job centers, universities, hospitals, and the airport) under two scenarios:

- **Rail only**
- **Full MARTA network** (bus + rail)

Accessibility is measured as the number of destinations reachable from each census tract within 20, 30, and 45 minutes during a weekday morning peak period, accounting for walk access to/from transit. The equity component relates these accessibility patterns to tract-level median household income, percent minority, and percent zero-vehicle households.

## Data Sources

| Source | Used for |
|---|---|
| Census/ACS (via `tidycensus`) | Tract boundaries and socioeconomic variables (income, minority share, zero-vehicle households) |
| MARTA GTFS feed | Transit network: rail lines, bus routes, stations, stops, and weekday schedules |
| OpenStreetMap (`.osm.pbf` extract) | Pedestrian network for walk access to/from transit |
| Manually defined destination layer | 7 policy-relevant destinations (job centers, universities, hospitals, airport) |

## Methods

1. Each census tract is represented by a single centroid as its origin point.
2. The OSM walking network is filtered to pedestrian-relevant ways and built into a routable network (`sfnetworks`).
3. Walk-accessible transit stops are identified for each origin and destination within a fixed walking distance.
4. Scheduled transit travel times are computed using RAPTOR-based routing (`tidytransit::raptor()`) for both scenarios.
5. Door-to-door travel time = walk access + transit travel time + walk egress.
6. Results are summarized into tract-level accessibility metrics and joined to ACS demographics for the equity analysis.

## Tech Stack

R packages: 
`tidyverse`, `sf`, `sfnetworks` (network routing), `tidytransit` (GTFS transit routing), `tidycensus` (Census/ACS data),`  tmap`, `leaflet`,  `plotly `(mapping and visualization).

## Project Structure

```
.
├── scripts/
│   ├── 00_config.R                 # study parameters, packages, destinations
│   ├── 00_utils.R                  # shared helper functions
│   ├── 01_download_census.R        # ACS tract data
│   ├── 02_prepare_gtfs.R           # GTFS feed prep (rail-only vs bus+rail)
│   ├── 03_prepare_walking_network.R# OSM walking network
│   ├── 04_prepare_destinations.R   # destination point layer
│   ├── 05_compute_accessibility.R  # core transit-routing engine
│   ├── 06_summarize_metrics.R      # tract-level accessibility metrics
│   ├── 07_make_maps.R              # interactive maps
│   ├── 08_make_plots.R             # equity scatterplots/boxplots
│   └── 99_run_all.R                # runs the full pipeline in order
├── report.Rmd                      # final report (renders to index.html)
├── data_raw/                       # not tracked 
├── data_processed/                 # intermediate outputs (.rds, .gpkg)
└── outputs/                        # final tables, figures, and maps
```

## Setup

This repo does not include raw data (GTFS feeds and OSM extracts). To run the pipeline yourself:

1. Set a Census API key as `CENSUS_API_KEY` and a CARTO basemap key as `CARTO_API_KEY` in a local `.Renviron` file (not tracked).
2. Download the current MARTA GTFS feed and place the `.zip` in `data_raw/gtfs/`.
3. Download a Georgia or Atlanta `.osm.pbf` extract (e.g., from Geofabrik) and place it in `data_raw/osm/`. A static extract is used here rather than live `osmdata` queries, for reproducibility and to avoid API timeouts on a long-running pipeline.
4. Add a neighborhood/study-area boundary file (`.gpkg`, `.geojson`, or `.shp`) to `data_raw/boundaries/`.
5. Run `scripts/99_run_all.R` to execute the full pipeline.
6. Knit `report.Rmd` to regenerate the final report.

## Limitations

- Each tract is represented by a single centroid, so within-tract variation in population distribution is not captured.
- The analysis reflects a single weekday morning peak period only.
- Travel times are based on scheduled GTFS service, not real-time conditions (delays, reliability, crowding).
- The destination layer is selective (7 locations) rather than a comprehensive activity-based model.
- The OpenStreetMap walking network is an approximation and walking assumptions (distance and speed) may over or understate walk access.
