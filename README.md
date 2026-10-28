# MARTA Accessibility and Equity Analysis

**Author:** Katherine Losada

**Live report:** https://klosada.github.io/MARTA_Accessibility_Equity

## Overview

This project evaluates public transit accessibility and equity across the City of Atlanta using the MARTA multimodal transit network. It compares tract-level access to ten policy-relevant destinations (major job centers, universities, hospitals, and the airport) under two scenarios:

- **Rail only**
- **Multimodal** (full MARTA network, bus + rail)

Accessibility is measured as the number of destinations reachable from each census tract within 15 to 90 minutes (in 15-minute steps) for riders leaving home between 7 and 8 AM on a weekday. Travel time is the median door-to-door trip across those departure times, including walks and waits for scheduled service. The equity component relates these accessibility patterns to tract-level median household income, percent minority, and percent zero-vehicle households.

## Data Sources

| Source | Used for |
|---|---|
| Census/ACS 2023 5-year (via `tidycensus`) | Tract boundaries and socioeconomic variables (income, minority share, zero-vehicle households) |
| 2020 Census City of Atlanta place boundary (via `tigris`) | Study area for selecting tracts |
| 2020 Census Centers of Population | Population-weighted tract centroids used as routing origins |
| MARTA GTFS feed | Transit network: rail lines, bus routes, stations, stops, and weekday schedules |
| OpenStreetMap (`.osm.pbf` extract) | Pedestrian network for walk access to/from transit |
| Google Maps (destination coordinates) | 10 policy-relevant destinations (job centers, universities, hospitals, airport) |

## Methods

1. Each census tract is represented by its 2020 Census population-weighted centroid as the origin point (a point inside the tract when it falls outside).
2. The OSM walking network is filtered to pedestrian-relevant ways, split at intersections, and kept to its largest connected piece (`sfnetworks`).
3. Walk-accessible transit stops are identified for each origin and destination within an 800 m walk (roughly 0.5 mile).
4. MARTA's feed has no transfer table, so walking transfers are built between stops within a 400 m walk (walk time, at least 2 min).
5. Scheduled trips are routed with `tidytransit::raptor()` for both scenarios, up to 3 transfers.
6. For each home departure time, every minute from 7:00 to 8:00 AM: walk to stop + wait for the next departure + transit + walk to destination, capped at 2 hours. Travel time is the median across departures.
7. Results are summarized into tract-level metrics (destinations within 15–90 min, min, median, mean, and max travel time, wait cost, and transfers) and joined to ACS demographics.
8. Equity: tracts with no transit access and tracts with rail access are compared with all other tracts (Wilcoxon tests).

## Tech Stack

R packages: 
`tidyverse`, `sf`, `sfnetworks` (network routing), `tidytransit` (GTFS transit routing), `tidycensus` and `tigris` (Census data), `leaflet`, `plotly` (mapping and visualization).

## Project Structure

```
.
├── scripts/
│   ├── 00_config.R                  # study parameters, packages, destinations
│   ├── 00_utils.R                   # shared helper functions
│   ├── 01_download_census.R         # city boundary, ACS tracts, origins
│   ├── 02_prepare_gtfs.R            # GTFS feed prep (rail-only vs bus+rail)
│   ├── 03_prepare_walking_network.R # OSM walking network
│   ├── 04_prepare_destinations.R    # destination point layer
│   ├── 05_compute_accessibility.R   # core transit-routing engine
│   ├── 06_summarize_metrics.R       # tract-level accessibility metrics
│   ├── 99_run_all.R                 # runs the full pipeline in order
│   ├── code_sample.R                # condensed excerpt of the core pipeline
│   └── watch_report.R               # dev tool: re-renders report.Rmd on save
├── report.Rmd                       # report with interactive maps and plots
├── index.html                       # rendered report (GitHub Pages)
├── data_raw/                        # not tracked
├── data_processed/                  # not tracked, intermediate outputs (.rds)
└── outputs/tables/                  # summary and travel time tables (.csv)
```

## Setup

This repo does not include raw data (GTFS feeds and OSM extracts). To run the pipeline yourself:

1. Set a Census API key as `CENSUS_API_KEY` and a CARTO basemap key as `CARTO_API_KEY` in a local `.Renviron` file (not tracked).
2. Download the current MARTA GTFS feed and place the `.zip` in `data_raw/gtfs/`.
3. Download the 2020 Census tract Centers of Population for Georgia (`CenPop2020_Mean_TR13.txt`, from www2.census.gov/geo/docs/reference/cenpop2020/tract/) and place it in `data_raw/census/`.
4. Download a Georgia or Atlanta `.osm.pbf` extract (e.g., from Geofabrik) and place it in `data_raw/osm/`. A static extract is used here rather than live `osmdata` queries, for reproducibility and to avoid API timeouts on a long-running pipeline.
5. Run `scripts/99_run_all.R` to execute the full pipeline.
6. Knit `report.Rmd` to regenerate the final report.

## Limitations

- Each tract is represented by a single population-weighted centroid, so within-tract variation in population distribution is not captured.
- The analysis covers weekday departures between 7 and 8 AM only.
- Travel times are based on scheduled GTFS service, not real-time conditions (delays, reliability, crowding).
- The destination layer is selective (10 locations) rather than a comprehensive activity-based model.
- The OpenStreetMap walking network is an approximation and walking assumptions (distance and speed) may over or understate walk access.
