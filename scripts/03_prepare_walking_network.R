source(here::here("scripts", "00_utils.R"))
ensure_directories()

# Builds a routable walking network from a local OpenStreetMap extract.

tracts_acs <- readRDS(here::here("data_processed", "tracts_acs.rds"))
osm_pbf <- list.files(dirs$raw_osm, pattern = "\\.osm\\.pbf$", full.names = TRUE)

if (!length(osm_pbf)) {
  stop("No Georgia or Atlanta .osm.pbf extract in data_raw/osm.")
}

# Study tracts plus a 3 km buffer.
study_area <- tracts_acs %>%
  st_union() %>%
  st_buffer(3000)

study_bbox <- study_area %>%
  st_transform(4326) %>%
  st_bbox()

# Extracts OSM lines with GDAL, which reads large extracts in full
# (st_read on a statewide .pbf stops early).
message("Extracting OSM lines for the study area...")
osm_lines_gpkg <- tempfile(fileext = ".gpkg")
sf::gdal_utils(
  "vectortranslate",
  source = osm_pbf[[1]],
  destination = osm_lines_gpkg,
  options = c(
    "-f", "GPKG", "-lco", "SPATIAL_INDEX=NO",
    "-spat", study_bbox[["xmin"]], study_bbox[["ymin"]], study_bbox[["xmax"]], study_bbox[["ymax"]],
    "-where", "highway IS NOT NULL",
    "lines"
  ),
  quiet = TRUE
)

walk_lines <- st_read(osm_lines_gpkg, quiet = TRUE, stringsAsFactors = FALSE) %>%
  # Walkable ways only (no motorways). Steps and corridors connect station platforms to the street.
  filter(highway %in% c(
    "footway", "path", "pedestrian", "living_street", "residential",
    "service", "unclassified", "tertiary", "secondary", "primary",
    "steps", "corridor", "cycleway", "track"
  )) %>%
  st_transform(study_crs) %>%
  st_intersection(study_area) %>%
  st_collection_extract("LINESTRING") %>%
  st_cast("MULTILINESTRING") %>%
  st_cast("LINESTRING", warn = FALSE) %>%
  select(osm_id, name, highway)
st_geometry(walk_lines) <- "geometry"

message("Building sfnetwork...")
walk_net <- walk_lines %>%
  sfnetworks::as_sfnetwork(directed = FALSE) %>%
  # Splits streets where they cross so intersections connect.
  tidygraph::convert(sfnetworks::to_spatial_subdivision, .clean = TRUE) %>%
  # Keeps the largest connected network so no stop snaps to an isolated fragment.
  activate(nodes) %>%
  filter(tidygraph::group_components() == 1) %>%
  activate(edges) %>%
  mutate(weight = as.numeric(st_length(geometry)))
save_rds(walk_net, here::here("data_processed", "walk_net.rds"))
