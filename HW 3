# ===========================================================================
# STAT 184 - Homework 3: Reproducing plots and exploring real spatial data
# Problems 1A-1C, 2, and 3
# Due: November 1, 2026

# ===========================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(jsonlite)

options(timeout = 180)

vehicles <- ggplot2::mpg       # Complete dataset, as instructed
gems     <- ggplot2::diamonds  # Complete dataset, as instructed

output_dir <- "HW3_outputs"
raw_dir    <- file.path(output_dir, "raw")
plot_dir   <- file.path(output_dir, "plots")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(name, plot, width = 8, height = 5) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), plot = plot,
         width = width, height = height, units = "in", dpi = 300,
         bg = "white")
}

# ===========================================================================
# PROBLEM 1A - Engine displacement vs. highway MPG, faceted by drive type
# ===========================================================================

p_1a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.70, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "#3366CC", linewidth = 1) +
  facet_wrap(~ drv, nrow = 1) +
  scale_color_manual(values = c("4" = "#F8766D",
                                "f" = "#00BA38",
                                "r" = "#619CFF")) +
  labs(title = "Engine size and highway fuel economy by drive type",
       x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)",
       color = "drv") +
  theme_bw()

print(p_1a)
save_plot("problem_1a", p_1a, 10, 4.4)

# EXPLANATION 1A: facet_wrap(~ drv) separates the data into three panels.
# Each geom_smooth() is computed separately on the rows in its own panel.
# Therefore there are three fitted lines even though color = drv appears only
# inside geom_point(), not in the overall ggplot mapping or smooth layer.

# ===========================================================================
# PROBLEM 1B - Stacked counts and within-drive-type proportions
# ===========================================================================

p_1b_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "stack") +
  labs(title = "Vehicle counts by drive type and cylinders",
       x = "drv", y = "count", fill = "factor(cyl)") +
  theme_bw()

p_1b_proportion <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(title = "Cylinder proportions within each drive type",
       x = "drv", y = "Proportion within drive type",
       fill = "factor(cyl)") +
  theme_bw()

print(p_1b_count)
print(p_1b_proportion)
save_plot("problem_1b_counts", p_1b_count, 6.6, 5)
save_plot("problem_1b_proportions", p_1b_proportion, 6.6, 5)

# EXPLANATION 1B: geom_bar(position = "stack") shows actual vehicle counts.
# position = "fill" divides each section by the TOTAL for its own drv group.
# Thus every bar has height 1 (100%), even when groups have different numbers
# of vehicles (103 four-wheel, 106 front-wheel, 25 rear-wheel). Equal
# proportion-bar heights do not imply equal sample sizes.

drive_sample_sizes <- vehicles %>% count(drv, name = "n_vehicles")
print(drive_sample_sizes)
write_csv(drive_sample_sizes,
          file.path(output_dir, "problem_1b_drive_sample_sizes.csv"))

# ===========================================================================
# PROBLEM 1C - Histograms using the SAME smaller diamond dataset
# ===========================================================================

# The assignment says "the same smaller table" but does not specify how to
# construct it. We choose diamonds with carat <= 2.00; this keeps the
# 0.99/1.00-carat region visible. Both histograms use this identical table.
gems_small <- gems %>% filter(!is.na(carat), carat <= 2.00)

n_retained <- nrow(gems_small)
n_excluded <- nrow(gems) - n_retained
cat("\nPROBLEM 1C: original diamonds:", nrow(gems),
    "; retained:", n_retained, "; excluded:", n_excluded, "\n")

histogram_base <- ggplot(gems_small, aes(x = carat)) +
  labs(x = "Carat", y = "Count") +
  coord_cartesian(xlim = c(0, 2.00)) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.25)) +
  theme_bw()

p_fine <- histogram_base +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right",
                 fill = "#3388BB", color = "white", linewidth = 0.12) +
  labs(title = "Diamond carats (fine bins)", subtitle = "Bin width = 0.01",
       caption = paste0("Diamonds with carat <= 2.00: ", n_retained,
                        " retained, ", n_excluded, " excluded"))

p_coarse <- histogram_base +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right",
                 fill = "#3388BB", color = "white", linewidth = 0.35) +
  labs(title = "Diamond carats (coarse bins)", subtitle = "Bin width = 0.5",
       caption = paste0("Diamonds with carat <= 2.00: ", n_retained,
                        " retained, ", n_excluded, " excluded"))

print(p_fine)
print(p_coarse)
save_plot("problem_1c_fine", p_fine, 8, 4.5)
save_plot("problem_1c_coarse", p_coarse, 8, 4.5)

# Answer using the ORIGINAL gems dataset, not gems_small.
exact_carat_counts <- tibble(carat = c(0.99, 1.00)) %>%
  left_join(gems %>% count(carat, name = "n_diamonds"), by = "carat") %>%
  mutate(n_diamonds = coalesce(n_diamonds, 0L))
print(exact_carat_counts)
write_csv(exact_carat_counts,
          file.path(output_dir, "problem_1c_exact_carat_counts.csv"))

# EXPLANATION 1C (3 sentences): Fine bins make the concentration of values at
# preferred, round carat sizes much easier to notice, such as 0.30, 0.50,
# 0.70, 1.00 and 1.50. The clearest example is just below one carat: only 23
# diamonds are recorded at 0.99 but 1,558 at 1.00. The wide 0.5-carat bins
# merge these spikes and gaps with many nearby values, hiding the pattern.

# ===========================================================================
# PROBLEMS 2 AND 3 - Chicago Public Library locations and community areas
# ---------------------------------------------------------------------------
# Official public data (NOT built-in example coordinates):
# Libraries: https://data.cityofchicago.org/d/x8fc-8rcq
# Areas:     https://data.cityofchicago.org/d/igwz-8jzy
# Data portal licensing: See Terms of Use; do not assume unrestricted licensing.
#
# Sites are taken from the official City of Chicago API, with an explicit
# 1,000-row pagination loop and an API count cross-check. Raw downloaded JSON
# and GeoJSON files are never edited. Remove the raw folder only if you
# intentionally want to refresh the data to a different download date.
# ===========================================================================

fetch_once <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading official dataset: ", url)
    tryCatch(
      utils::download.file(url, destfile = dest,
                           method = "libcurl", mode = "wb", quiet = TRUE),
      error = function(e) {
        if (file.exists(dest)) unlink(dest)
        stop("Download failed: ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    stop("Missing or empty downloaded dataset: ", dest)
  }
  dest
}

# ---- Obtain all library rows, with pagination and unmodified raw downloads.
library_base <- "https://data.cityofchicago.org/resource/x8fc-8rcq.json"
page_size <- 1000L
all_library_records <- list()
offset <- 0L
first_raw_page <- NULL

repeat {
  # $order makes paging stable if the dataset ever exceeds one page
  page_url <- paste0(library_base, "?$limit=", page_size,
                     "&$offset=", offset, "&$order=:id")
  page_file <- file.path(raw_dir,
                         sprintf("chicago_libraries_offset_%06d.json", offset))
  fetch_once(page_url, page_file)
  if (is.null(first_raw_page)) first_raw_page <- page_file
  
  page_records <- jsonlite::fromJSON(page_file, simplifyVector = FALSE)
  if (!is.list(page_records)) stop("Unexpected library API response format.")
  all_library_records <- c(all_library_records, page_records)
  
  if (length(page_records) < page_size) break
  offset <- offset + page_size
  if (offset > 1000000L) stop("Unexpectedly many pages: inspect API download.")
}

# This explicit check prevents a silently incomplete one-page API download.
count_url  <- paste0(library_base, "?$select=count(*)")
count_file <- file.path(raw_dir, "chicago_libraries_api_count.json")
fetch_once(count_url, count_file)
count_response <- jsonlite::fromJSON(count_file, simplifyVector = FALSE)
api_total <- as.integer(count_response[[1]][["count"]])
if (is.na(api_total) || length(all_library_records) != api_total) {
  stop("Library count mismatch. To refresh, remove the cached library JSON ",
       "files in HW3_outputs/raw/ and rerun.")
}
cat("\nPROBLEM 2: Official Chicago library records downloaded:", api_total, "\n")

# Utility: get the first nonblank library name among supported field names.
get_name <- function(rec) {
  for (field in c("name", "name_", "branch", "branch_")) {
    value <- rec[[field]]
    if (!is.null(value) && length(value) == 1L &&
        !is.na(value) && nzchar(trimws(as.character(value)))) {
      return(as.character(value))
    }
  }
  NA_character_
}

# Utility: Socrata LOCATION objects have latitude/longitude fields.
# Some API exports use GeoJSON-like coordinates [longitude, latitude].
get_coordinate <- function(rec, component) {
  loc <- rec[["location"]]
  if (is.list(loc) && !is.null(loc[[component]])) {
    return(suppressWarnings(as.numeric(loc[[component]])))
  }
  if (is.list(loc) && !is.null(loc[["coordinates"]]) &&
      length(loc[["coordinates"]]) >= 2L) {
    idx <- if (component == "longitude") 1L else 2L
    return(suppressWarnings(as.numeric(loc[["coordinates"]][[idx]])))
  }
  if (!is.null(rec[[component]])) {
    return(suppressWarnings(as.numeric(rec[[component]])))
  }
  NA_real_
}

# Create one row per source record, BEFORE cleaning; do not deduplicate points.
locations_audit <- tibble(
  original_row = seq_along(all_library_records),
  facility_name = vapply(all_library_records, get_name, character(1)),
  latitude = vapply(all_library_records,
                    get_coordinate, numeric(1), component = "latitude"),
  longitude = vapply(all_library_records,
                     get_coordinate, numeric(1), component = "longitude")
) %>%
  mutate(
    removal_reason = case_when(
      is.na(facility_name) | !nzchar(trimws(facility_name)) ~
        "Missing facility name",
      !is.finite(latitude) | !is.finite(longitude) ~
        "Missing or invalid coordinates",
      latitude < -90 | latitude > 90 | longitude < -180 | longitude > 180 ~
        "Coordinates outside valid latitude/longitude ranges",
      # Broad Chicago-area bounding box documents the chosen study area.
      latitude < 41.55 | latitude > 42.10 |
        longitude < -88.05 | longitude > -87.45 ~
        "Outside Chicago-area study bounding box",
      TRUE ~ "Retained"
    )
  )

cleaned_locations <- locations_audit %>%
  filter(removal_reason == "Retained") %>%
  transmute(site_id = sprintf("LIB_%04d", original_row),
            facility_name, longitude, latitude, original_row)

# Sites with identical coordinates are still separate locations/records.
n_repeated_coords <- sum(duplicated(cleaned_locations[c("longitude", "latitude")]))
n_repeated_name_coords <- sum(duplicated(
  cleaned_locations[c("facility_name", "longitude", "latitude")]))

cleaning_summary <- locations_audit %>%
  count(removal_reason, name = "n_records")
print(cleaning_summary)
cat("Original rows:", nrow(locations_audit),
    "\nRetained:", nrow(cleaned_locations),
    "\nExcluded:", nrow(locations_audit) - nrow(cleaned_locations),
    "\nRepeated coordinate pairs (extra rows retained):", n_repeated_coords,
    "\nRepeated name-and-coordinate pairs (extra rows retained):",
    n_repeated_name_coords, "\n")

write_csv(locations_audit,   file.path(output_dir, "cleaning_audit.csv"))
write_csv(cleaning_summary,  file.path(output_dir, "cleaning_summary.csv"))
write_csv(cleaned_locations, file.path(output_dir, "cleaned_locations.csv"))

stopifnot(nrow(cleaned_locations) >= 20L,
          anyDuplicated(cleaned_locations$site_id) == 0L)

# NAD83 / UTM zone 16N (EPSG:26916) uses metres in Chicago.
# Transform BEFORE computing buffers and area, never buffer in lon/lat degrees.
sites_m <- st_as_sf(cleaned_locations,
                    coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE) %>%
  st_transform(26916)
cat("Projected site CRS:", st_crs(sites_m)$input,
    "| Units:", st_crs(sites_m)$units_gdal, "\n")
stopifnot(st_crs(sites_m)$epsg == 26916)
st_write(sites_m, file.path(output_dir, "sites_m.gpkg"),
         layer = "sites_m", delete_dsn = TRUE, quiet = TRUE)
saveRDS(sites_m, file.path(output_dir, "sites_m.rds"))

# Source metadata: source pages, exact download URLs, retrieval dates, format.
area_url <- paste0(
  "https://data.cityofchicago.org/api/geospatial/igwz-8jzy",
  "?method=export&format=GeoJSON"
)
area_raw_file <- file.path(raw_dir, "chicago_community_areas.geojson")
fetch_once(area_url, area_raw_file)
site_access_date <- as.character(as.Date(file.info(first_raw_page)$mtime))
area_access_date <- as.character(as.Date(file.info(area_raw_file)$mtime))

data_sources <- tibble(
  data_type = c("Point facilities", "Area polygons"),
  dataset_name = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
                   "Boundaries - Community Areas"),
  publisher = c("City of Chicago / Chicago Public Library", "City of Chicago"),
  source_page = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                  "https://data.cityofchicago.org/d/igwz-8jzy"),
  download_url = c(library_base, area_url),
  retrieved_on = c(site_access_date, area_access_date),
  raw_format = c("JSON (all pages)", "GeoJSON"),
  access_and_license = c("Public portal; see City of Chicago Terms of Use",
                         "Public portal; see City of Chicago Terms of Use")
)
write_csv(data_sources, file.path(output_dir, "data_sources.csv"))

# ---- PROBLEM 2: Maps of sites and distance buffers ------------------------
source_caption <- paste0(
  "Sources: Chicago Data Portal, public libraries (downloaded ",
  site_access_date, "); community-area boundaries (downloaded ",
  area_access_date, ")."
)

# Context layer only (city outline); buffers below are NOT clipped to it.
city_outline <- st_read(area_raw_file, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(26916) %>%
  st_union() %>%
  st_sf(layer = "City of Chicago", geometry = .)

p_sites <- ggplot() +
  geom_sf(data = city_outline, aes(fill = layer),
          color = "grey50", linewidth = 0.25) +
  geom_sf(data = sites_m, aes(color = "Public library"), size = 1.7,
          alpha = 0.85) +
  scale_fill_manual(values = c("City of Chicago" = "grey96"), name = "Boundary") +
  scale_color_manual(values = c("Public library" = "#087E8B"),
                     name = "Facilities") +
  coord_sf(datum = NA) +
  labs(title = "Chicago Public Library locations",
       subtitle = paste(nrow(sites_m), "retained facilities"),
       caption = source_caption) +
  theme_bw() +
  theme(plot.caption = element_text(size = 8, hjust = 0))

# Buffer each site, then union ALL buffers. Overlapping portions count ONCE.
# Do not clip to city limits: assignment requires outside-boundary areas.
buffer_500 <- st_sf(radius_m = 500,
                    geometry = st_union(st_buffer(sites_m, dist = 500)))
buffer_1000 <- st_sf(radius_m = 1000,
                     geometry = st_union(st_buffer(sites_m, dist = 1000)))

buffer_summary <- tibble(
  radius_m = c(500L, 1000L),
  union_area_km2 = c(as.numeric(st_area(buffer_500)) / 1000000,
                     as.numeric(st_area(buffer_1000)) / 1000000)
)
stopifnot(buffer_summary$union_area_km2[[2]] >=
            buffer_summary$union_area_km2[[1]])
print(buffer_summary)
write_csv(buffer_summary, file.path(output_dir, "buffer_summary.csv"))

st_write(buffer_500, file.path(output_dir, "buffer_500m.gpkg"),
         layer = "buffer_500m", delete_dsn = TRUE, quiet = TRUE)
st_write(buffer_1000, file.path(output_dir, "buffer_1000m.gpkg"),
         layer = "buffer_1000m", delete_dsn = TRUE, quiet = TRUE)
saveRDS(buffer_500, file.path(output_dir, "buffer_500m.rds"))
saveRDS(buffer_1000, file.path(output_dir, "buffer_1000m.rds"))

p_buffers <- ggplot() +
  geom_sf(data = city_outline, aes(fill = layer),
          color = "grey50", linewidth = 0.25) +
  geom_sf(data = buffer_500, aes(fill = "Within 500 m of a library"),
          color = "#235D95", linewidth = 0.15, alpha = 0.55) +
  geom_sf(data = sites_m, aes(color = "Public library"), size = 0.9) +
  scale_fill_manual(values = c("City of Chicago" = "grey96",
                               "Within 500 m of a library" = "#63A7D6"),
                    name = "Areas") +
  scale_color_manual(values = c("Public library" = "#B31B1B"),
                     name = "Facilities") +
  coord_sf(datum = NA) +
  labs(title = "Combined 500 m library buffers in Chicago",
       subtitle = sprintf("Union area = %.1f km2; overlaps counted once; not clipped to the city",
                          buffer_summary$union_area_km2[[1]]),
       caption = source_caption) +
  theme_bw() +
  theme(plot.caption = element_text(size = 8, hjust = 0))

print(p_sites)
print(p_buffers)
save_plot("problem_2_sites", p_sites, 9, 7)
save_plot("problem_2_buffers_500m", p_buffers, 9, 7)

cat(sprintf("\nPROBLEM 2 INTERPRETATION: Combined 500 m area: %.3f km2; ",
            buffer_summary$union_area_km2[[1]]))
cat(sprintf("combined 1000 m area: %.3f km2.\n",
            buffer_summary$union_area_km2[[2]]))
cat("These are distances from libraries, NOT shares of residents served.\n")

# ===========================================================================
# PROBLEM 3 - Count libraries by community area and compare area densities
# ===========================================================================

# Load a SEPARATE real boundary dataset; convert the geometry to the same CRS.
districts_raw <- st_read(area_raw_file, quiet = TRUE)
n_boundary_raw <- nrow(districts_raw)
n_empty_raw    <- sum(st_is_empty(districts_raw))
n_invalid_raw  <- sum(!st_is_valid(districts_raw))
names(districts_raw) <- tolower(names(districts_raw))

if (!all(c("area_numbe", "community") %in% names(districts_raw))) {
  stop("Community-area dataset schema changed; inspect names(districts_raw).")
}

# Repairs are documented; only genuinely empty/non-polygon records are removed.
districts_fixed <- st_make_valid(districts_raw)
if (any(!st_geometry_type(districts_fixed) %in% c("POLYGON", "MULTIPOLYGON"))) {
  districts_fixed <- st_collection_extract(districts_fixed, "POLYGON",
                                           warn = FALSE)
}
districts_fixed <- districts_fixed[!st_is_empty(districts_fixed), ]
districts_fixed <- st_transform(districts_fixed, st_crs(sites_m))

districts_m <- districts_fixed %>%
  mutate(district_id = as.character(area_numbe),
         district_name = as.character(community)) %>%
  select(district_id, district_name) %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1000000)

if (anyNA(districts_m$district_id) ||
    any(!nzchar(trimws(districts_m$district_id))) ||
    anyDuplicated(districts_m$district_id) != 0L) {
  stop("Boundary IDs are missing or duplicated; investigate boundary source.")
}
stopifnot(nrow(districts_m) >= 5L,
          all(st_is_valid(districts_m)),
          all(districts_m$area_km2 > 0),
          st_crs(districts_m) == st_crs(sites_m))

cat("\nPROBLEM 3 BOUNDARY AUDIT:\n",
    "Source polygons:", n_boundary_raw,
    "\nEmpty before repair:", n_empty_raw,
    "\nInvalid before repair:", n_invalid_raw,
    "\nAfter validation / empty removal:", nrow(districts_m), "\n")

boundary_audit <- tibble(
  raw_polygons = n_boundary_raw,
  invalid_before_repair = n_invalid_raw,
  empty_before_repair = n_empty_raw,
  retained_polygons = nrow(districts_m),
  removed_polygons = n_boundary_raw - nrow(districts_m)
)
write_csv(boundary_audit, file.path(output_dir, "boundary_audit.csv"))
st_write(districts_m, file.path(output_dir, "districts_m.gpkg"),
         layer = "districts_m", delete_dsn = TRUE, quiet = TRUE)
saveRDS(districts_m, file.path(output_dir, "districts_m.rds"))

# st_intersects returns ALL polygon matches for each point. We assign a point
# only when it intersects EXACTLY ONE district. Zero/multiple are unassigned.
matches <- st_intersects(sites_m, districts_m)
match_n <- lengths(matches)
assigned_id <- vapply(seq_along(matches), function(i) {
  if (length(matches[[i]]) == 1L) {
    districts_m$district_id[matches[[i]][[1]]]
  } else {
    NA_character_
  }
}, character(1))

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_area_matches = match_n,
  assigned_district_id = assigned_id,
  match_status = case_when(
    match_n == 0 ~ "No area match",
    match_n == 1 ~ "Exactly one area (counted)",
    TRUE ~ "Multiple areas (not counted)"
  )
)
write_csv(site_match_audit, file.path(output_dir, "site_match_audit.csv"))
print(site_match_audit %>% count(match_status, name = "n_sites"))
print(site_match_audit %>% filter(n_area_matches != 1))

n_unassigned <- sum(match_n != 1L)
n_assigned   <- sum(match_n == 1L)

counts_by_id <- site_match_audit %>%
  filter(n_area_matches == 1L) %>%
  count(assigned_district_id, name = "n_sites")

district_counts <- districts_m %>%
  st_drop_geometry() %>%
  left_join(counts_by_id,
            by = c("district_id" = "assigned_district_id")) %>%
  mutate(n_sites = coalesce(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

# Keep EVERY community area, including zero-count areas.
stopifnot(nrow(district_counts) == nrow(districts_m),
          all(!is.na(district_counts$n_sites)),
          sum(district_counts$n_sites) == n_assigned)

write_csv(district_counts, file.path(output_dir, "district_counts.csv"))
cat("\nPROBLEM 3 district_counts (all areas, including zeros):\n")
print(as_tibble(district_counts), n = Inf)
cat("Assigned sites:", n_assigned, "| Unassigned sites:", n_unassigned,
    "| Total:", nrow(sites_m), "\n")

map_districts <- districts_m %>%
  left_join(district_counts %>%
              select(district_id, n_sites, sites_per_km2),
            by = "district_id")

# EXACT same boundary data, CRS, map extent and aspect ratio for both maps.
map_bbox <- st_bbox(districts_m)
map_caption <- paste0(source_caption, " Unassigned sites: ", n_unassigned,
                      ". White with dark outline = zero libraries; ",
                      "grey = missing data.")

# Zero-count areas get their own legend key (white fill, dark outline) so
# they are clearly different from low positive values and from missing (NA).
make_map <- function(var, low, high, legend_name, breaks_fun, label_fun, title) {
  d_pos  <- map_districts %>%
    filter(is.na(.data[[var]]) | .data[[var]] > 0)
  d_zero <- map_districts %>%
    filter(!is.na(.data[[var]]) & .data[[var]] == 0)
  ggplot() +
    geom_sf(data = d_pos, aes(fill = .data[[var]]),
            color = "grey55", linewidth = 0.18) +
    geom_sf(data = d_zero, aes(color = "0 libraries (zero)"),
            fill = "white", linewidth = 0.3) +
    scale_fill_gradient(low = low, high = high, na.value = "grey65",
                        name = legend_name, breaks = breaks_fun,
                        labels = label_fun) +
    scale_color_manual(values = c("0 libraries (zero)" = "grey25"),
                       name = NULL) +
    guides(fill = guide_colorbar(order = 1),
           color = guide_legend(order = 2,
                                override.aes = list(fill = "white"))) +
    coord_sf(xlim = c(map_bbox[["xmin"]], map_bbox[["xmax"]]),
             ylim = c(map_bbox[["ymin"]], map_bbox[["ymax"]]),
             expand = FALSE, datum = NA) +
    labs(title = title,
         subtitle = paste(n_assigned, "assigned;", n_unassigned, "unassigned"),
         caption = map_caption) +
    theme_bw() +
    theme(plot.caption = element_text(size = 7.5, hjust = 0))
}

p_count <- make_map(
  "n_sites", "#C6DBEF", "#08519C", "Libraries\n(count)",
  breaks_fun = function(lim) seq(ceiling(lim[1]), floor(lim[2])),
  label_fun = function(x) format(x, trim = TRUE),
  title = "Chicago libraries by community area: total count")

p_density <- make_map(
  "sites_per_km2", "#FCBBA1", "#99000D", "Libraries\nper km\u00B2",
  breaks_fun = ggplot2::waiver(),
  label_fun = function(x) sprintf("%.2f", x),
  title = "Chicago libraries by community area: density per km\u00B2")

# Optional context map: sites over the same neighborhood polygons.
p_district_sites <- ggplot() +
  geom_sf(data = districts_m, fill = "grey97", color = "grey70",
          linewidth = 0.2) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.15) +
  scale_color_manual(values = c("Library" = "#B22222"), name = "Sites") +
  coord_sf(datum = NA) +
  labs(title = "Libraries over Chicago community-area boundaries",
       caption = map_caption) +
  theme_bw() +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))

print(p_district_sites)
print(p_count)
print(p_density)
save_plot("problem_3_sites_over_areas", p_district_sites, 9, 7)
save_plot("problem_3_facility_count", p_count, 9, 7)
save_plot("problem_3_facility_density", p_density, 9, 7)

# ---- Interpretation (handles ties) -----------------------------------------
max_n <- max(district_counts$n_sites)
tied_count <- district_counts %>% filter(n_sites == max_n)
highest_density <- district_counts %>%
  arrange(desc(sites_per_km2), district_name) %>% slice(1)
n_zero <- sum(district_counts$n_sites == 0)

interpretation <- c(
  "HOMEWORK 3 - RESULTS AND INTERPRETATIONS",
  "",
  "Problem 1A: Faceting by drv fits a separate regression within each panel.",
  "Problem 1B: Equal 100% stacked heights do not mean equal counts; each drv bar is normalized within that group.",
  sprintf("Problem 1C: Selected diamonds with carat <= 2.00: %d retained, %d excluded.",
          n_retained, n_excluded),
  "Fine 0.01-carat bins reveal sharp spikes around specific reported carat values that wider bins conceal.",
  "",
  sprintf("Problem 2: %d site records downloaded; %d retained after cleaning.",
          nrow(locations_audit), nrow(sites_m)),
  sprintf("Problem 2: %d extra identical-coordinate rows retained, not deduplicated.",
          n_repeated_coords),
  "Projected CRS: EPSG:26916, NAD83/UTM zone 16N, linear units: metres.",
  sprintf("Combined 500 m buffer: %.3f km2; combined 1000 m buffer: %.3f km2.",
          buffer_summary$union_area_km2[[1]],
          buffer_summary$union_area_km2[[2]]),
  "Overlapping buffers are counted once and are NOT clipped at the city boundary.",
  "These measures show planar proximity, not the share of residents with access.",
  "",
  sprintf("Problem 3: %d community areas; %d sites assigned to exactly one; %d unassigned.",
          nrow(districts_m), n_assigned, n_unassigned),
  sprintf("Most libraries by count: %d each in %s.",
          max_n, paste(tied_count$district_name, collapse = ", ")),
  sprintf("Highest area density: %s (%.3f libraries per km2).",
          highest_density$district_name, highest_density$sites_per_km2),
  sprintf("%d of %d community areas have zero libraries.",
          n_zero, nrow(district_counts)),
  "Area density divides each community area's library count by its polygon area.",
  "Rankings change when area is considered: small areas can have high density despite modest counts, and large areas can have several libraries but low density.",
  "Community-area density is not population-based library availability.",
  sprintf("Sources downloaded: libraries %s; community-area boundaries %s.",
          site_access_date, area_access_date),
  "Chicago portal terms of use apply; publication dates are not proof of current operating status."
)
writeLines(interpretation, file.path(output_dir, "results_and_interpretations.txt"))
cat("\n", paste(interpretation, collapse = "\n"), "\n")

cat("\nDONE! Saved figures in: ", plot_dir,
    "\nSaved CSVs, original raw data, and GIS objects in: ", output_dir,
    "\nRemember to provide your OWN GitHub repo link when submitting.\n",
    sep = "")
