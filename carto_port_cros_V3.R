#' --- title: "Coastal Map of the Hyères Islands (France) with Inset Map"
#' --- author: "Etienne Boncourt"
#' --- date: "2026-01-06"
#' --- output: "A ggplot2 map visualizing the coastline of the Hyères Islands (France) with points of interest and an inset map of France for context."
#'
#' This script downloads OpenStreetMap coastline data for the Hyères Islands region,
#' plots points of interest (islands and coastal landmarks), and adds a reference colony.
#' It also includes an inset map of France to provide geographical context.
#' Dependencies: sf, ggplot2, ggspatial, osmdata, rnaturalearth, cowplot

# Load required libraries
library(sf)
library(ggplot2)
library(ggspatial)
library(osmdata)
library(rnaturalearth)
library(cowplot)

# Download coastline data for the Hyères Islands region
# Bounding box: lon = [5.5, 7.5], lat = [42.5, 43.5]
coast_data <- opq(bbox = c(left = 5.5, bottom = 42.5, right = 7.5, top = 43.5)) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# Extract coastline lines
coastline <- coast_data$osm_lines

# Create a spatial object for the bounding box (for cropping)
france_bbox <- st_as_sfc(st_bbox(c(xmin = 5.5, xmax = 7.5, ymin = 42.5, ymax = 43.5), crs = 4326))
france_bbox <- st_as_sf(france_bbox)

# Crop, union, and cast coastline to polygons
coastline <- st_crop(coastline, france_bbox)
coastline <- st_union(coastline)
coastline <- st_cast(coastline, "POLYGON")

# Define points of interest (islands and coastal landmarks)
points_of_interest <- data.frame(
  name = c("Port-Cros Island", "Porquerolles Island", "Levant Island", "Cap Bénat"),
  lon = c(6.40, 6.20, 6.46, 6.35),
  lat = c(43.01, 43.00, 43.025, 43.10)
)

# Define reference colony (e.g., a study site)
reference_colony <- data.frame(
  name = c("Reference colony"),
  lon = c(6.382),
  lat = c(43.0115)
)

# Convert data frames to spatial objects
points_sf <- st_as_sf(points_of_interest, coords = c("lon", "lat"), crs = 4326)
reference_sf <- st_as_sf(reference_colony, coords = c("lon", "lat"), crs = 4326)

# Create the main map
main_map <- ggplot() +
  # Add coastline polygon
  geom_sf(data = coastline, fill = "white", color = "black") +
  # Add reference colony (red point)
  geom_sf(data = reference_sf, color = "red", size = 2) +
  # Label points of interest
  geom_sf_text(data = points_sf, aes(label = name), vjust = 1.5, size = 3, color = "black") +
  # Label reference colony
  geom_sf_text(data = reference_sf, aes(label = name), vjust = 0, hjust = 1, size = 3, color = "black") +
  # Set map coordinates and limits
  coord_sf(xlim = c(6.14, 6.52), ylim = c(42.95, 43.13), expand = FALSE) +
  # Add axis labels
  labs(x = "Longitude", y = "Latitude") +
  # Customize map appearance
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA)
  )

# Add a north arrow
main_map <- main_map +
  annotation_north_arrow(
    location = "topleft",
    which_north = TRUE,
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  )

# Add a manual scale bar (30 km)
main_map <- main_map +
  annotate("segment", x = 6.1, xend = 6.4, y = 42.95, yend = 42.95, colour = "black", size = 1) +
  annotate("text", x = 6.25, y = 42.93, label = "30 km", size = 3)

# Print the main map
print(main_map)

# --- Inset Map: France ---
# Download France's borders for the inset map
france <- ne_countries(scale = 110, country = "france", returnclass = "sf")

# Define the bounding box of the main map (for the inset)
main_bbox <- st_bbox(c(xmin = 6.14, xmax = 6.55, ymin = 42.95, ymax = 43.13), crs = 4326)
main_bbox_sf <- st_as_sfc(main_bbox)

# Create the inset map
france_map <- ggplot() +
  geom_sf(data = france) +
  geom_sf(data = main_bbox_sf, fill = "red", color = "red", alpha = 0.3, size = 0.5) +
  coord_sf(xlim = c(-5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"))

# Combine main map and inset map using cowplot
final_plot <- ggplot2::ggplot()
combined_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(ggplot_gtable(ggplot_build(main_map))) +
  cowplot::draw_plot(ggplot_gtable(ggplot_build(france_map)), x = 0.7, y = 0.70, width = 0.3, height = 0.25)

# Print the final combined plot
print(combined_plot)
