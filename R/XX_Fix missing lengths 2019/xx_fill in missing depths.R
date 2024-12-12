# Load required library
library(terra)
library(dplyr)

# File paths
csv_file <- "data/raw/EM Export/2019-08_Ningaloo_stereo-BRUVs_Metadata.csv"
raster_file <- "data/spatial/rasters/depth_195_50m.tif"   # Replace with the path to your raster file

# Step 1: Read the CSV file
points <- read.csv(csv_file)

# Step 2: Load the raster
depth_raster <- rast(raster_file)

# Step 3: Check the CRS of the raster
cat("Raster CRS:", crs(depth_raster), "\n")

# Step 4: Create a spatial vector for the points (assuming WGS84 CRS for latitude/longitude)
points_sf <- vect(points, geom = c("Longitude", "Latitude"), crs = "EPSG:4326") # EPSG:4326 is the CRS for WGS84

# Step 5: Reproject points to match the raster CRS (if necessary)
if (!identical(crs(points_sf), crs(depth_raster))) {
  points_sf <- project(points_sf, crs(depth_raster))
  cat("Reprojected points to match raster CRS.\n")
}

# Step 6: Extract depth values
depths <- extract(depth_raster, points_sf)

# Step 7: Add the depth values to the original data
points$depth <- depths[, 2]  # Assuming the first column in `depths` is an ID column


missing_depths <- points %>%
  dplyr::filter(Depth %in% "?") %>%
  dplyr::select(Sample, Depth, depth)
