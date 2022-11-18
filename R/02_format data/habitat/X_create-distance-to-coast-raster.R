library(terra)
library(sf)

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%
  dplyr::filter(FEAT_CODE %in% "mainland")

st_crs(aus) <- gdacrs
aus <- st_transform(aus, wgscrs)
aus <- st_union(aus)                                                            # Joins the states together
ausout <- st_cast(aus, "MULTILINESTRING")                                       # 2 different polygon forms - this one not filled
plot(ausout)
ausv <- vect(ausout)
plot(ausv)

# Load bathy raster to calculate distance
bathy <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif")
crop(bathy, ext())
plot(bathy)

distcoast <- terra::distance(bathy, ausv)
