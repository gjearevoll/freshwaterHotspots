
#### DISTANCE TO WATER CALCULATION ####

# To discern whether or not an observation is relevant, we need to know how close it is to 
# a water body. That's what we're figuring out here, using geonorge and elvenett's network
# maps of freshwater in Norway.

.libPaths(c("/cluster/projects/nn11017k/R"))
sapply(list.files("functions", full.names = TRUE), source)
library(dplyr)
library(sf)
library(terra)

regionGeometry <- readRDS("data/regionGeometry.RDS")

# See if we have pre-calculated this before
if (file.exists("data/external/waterDistance/distanceRasterRivers.tiff")) {
  baseWaterRivers <- rast("data/external/waterDistance/distanceRasterRivers.tiff")
  baseWaterLakes <- rast("data/external/waterDistance/distanceRasterLakes.tiff")
  
  # If not, start from scratch
} else {
  vectorBase <- get_geonorge(dataName = "N250Kartdata", targetDir = "data/external/waterDistance", dataFormat = "FGDB")
  lakeBoundary <- st_read("data/external/riverNetwork/Innsjo/Innsjo_Innsjo.shp")
  lakeBoundaryVect <- vect(lakeBoundary) |>
    project("EPSG:25833")
 
  # Get a base raster with expanded buffer (as closest road or lake may be over county lines)
  baseWaterRivers <- terra::rast(extent = ext(regionGeometry), res = 500, crs = "EPSG:25833")
  baseWaterRivers$cellId <- paste0("cell", 1:ncell(baseWaterRivers))
  
  baseWaterLakes <- terra::rast(extent = ext(regionGeometry), res = 500, crs = "EPSG:25833")
  baseWaterLakes$cellId <- paste0("cell", 1:ncell(baseWaterLakes))
  
  # Extract vectors to a raster layer and figure out which cells have a road/water body in them
  # Do for rivers first
  geoVectorExtractedRivers <- terra::extract(baseWaterRivers, vectorBase)
  baseWaterRivers[["distance"]] <- 0
  baseWaterRivers[["distance"]][!(baseWaterRivers$cellId %in% geoVectorExtractedRivers$cellId)] <- NA
  
  geoVectorExtractedLakes <- terra::extract(baseWaterLakes, lakeBoundaryVect)
  baseWaterLakes[["distance"]] <- 0
  baseWaterLakes[["distance"]][!(baseWaterLakes$cellId %in% geoVectorExtractedLakes$cellId)] <- NA
  
  writeRaster(baseWaterRivers, "data/external/waterDistance/distanceRasterRivers.tiff")
  writeRaster(baseWaterLakes, "data/external/waterDistance/distanceRasterLakes.tiff")
}

# Calculate the minimum distance to a water body at each pixel in Norway
riverMask <- terra::distance(baseWaterRivers[["distance"]])
lakeMask <- terra::distance(baseWaterLakes[["distance"]])
totalMask <- c(riverMask, lakeMask)
names(totalMask) <- c("river", "lake")
totalMask$minimum <- ifel(totalMask$river > totalMask$lake, totalMask$lake, totalMask$river)
distanceRaster <- totalMask$minimum
writeRaster(distanceRaster, "data/waterDistance/distanceRaster.tiff", overwrite = TRUE)
