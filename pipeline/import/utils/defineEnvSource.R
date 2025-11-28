
### ENVIRONMENTAL SOURCE DEFINITION ####

# This script defines parameters for extracting the necessary environmental data from their
# various sources. It relies on the functions found in functions that begin
# with 'get_'. A full description of each data source can be found at 
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp 

if (covariate == "elevation") {
  
  # download and save if missing
  rasterisedVersion <- get_geonorge(targetDir = dataPath, dataFormat = "TIFF")
  
} else if (covariate == "elevation_variability") {
  
  # Can just download elevation data again - further processing takes place later on
  rasterisedVersion <- checkAndImportRast("elevation", regionGeometryBuffer, file.path(covFolder, "elevation"), quiet = T)
  
} else if (covariate %in% c("summer_temperature", "summer_precipitation")) {
  
  # Met data is available from a download linl
  rasterisedVersion <- checkAndImportRast(covariate, regionGeometryBuffer, dataPath, quiet = TRUE)
  if(is.null(rasterisedVersion)){
    rasterisedVersion <- get_met(covariate, dataPath)
  }
  
} else if (covariate == "corine") {
  
  
  # All this needs to be downloaded from corine beforehand - script for the get_corine function
  # has an in-depth description of how to do this.
  corineBase <- get_corine(data_path = "data/external/covariates/corine")  
  
  corineProcessed <- crop(corineBase, ext(terra::project(regionGeometryBuffer, corineBase)))
  levelTable <- levels(corineProcessed)[[1]]
  allCats <- unique(levelTable[,2])
  catList <- lapply(allCats, FUN = function(cat1) {
    if (is.na(cat1)) {return(NA)}
    catLevels <- levelTable$Value[levelTable$LABEL3 %in% cat1]
    catRaster <- ifel(corineProcessed %in% catLevels, 1, 0)
    #contRaster <- terra::project(catRaster, rivers2, method="average")
    catRaster
  }) |> setNames(allCats)
  rasterisedVersion <- unlist(catList)[!is.na(unlist(catList))]
  names(rasterisedVersion) <- paste0(covariate, "_", gsub(" ","_",stringr::str_replace_all(names(rasterisedVersion), "[[:punct:]]", "_")))
  rasterisedVersion <- rast(rasterisedVersion)
  
  
} else if (covariate == "habitat_heterogeneity") {
  
  corineBase <- get_corine(data_path = "data/external/covariates/corine")  
  file_path <- generateRastFileName(corineBase, "corine", file.path(covFolder, "corine"))
  writeRaster(corineBase, filename = file_path, overwrite = TRUE)
  
  croppedRaster <- crop(corineBase, ext(terra::project(regionGeometryBuffer, corineBase)))
  
  # Need a new package to get habitat heterogeneity
  library(rasterdiv)
  cropFactor <- ifelse(res > 1000, 3, ifelse(res <= 100, 9, 5))
  rasterisedVersion <- Shannon(croppedRaster, window = cropFactor)
} else if (covariate == "net_primary_productivity") {
  
  # MODIS functions no longer compatible - unfortunately we have to use previous versions of this data
  rasterisedVersion <- get_modis(regionGeometry, crs, covariate)
} else if (covariate == "distance_to_roads") {
  
  # Get relevant vector for water/roads
  vectorBase <- get_geonorge(dataName = "N250Kartdata", targetDir = dataPath, dataFormat = "FGDB")
  
  # get  base raster with expanded buffer (as closest road or lake may be over county lines)
  baseRasterDistance <- st_as_sf(regionGeometryBuffer) |>
    st_buffer(40000) |>
    st_transform(crs = paste0("EPSG:", crs)) |> 
    vect()
  baseWaterRaster <- terra::rast(extent = ext(baseRasterDistance), res = 1000, crs = paste0("EPSG:", crs))
  baseWaterRaster$cellId <- paste0("cell", 1:ncell(baseWaterRaster))
  
  # Extract vectors to a raster layer and figure out which cells have a road/wtaer body in them
  geoVectorExtracted <- terra::extract(baseWaterRaster, vectorBase)
  baseWaterRaster[[covariate]] <- 0
  baseWaterRaster[[covariate]][!(baseWaterRaster$cellId %in% geoVectorExtracted$cellId)] <- NA
  
  # Calculate distance
  rasterisedVersion <- terra::distance(baseWaterRaster[[covariate]])
} else if (covariate == "kalkinnhold") {
  
  rasterisedVersion <- get_artsdatabanken(covariate)
  
  allCats <- unique(levels(rasterisedVersion)[[1]][,2])
  catList <- lapply(allCats, FUN = function(cat1) {
    if (cat1 == "no data") {return(NA)}
    catRaster <- ifel(rasterisedVersion == cat1, 1, 0)
    contRaster <- terra::project(catRaster, rivers2, method="average")
    contRaster
  }) |> setNames(allCats)
  rasterisedVersion <- unlist(catList)[!is.na(unlist(catList))]
  names(rasterisedVersion) <- paste0(covariate, "_", gsub(" ","_",stringr::str_replace_all(names(rasterisedVersion), "[[:punct:]]", "_")))
  rasterisedVersion <- rast(rasterisedVersion)
  
}