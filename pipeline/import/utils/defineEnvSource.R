if (covariate == "elevation") {
  # download and save if missing
  # download
  rasterisedVersion <- get_geonorge(targetDir = dataPath, dataFormat = "TIFF")
  
} else if (covariate == "elevation_variability") {
  rasterisedVersion <- checkAndImportRast("elevation", regionGeometryBuffer, file.path(covFolder, "elevation"), quiet = T)
  
} else if (covariate %in% c("summer_temperature", "summer_precipitation")) {
  rasterisedVersion <- checkAndImportRast(covariate, regionGeometryBuffer, dataPath, quiet = TRUE)
  if(is.null(rasterisedVersion)){
    # download
    rasterisedVersion <- get_met(covariate, dataPath)
  }
  
} else if (covariate == "habitat_heterogeneity") {
  corineBase <- checkAndImportRast("corine", regionGeometryBuffer, file.path(covFolder, "corine"), quiet = TRUE)
  if(is.null(corineBase)){
    # download
    corineBase <- get_corine(data_path = "data/external/covariates/corine")  
    # save
    file_path <- generateRastFileName(corineBase, "corine", file.path(covFolder, "corine"))
    writeRaster(corineBase, filename = file_path, overwrite = TRUE)
  }
  croppedRaster <- crop(corineBase, ext(terra::project(regionGeometryBuffer, corineBase)))
  
  library(rasterdiv)
  cropFactor <- ifelse(res > 1000, 3, ifelse(res <= 100, 9, 5))
  rasterisedVersion <- Shannon(croppedRaster, window = cropFactor)
} else if (covariate == "net_primary_productivity") {
  
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
  extractedValues <-  lapply(1:length(rasterisedVersion), FUN = function(x) {
    focalRaster <- rasterisedVersion[[x]]
    extractedValues <- extract(focalRaster,rivers)  
    colnames(extractedValues)[2] <- "cov"
    summarisedValues <- extractedValues %>%
      group_by(ID) %>% 
      summarise(extracted = mean(cov, na.rm = T)) %>%
      as.data.frame()  
    colnames(summarisedValues)[2] <- names(rasterisedVersion)[x]
    cat("\nSuccesfully extracted river values for", names(rasterisedVersion)[x])
    summarisedValues <- scale(summarisedValues[,2])
    summarisedValues
  })
  extractedValues <- do.call(cbind, extractedValues)
}