
#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

library(terra)
library(sf)
library(dplyr)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

crs = 25833
res <- 500

###----------------####
#### 1. DATA SETUP ####
###----------------####

## Load in the sf river network
# These can be downloaded from https://nedlasting.nve.no/gis/?tk=elvenett& using the below options.
# kartformat = .shp
# koordinatsystem = EUREF89

cat("Loading river network")
rivers <-  read_sf("data/external/riverNetwork/Elv/Elv_Elvenett.shp")
rivers <- rivers[!duplicated(rivers),]
rivers2 <- rivers[!duplicated(rivers),c("elvenavn", "elvelengde")]
cat("Loading catchments")
catchments <- read_sf("data/external/riverNetwork/Nedborfelt/Nedborfelt_RegineEnhet.shp")
catchments <- catchments[,c("vassdragNr", "enhAreal")]

# Import covariate list
parameters <- read.csv("data/external/focalCovariates.csv", sep = ";")

# Create a buffer for downloading covariates
regionGeometryBuffer <- st_bbox(catchments) |>
  st_transform(crs) |> 
  st_as_sfc() |>
  st_buffer(10000) |>
  st_segmentize(dfMaxLength = 10000) |> 
  vect() 
saveRDS(regionGeometryBuffer, "data/regionGeometry.RDS")

###----------------------####
#### 2. COVARIATE IMPORT ####
###----------------------####

# Need to import covariates, convert to raster and then extract to hovedelv. Start doing this
# one by one, we can convert to a function later on.
covFolder <- file.path("data/external/covariates")

covariateList <- list()
rasterList <- list()
covariateNames <- c()

#Have to add "habitat_heterogeneity" back in later when there's time
for (covariate in parameters$covariate) {
  
  # Check for previous version of covariate. If none exists, create it.
  dataPath <- file.path(covFolder, covariate)
  cat("\nChecking for existing",covariate,"file")
  rasterisedVersion <- NULL
  if (dir.exists(dataPath)) {
    rasterisedVersion <- checkAndImportRast(covariate, regionGeometryBuffer, dataPath, quiet = TRUE)
    cat("\nExisting",covariate,"file found")
  } else {
    dir.create(dataPath)
    cat("\nNo",covariate,"file found, starting upload")
  }
  
  if (is.null(rasterisedVersion)) {
    
    source("pipeline/import/utils/defineEnvSource.R")
    
    # Now ensure that raster is correctly projected
    rasterisedVersion <- extend(rasterisedVersion, terra::project(regionGeometryBuffer, rasterisedVersion), snap = "out")
    file_path <- generateRastFileName(rasterisedVersion, covariate, dataPath)
    cat("\nSaving",covariate,"file")
    writeRaster(rasterisedVersion, filename = file_path, overwrite = TRUE)
    
    
  }
  
  rasterList[[covariate]] <- rasterisedVersion
  
}

###-----------------------####
#### 3. COVARIATE SUMMARY ####
###-----------------------####

# Vectorise rivers and catchments
cat("\nVectorising catchments")
vectCatchment <- vect(catchments[,"vassdragNr"]) |> project(paste0("EPSG:",crs))
cat("\nVectorising rivers")
vectRivers <- vect(rivers2) |> project(paste0("EPSG:",crs))

# Extract covariates either to river level or catchment level, depending on covariate
catchmentList <- list()
riverList <- list()
for (covariate in parameters$covariate) {
  
  # Check if there's a version that's already been extracted
  savedFileName <- file.path("data/external/covariates",covariate,"extractedVersion.RDS")
  if (!file.exists(savedFileName)) {
    extractedVersion <- readRDS(savedFileName)
  } else {
    
    covRaster <- rasterList[[covariate]]
    
    extractionType <- parameters$level[parameters$covariate == covariate]
    extractionLayer <- if (extractionType == "catchment") vectCatchment else vectRivers
    
    # Categorical rasters
    if (parameters$categorical[parameters$covariate == covariate]) {
      cats <- names(covRaster)
      catList <- list()
      cat("\nExtracting values for", covariate)
      for (focalCat in cats) {
        cat("\n\tProjecting values for", focalCat)
        covRasterProj <- project(covRaster[[focalCat]], "EPSG:25833")
        cat("\n\tExtracting values for", focalCat)
        focalExtract <- zonal(covRasterProj, extractionLayer, na.rm = TRUE)
        catList[[focalCat]] <- focalExtract
      }
      extractedVersion <- do.call(cbind, catList)
      ID <- 1:nrow(extractedVersion)
      extractedVersion <- cbind(ID, extractedVersion)
      
      # Continuous rasters
    } else {
      if (covariate == "elevation_variability") {
        cat("\nProjecting values for", covariate)
        covRasterProj <- project(covRaster, "EPSG:25833")
        cat("\nExtracting values for", covariate)
        extractedVersion <- extract(covRasterProj, extractionLayer) %>%
          group_by(ID) %>%
          summarise_all(sd, na.rm = TRUE)
        colnames(extractedVersion)[2] <- covariate
      } else {
        cat("\nProjecting values for", covariate)
        covRasterProj <- project(covRaster, "EPSG:25833")
        cat("\nExtracting values for", covariate)
        extractedVersion <- extract(covRasterProj, extractionLayer) %>%
          group_by(ID) %>%
          summarise_all(mean, na.rm = TRUE)
        colnames(extractedVersion)[2] <- covariate
      }
    }
    saveRDS(extractedVersion, paste0("data/external/covariates/", covariate, "/extractedVersion.RDS"))
  }
  
  # Add to relevant list
  if (extractionType == "catchment") {
    catchmentList[[covariate]] <- extractedVersion
  } else {
    riverList[[covariate]] <- extractedVersion
  }
  
}

###-----------------------####
#### 4. FINALISE DATASETS ####
###-----------------------####

# Need to create final versions of river and catchment data
# This will be merged in pipine/processing/riverCatchmentMatching.R

catchmentData <- cbind(catchmentList[[1]]$ID, do.call(cbind, lapply(catchmentList, FUN = function(x) {x[,-1]})))
colnames(catchmentData)[1] <- "ID"
scaledCatchmentData <- catchmentData
scaledCatchmentData[,2:ncol(scaledCatchmentData)] <- scale(scaledCatchmentData[,2:ncol(scaledCatchmentData)])
scaledCatchmentDataFull <- cbind(catchments[,c("vassdragNr", "enhAreal")], scaledCatchmentData)
saveRDS(scaledCatchmentDataFull, "data/scaledCatchmentDataFull.RDS")

riverData <- cbind(riverList[[1]]$ID, do.call(cbind, lapply(riverList, FUN = function(x) {x[,-1]})))
colnames(riverData)[1] <- "ID"
scaledRiverData <- riverData
scaledRiverData[,2:ncol(scaledRiverData)] <- scale(scaledRiverData[,2:ncol(scaledRiverData)])
scaledRiverDataFull <- cbind(rivers, scaledRiverData)
saveRDS(scaledRiverDataFull, "data/scaledRiverDataFull.RDS")



