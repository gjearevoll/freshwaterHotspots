
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

##Load in the sf river network
rivers <-  read_sf("data/external/riverNetwork/Elv_Hovedelv.shp")
rivers2 <- rivers[,c("elvenavn", "elvelengde")] |>
  st_transform(crs)

regionGeometryBuffer <- vect(ext(rivers2), crs = crs(rivers2))

# Need to import covariates, convert to raster and then extract to hovedelv. Start doing this
# one by one, we can convert to a function later on.
covFolder <- file.path("data/external/covariates")

covariateList <- list()
covariateNames <- c()

#Have to add "habitat_heterogeneity" back in later when there's time

for (covariate in c("net_primary_productivity", "elevation", "elevation_variability", 
                    "summer_temperature", "summer_precipitation", 
                    "distance_to_roads", "kalkinnhold")) {
  
  # Check for previous version of covariate
  dataPath <- file.path(covFolder, covariate)
  rasterisedVersion <- NULL
  if (dir.exists(dataPath)) {
    rasterisedVersion <- checkAndImportRast(covariate, regionGeometryBuffer, dataPath, quiet = TRUE)
  } else {
    dir.create(dataPath)
  }
  
  if (is.null(rasterisedVersion)) {
    
    source("pipeline/import/utils/defineEnvSource.R")
    
    if (!(covariate %in% c("elevation_variability", "kalkinnhold"))) {
      file_path <- generateRastFileName(rasterisedVersion, covariate, dataPath)
      writeRaster(rasterisedVersion, filename = file_path, overwrite = TRUE)
    }
    
  }
  
  # Extract summarised covariates for rivers
  
  if (covariate != "kalkinnhold") {
    extractedValues <- extract(rasterisedVersion,rivers)  
    colnames(extractedValues)[2] <- "cov"
    cat("\nSuccesfully extracted river values for", covariate)
    
    # Elevation variability uses sd instead of mean - only exception
    if (covariate != "elevation_variability") {
      summarisedValues <- extractedValues %>%
        group_by(ID) %>% 
        summarise(extracted = mean(cov, na.rm = T)) %>%
        as.data.frame()  
    } else {
      summarisedValues <- extractedValues %>%
        group_by(ID) %>% 
        summarise(extracted = sd(cov, na.rm = T)) %>%
        as.data.frame()  
    }
    
    # Scale and add to list
    colnames(summarisedValues)[2] <- covariate
    summarisedValues <- scale(summarisedValues[,2])
    covariateList[[covariate]] <- summarisedValues
    covariateNames <- c(covariateNames, covariate)
    
  } else { 
    
    covariateList[[covariate]] <- extractedValues
    covariateNames <- c(covariateNames, names(rasterisedVersion))
    
  }
}

# Bind and name finished version
covariateTable <- do.call(cbind, covariateList)
colnames(covariateTable) <- covariateNames

riversObject <- cbind(rivers, covariateTable)
st_write(riversObject, dsn="data/external/environmentalDataImported.gpkg", layer='covariates')


