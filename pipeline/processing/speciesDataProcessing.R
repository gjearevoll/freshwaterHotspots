
#### SPECIES DATA IMPORT ####

# All species data is brought over directly from the Hotspots pipeline. There it has been 
# downloaded, filtered and processed, and assigned a data type. THe hard work is done, here
# we just need to adjust it for freshwater use.

# Two major filters we need to use here.
# 1. Only freshwater species (lists pre-compiled by Hanne Krogstie)
# 2. Only observations within a certain distance of a freshwater body

sapply(list.files("functions", full.names = TRUE), source)
library(dplyr)
library(sf)
library(terra)

###----------------####
#### 1. DATA SETUP ####
###----------------####

regionGeometry <- readRDS("data/regionGeometry.RDS")

speciesGroups <- c("birds", "insects","aquaticPlants", "fish")

# Create a distance buffer around waterways that we can use to remove observations that are
# too far from a river.
distanceBuffer <- 2000

# The data that is brought over from the Hotspots pipeline needs to be saved in the folder
# "data/species/rawData" with the filename "speciesDataProcessed[speciesGroupName].RDS".
proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'

# # We import water catchments to find relevant catchments later on
# catchments <- read_sf("data/external/riverNetwork/Nedborfelt/Nedborfelt_RegineEnhet.shp")
# catchments$nbfHavNr[is.na(catchments$nbfHavNr)] <- paste0("NAcatchment", 1:length(catchments$nbfHavNr[is.na(catchments$nbfHavNr)]))
# nedborFelts <- unique(catchments$nbfHavNr)
vannomrade <- st_read("data/external/riverNetwork/waterAreas.gpkg")
###--------------------####
#### 2. DATA FILTERING ####
###--------------------####

# Now cycle through each species group
for (speciesGroup in speciesGroups) {
  
  cat("\nImporting", speciesGroup, "data")
  speciesData <- readRDS(paste0("data/species/rawData/speciesDataProcessed",speciesGroup ,".RDS"))
  
  if (speciesGroup == "fish") {
    huitfeldtKaas <- readRDS("localArchive/HuitfeldtKaas/processedDataset.RDS")
    speciesData[["Huitfeldt Kaas: Freswhater fish distribution in Norway 1918"]] <- huitfeldtKaas
  }
  
  # Import names of species we want to filter down to.
  cat("\nImporting", speciesGroup, "names")
  speciesLists <- readRDS(paste0("data/species/taxaLists/", speciesGroup, "Names.rds"))[[1]]
  cat("\nGetting correct", speciesGroup, "names")
  
  # Get correct species names for the list
  speciesNamesCorrect <- getGbifBackbone(speciesLists)$species
  speciesNamesCorrect <- gsub(" ", "_",speciesNamesCorrect[!is.na(speciesNamesCorrect)])
  
  # If the species group is birds, we need to import TOV data
  if (speciesGroup == "birds") {
    birdDataTOV <- readRDS("localArchive/birdDataTOV.RDS")
    birdDataTOVFiltered <- birdDataTOV[birdDataTOV$simpleScientificName %in% speciesNamesCorrect,]
    speciesData[["TOVData"]] <- st_transform(birdDataTOVFiltered, st_crs(speciesData[[1]])) |>
      st_crop(st_transform(st_as_sf(regionGeometry), st_crs(speciesData[[1]])))
    cat("Birds data filtered on TOV species.")
  }
  
  # Get right species
  speciesDataFiltered <- lapply(speciesData, FUN = function(ds) {
    dsFiltered <- ds[ds$simpleScientificName %in% speciesNamesCorrect,]
    dsFiltered
  })
  speciesDataFiltered <- speciesDataFiltered[lapply(speciesDataFiltered, nrow) != 0]
  
  # Get relevant vector for distance to water
  cat("\nImprting water distance raster")
  if (file.exists("data/external/waterDistance/distanceRaster.tiff")) {
    distanceRaster <- rast("data/external/waterDistance/distanceRaster.tiff")
  } else {
    source("pipeline/processing/waterDistanceCalculation.R")
  }
  
  
  # Calculate distance (only for non-fish species, we assume fish sightings are in water bodies)
  if (speciesGroup != "fish") {
    cityMaskNA <- st_transform(st_as_sf(as.polygons(ifel(distanceRaster > distanceBuffer, NA, 1))), crs= "+proj=longlat +ellps=WGS84")
    
    maskedData <- lapply(speciesDataFiltered, FUN = function(x) {
      newDatasetLongLat <- st_transform(x, crs = "+proj=longlat +ellps=WGS84")
      newDatasetMasked <- st_intersection(newDatasetLongLat, cityMaskNA)
      cat("\nDataset masked.", (nrow(x) - nrow(newDatasetMasked)), "entries removed.")
      newDatasetMasked2 <- st_transform(newDatasetMasked, crs = st_crs(x))
      return(newDatasetMasked2)
    })
    speciesDataFiltered <- maskedData[lapply(maskedData, nrow) != 0]
  }
  
  ###------------------####
  #### 3. REFORMATTING ####
  ###------------------####
  
  # Needs a completely new format for the freshwater models, created below
  
  cat("\nEditing", speciesGroup, "data to standard format")
  ##Load in the species sf data frame 
  species_observations_edited <- lapply(1:length(speciesDataFiltered), function(ds) {
    dataset <- speciesDataFiltered[[ds]]
    datatype <- unique(dataset$dataType)
    
    # Name columns
    dataset$dataset_name <- names(speciesDataFiltered)[ds]
    dataset$y <- ifelse(dataset$dataType == "PO", 1, dataset$individualCount)
    dataset$e <- ifelse(dataset$dataType == "PO",0, 1)
    dataset$likelihood <- dataset$dataType
    dataset$PO_intercept <- ifelse(dataset$dataType == "PO",1, NA)
    dataset$PA_intercept <- ifelse(dataset$dataType == "PA",1, NA)
    dataset$Counts_intercept <- ifelse(dataset$dataType == "Counts",1, NA)
    dataset$species_name <- dataset$simpleScientificName
    dataset[!is.na(dataset$species_name),c("dataset_name", "y", "e", "likelihood", "PO_intercept", "PA_intercept", "Counts_intercept", "species_name")]
  })  
  species_observations_collated <- do.call(rbind, species_observations_edited)
  
  
  ###-----------------------####
  #### 4. SPECIES FILTERING ####
  ###-----------------------####
  
  
  # Finally, remove any species with less than 50 observations
  speciesTallies <- st_drop_geometry(species_observations_collated) %>%
    filter(PO_intercept == 1 | (PA_intercept == 1 & y == 1) | (Counts_intercept == 1 & y > 0)) %>%
    group_by(species_name) %>%
    tally() %>% as.data.frame()
  speciesToKeep <- speciesTallies$species_name[speciesTallies$n >= 50]
  speciesToRemove <- speciesTallies$species_name[speciesTallies$n < 50]

  # species_observations <- species_observations_collated[species_observations_collated$species_name %in% speciesToKeep,]
  # cat("\nRemoving",length(speciesToRemove), "species due to too few observations.")
  
  
  # Get catchment lists per species
  speciesCatchmentList <- list()
  species_observations_collated_list <- list()
  speciesCatchmentListAllPresences <- list()
  for (fs in 1:length(speciesToKeep)) {
    focalSpecies <- speciesToKeep[fs]
    focalObs <- species_observations_collated[species_observations_collated$species_name %in% focalSpecies,]
    if (st_crs(focalObs) != st_crs(vannomrade)) {focalObs <- st_transform(focalObs, st_crs(vannomrade))}
    
    # See which observations intersect with the omrade
    waterAreasVec <- unlist(lapply(st_intersects(focalObs, vannomrade), 
                  FUN = function(x) {if (length(x) == 0) return(NA) else if (length(x) > 1) return(x[[1]]) else x}))
    focalObs$catchmentLocation <- vannomrade$navn[waterAreasVec]
    focalObs$presence <- ifelse(focalObs$y > 0,1,0)
    
    # Return list of all catchments with any presences
    speciesCatchmentListAllPresences[[fs]] <- unique(st_drop_geometry(focalObs)[focalObs$y > 0, "catchmentLocation"])
    
    totalObsPerCatchment <- st_drop_geometry(focalObs)[!is.na(focalObs$catchmentLocation),] %>%
      filter(presence == 1) %>%
      group_by(catchmentLocation) %>% 
      tally() %>% filter(n > 20) %>%as.data.frame()
    paObsPerCatchment <- st_drop_geometry(focalObs)[!is.na(focalObs$catchmentLocation),] %>%
      filter(presence == 1 & likelihood %in% c("PA", "Counts")) %>%
      group_by(catchmentLocation, likelihood) %>% 
      tally() %>% filter(n > 0) %>%as.data.frame()
    
    
    filteredSpeciesList <- focalObs[(focalObs$catchmentLocation %in% totalObsPerCatchment$catchmentLocation) &
                                      (focalObs$catchmentLocation %in% paObsPerCatchment$catchmentLocation),]
    speciesCatchmentList[[fs]] <- unique(filteredSpeciesList$catchmentLocation)
    species_observations_collated_list[[fs]] <- filteredSpeciesList
  }
  new_species_observation_collated <- do.call(rbind, species_observations_collated_list)
  
  # cat("\nSaving species data.")
  saveRDS(new_species_observation_collated, paste0("data/species/", speciesGroup,"FinalVersion.RDS"))
  
  # Filter out any empty groups
  names(speciesCatchmentList) <- names(speciesCatchmentListAllPresences) <- speciesToKeep
  speciesCatchmentList <- speciesCatchmentList[lapply(speciesCatchmentList, length) > 0]
  speciesCatchmentListAllPresences <- speciesCatchmentListAllPresences[lapply(speciesCatchmentListAllPresences, length) > 0]
  saveRDS(speciesCatchmentList, paste0("data/species/", speciesGroup,"InCatchments.RDS"))
  saveRDS(speciesCatchmentListAllPresences, paste0("data/species/", speciesGroup,"FullCatchments.RDS"))
  
}


