
#### MODEL PREPARATION ####

# This script needs to prepare a list of catchment and species combinations that can be run.
# We start with a species group, then figure out how many of those species can be run for each 
# water area.

library(sf)

# Define species and get species data
speciesGroups <- c("birds", "insects", "fish", "aquaticPlants")
vannomrade <- st_read("data/external/riverNetwork/waterAreas.gpkg")


for (speciesGroup in speciesGroups) {
  speciesData <- readRDS(paste0("data/species/", speciesGroup,"FinalVersion.RDS"))
  speciesCatchmentList <- readRDS(paste0("data/species/", speciesGroup,"InCatchments.RDS"))
  
  # Get water areas
  
  # Get list of species per catchment
  catchmentList <- list()
  for (v in 1:nrow(vannomrade)) {
    voNavn <- vannomrade$navn[v]
    speciesVector <- c()
    for (s in 1:length(speciesCatchmentList)) {
      focalSpecies <- speciesCatchmentList[[s]]
      if (voNavn %in% focalSpecies) {
        speciesVector <- c(speciesVector, names(speciesCatchmentList)[s])
      }
    }
    if (is.null(speciesVector)) {speciesVector <- NA}
    catchmentList[[v]] <- speciesVector
  }
  names(catchmentList) <- vannomrade$navn
  catchmentsToRun <- catchmentList[unlist(lapply(catchmentList, FUN = function(x) {!(is.na(x[1]) & length(x) == 1)}))]
  
  # Get list of species for each catchment, with catchment split into no more than 5 species
  firstList <- list()
  for (ca in 1:length(catchmentsToRun)) {
    catch <- catchmentsToRun[[ca]]
    catchName <- names(catchmentsToRun)[ca]
    if (length(catch) > 1) {
      catchSplit <- rep(1:length(catch), each = 1)[1:length(catch)]
      catchmentSplit <- lapply(unique(catchSplit), FUN = function(cs) {
        catch[catchSplit == cs]
      }) |> setNames(paste0(catchName, unique(catchSplit)))
      firstList <- c(firstList, catchmentSplit)
    } else {
      catchment <- list(catch)
      names(catchment) <- catchName
      firstList <- c(firstList, catchment)}
  }# |> setNames(names(catchmentsToRun))
  
  saveRDS(firstList, paste0("data/species/", speciesGroup,"CatchmentList.RDS"))
}


