
###---------------------###
#### Output processing ####
###---------------------###

# This script processes and compiles the different outputs produced by our model runs.

# For the moment, I've commented out the bits that allow us to run projections on non-modelled catchments using
# the nearest possible catchment as opposed to the aggregation of all catchments.

# Import relevant libraries
library(sf)
library(dplyr)

# Import species group and data
args <- commandArgs(trailingOnly = TRUE)
speciesGroup <- args[1]

modelFolder <- paste0("data/species/", speciesGroup)
modelOutputFolder <- paste0("data/species/modelOutputs/", speciesGroup)
speciesModelledList <- readRDS(paste0(modelFolder, "InCatchments.RDS"))
modelledCatchmentList <- readRDS(paste0(modelFolder, "CatchmentList.RDS"))
allPresenceList <- readRDS(paste0(modelFolder, "FullCatchments.RDS"))
speciesModelled <- names(speciesModelledList)

# Get centroids
waterAreas <- st_read("data/external/riverNetwork/waterAreas.gpkg")
centroidList <- st_centroid(waterAreas)

# Get env data in too
covariates = st_read("data/finalData/environmentalData.gpkg")
covariates <- covariates[!duplicated(covariates),]

# Get full strekLnr map for plotting
covariates$streknLnr[is.na(covariates$streknLnr)] <- paste0("nastretch", seq(1:length(covariates$streknLnr[is.na(covariates$streknLnr)])))
stretchMatch <- covariates[, c("vassdragNr", "streknLnr")]

# Now get geometries for each stretch - note that since there's so many, we have to do these uniquely
duplicatedStreknLnr <- covariates$streknLnr[duplicated(covariates$streknLnr)]
covariatesNonDupes <- covariates[!(covariates$streknLnr %in% duplicatedStreknLnr), "streknLnr"]
covariatesDupes <- covariates[covariates$streknLnr %in% duplicatedStreknLnr, "streknLnr"] %>%
  group_by(streknLnr) %>%
  summarise()
strekGeom <- rbind(covariatesNonDupes, covariatesDupes)

mergeTable <- readRDS("data/catchmentsWaterAreas.RDS")

# Now we want a list of each species, containing he following information
# 1. Fixed effects for each catchment where a model ran
# 2. A centrepoint for each catchment where a model ran
# 3. Likelihoods for each catchment where a model ran
# 4. A list of catchments we need to predict onto
downloadSpeciesModels <- lapply(speciesModelled, FUN = function(s) {
  
  # Temp code since I ran as the old version
  # Using trout as an example
  #s <- "Salmo_trutta"
  cat("Starting processing for",s, "\n")
  
  # Find catchments we ran a model for
  modelledCatchments <- names(modelledCatchmentList[unlist(lapply(modelledCatchmentList, FUN = function(catch) {catch == s}))])
  modelledCatchmentsShort <-   gsub("/","",gsub("^\\d+|\\d+$", "", modelledCatchments))  
  
  # Make sure models actually ran
  effectFileNames <- paste0(modelOutputFolder, "/", modelledCatchmentsShort, "/", modelledCatchments, "fixedEffects.RDS")
  checkEffectsExist <- sapply(effectFileNames, file.exists)
  modelledCatchments <- modelledCatchments[checkEffectsExist]
  modelledCatchmentsShort <-   gsub("/","",gsub("^\\d+|\\d+$", "", modelledCatchments))  
  cat(length(modelledCatchments), "catchments were successfully modelled\n")
  if (length(modelledCatchments) == 0){return(NA)}
  
  #  Download fixed effects
  fixedEffectList <- lapply(effectFileNames[checkEffectsExist], readRDS) |> setNames(modelledCatchmentsShort)
  mergedEffects <- data.frame(mean = rowMeans(do.call(cbind, lapply(fixedEffectList, FUN = function(x) {x$mean}))),
                              sd = sqrt(rowSums(do.call(cbind, lapply(fixedEffectList, FUN = function(x) {x$sd}))^2)/length(fixedEffectList)))
  row.names(mergedEffects) <- row.names(fixedEffectList[[1]])
  
  # Get list of catchments to project onto, then get rid of the ones we have models for
  allCatchmentsWithPresence <- allPresenceList[[s]][!is.na(allPresenceList[[s]])]
  catchmentsToProject <- allCatchmentsWithPresence[!(allCatchmentsWithPresence %in% modelledCatchmentsShort)]
  
  # Find which catchments are closest to those we have to project to
  catchmentTable <- data.frame(catchmentsToProject = catchmentsToProject)
  catchmentsForUse <- centroidList[centroidList$navn %in% modelledCatchmentsShort,]
  
  cat("Projecting data for",nrow(catchmentTable), "water areas\n")
  
  # Get list containing projected results
  projectedList <- lapply(catchmentTable$catchmentsToProject, FUN = function(catch) {
    
    cat("\tProjecting data for",catch,"\n")
    
    # Get centroid of focal catchment and find the closest centroid from the models we successfully ran
    focalCP <- centroidList[centroidList$navn == catch,]
    #catchmentsForUseSub <- catchmentsForUse
    #catchmentsForUseSub$extraGeom <- focalCP$geom
    #catchmentsForUseSub$dist <- st_distance(catchmentsForUseSub$geom, catchmentsForUseSub$extraGeom)[,1]
    #closestModel <- catchmentsForUseSub$navn[catchmentsForUseSub$dist == min(catchmentsForUseSub$dist)]
    
    # Get fixed effects for the closest catchment
    #effectsToUse <- fixedEffectList[[closestModel]]
    
    # If there are more than one species then we need to change the row names later on
    #moreSpecies <- ifelse(length(unique(effectsToUse$ID)) > 1, TRUE, FALSE)
    
    #effectsToUse <- effectsToUse[effectsToUse$ID == s, c("mean", "sd")]
    #if (moreSpecies) {row.names(effectsToUse) <- substr(row.names(effectsToUse),1,nchar(row.names(effectsToUse))-2)}
    focalCatchCovs <- covariates[covariates$vassdragNr %in% mergeTable$vassdragNr[mergeTable$newName == focalCP$navn],]
    streamNames <- focalCatchCovs$streknLnr
    
    # Get results
    focalCatchCovs$speciesIntercept <- 1
    focalCatchCovs <- focalCatchCovs[,row.names(mergedEffects)]
    habitatSuitability <- as.matrix(st_drop_geometry(focalCatchCovs)) %*% as.matrix(mergedEffects)
    focalCatchCovs$mean <- habitatSuitability[,1]
    focalCatchCovs$sd <- habitatSuitability[,2]
    focalCatchCovs$streknLnr <- streamNames
    
    st_drop_geometry(focalCatchCovs[,c("streknLnr", "mean", "sd")])
  }) |> setNames(catchmentTable$catchmentsToProject)
  
  cat("Aggregating projected likelihoods\n")
  projectedLhoods <- do.call(rbind, projectedList) %>%
    group_by(streknLnr) %>%
    summarise(mean = mean(mean, na.rm = T),
              sd = mean(sd, na.rm = T)) %>%
    as.data.frame()
  
  # Now get likelihoods for the rivers we actually have
  likelihoodFileNames <- paste0(modelOutputFolder, "/", modelledCatchmentsShort, "/", modelledCatchments, "likelihoods.RDS")
  checkLhoodsExist <- sapply(likelihoodFileNames, file.exists)
  cat("Generating likelihoods for", length(likelihoodFileNames[checkLhoodsExist]), "water areas\n")
  mapLikelihoods <- lapply(likelihoodFileNames[checkLhoodsExist], FUN = function(l) {
    cat("\tProjecting data for",l,"\n")
    likelihoodFocal <- readRDS(l)
    assignedValues <- st_drop_geometry(likelihoodFocal) %>%
      filter(species_name == s) %>%
      group_by(streknLnr) %>%
      # HERE WE NEED TO UPDATE SCRIPT ONCE MEAN IS USED TO THE FOLLOWING
      summarise(exMean = mean(extractedMean, na.rm = TRUE),
                exSD = sum((extractedSD)^2, na.rm = TRUE),
                tally = length(streknLnr))
    assignedValues$sd <- sqrt(assignedValues$exSD/assignedValues$tally)
    # summarise(exMean = mean(extractedSMean, na.rm = TRUE),
    #           exSD = mean(spatialFieldMean, na.rm = TRUE))
    catchmentValues <- covariates[covariates$streknLnr %in% assignedValues$streknLnr,] 
    catchmentValues$mean <- assignedValues$exMean[match(catchmentValues$streknLnr, assignedValues$streknLnr)]
    catchmentValues$sd <- assignedValues$sd[match(catchmentValues$streknLnr, assignedValues$streknLnr)]
    st_drop_geometry(catchmentValues[,c("streknLnr", "mean", "sd")])
  } 
  )
  
  # allLikelihoods <- do.call(rbind, lapply(c(mapLikelihoods, projectedList), FUN = function(wa) {
  #   wa[,c("mean")]
  # }))
  # 
  cat("Binding likelihoods for", s, "\n")
  allLikelihoods <- do.call(rbind, mapLikelihoods)
  allLikelihoods <- rbind(allLikelihoods, projectedLhoods)
  
  # Now get a table showing what was modelled, what was projected, what was absent
  catchmentDF <- data.frame(waterArea = waterAreas$navn, status = NA)
  catchmentDF$status <- ifelse(catchmentDF$waterArea %in% catchmentsToProject, "projected",
                               ifelse(catchmentDF$waterArea %in% modelledCatchments, "modelled", "absent"))
  
  cat("Compiling output for", speciesGroup, "\n")
  
  outputList <- list(likelihoods = allLikelihoods, fixedEffects = fixedEffectList,
                     catchmentDF = catchmentDF)
  return(outputList)
  
}) |> setNames(speciesModelled)

# Remove empty species
downloadSpeciesModels <- downloadSpeciesModels[!is.na(downloadSpeciesModels)]

saveRDS(downloadSpeciesModels, paste0(modelOutputFolder, "/", speciesGroup, "OutputList.RDS"))

# Get consistent streknLnr list
streknLnr <- st_drop_geometry(strekGeom)

# Bind species estimates together
meanLists <- lapply(seq_along(downloadSpeciesModels), FUN = function(sp) {
  cat("Summarising", names(downloadSpeciesModels)[sp], "\n")
  focalDS <- downloadSpeciesModels[[sp]]$likelihoods[,c("streknLnr", "mean", "sd")] %>%
    group_by(streknLnr) %>% summarise(mean = mean(mean, na.rm = TRUE),
                                      sd = sum(sd^2, na.rm = TRUE),
                                      tally = length(streknLnr)) %>% as.data.frame
  focalDS$sd <- sqrt(focalDS$sd/focalDS$tally)
  focalDS$mean <- INLA:::inla.link.cloglog(focalDS$mean, inverse = TRUE) 
  likelihoods2 <- merge(streknLnr, focalDS[,1:3], by = "streknLnr", all.x = TRUE)
  likelihoods2$mean[is.na(likelihoods2$mean)] <- 0
  likelihoods2$sd[is.na(likelihoods2$sd)] <- 0
  likelihoods2
})
# Bind them together
meanDF <- as.data.frame(do.call(cbind, lapply(meanLists, FUN = function(x) {x[,"mean"]}))) 
colnames(meanDF) <- names(downloadSpeciesModels)
meanDF <- cbind(strekGeom,meanDF)
meanDF$richness <- rowSums(st_drop_geometry(meanDF[,2:ncol(meanDF)]))

# Bind SD together (get the right formula later)
sdDF <- as.data.frame(do.call(cbind, lapply(meanLists, FUN = function(x) {x[,"sd"]}))) 
colnames(sdDF) <- names(downloadSpeciesModels)
sdDF <- cbind(strekGeom,sdDF)
sdDF$sd <- sqrt(rowSums(st_drop_geometry(sdDF[,2:ncol(sdDF)])^2)/(ncol(sdDF)-1))

totalOutput <- st_zm(cbind(meanDF[c("richness", "streknLnr")], st_drop_geometry(sdDF["sd"])))
st_write(totalOutput, paste0(modelOutputFolder, "/", speciesGroup, "Richness.shp"), append = FALSE)
