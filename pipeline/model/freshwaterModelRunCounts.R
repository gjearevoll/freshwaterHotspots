
#### RIVER NETWORK CREATION ####

# This script gets our river network in a condition that it can be used in to create a metric graph item later on.
# Because of the massive computation required, this is intended to be run in parallel in Sigma2.

# Bring in the necessary packages.

.libPaths(c("/cluster/projects/nn11017k/R"))

installedPackages <- installed.packages()
necessaryPackages <- c("SSN2", "SSNbler")
uninstalledPackages <- necessaryPackages[!(necessaryPackages %in% row.names(installedPackages))]
install.packages(uninstalledPackages)

##Create river network
library(SSN2) 
library(SSNbler)
library(PointedSDMs)
library(INLA)
library(sf)
library(terra)
library(MetricGraph)
library(fmesher)
library(rSPDE)
library(inlabru)
library(FreshwaterTools) ##Github or source
library(dplyr)

# Read in river data
cat("\nReading in river data\n")

# Get catchment to water areas tool
catchmentMatching <- readRDS("data/catchmentsWaterAreas.RDS")

args <- commandArgs(trailingOnly = TRUE)
speciesGroup <- args[1]
segment <- as.numeric(args[2])
#speciesGroup <- "fish"


# Import species list
speciesList <- readRDS(paste0("data/species/", speciesGroup,"CatchmentList.RDS"))

# Get omrade name
omradeName <- names(speciesList)[segment]
cat("omrade name:", omradeName, "\n")
#omradeNameNoDigits <- gsub("/","",gsub('[[:digit:]]+', '', omradeName))
omradeNameNoDigits <- gsub("/","",gsub("^\\d+|\\d+$", "", omradeName))  

# Import species list
focalSpecies <- speciesList[[omradeName]]

# Define projection
proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'

# Create lsn path
cat("Creating modelOutputs path\n")
modelOutputPath <- paste0("data/species/modelOutputs/",speciesGroup,"/",omradeNameNoDigits)
#lsnPath <- paste0("data/external/riverNetwork/processedNetwork/processedNetwork",catchmentSet)
if (!dir.exists(modelOutputPath)) {
  dir.create(modelOutputPath, recursive = TRUE)
}

# Load in metricGraph
load(paste0("data/external/riverNetwork/waterAreaGraphsNew/", omradeNameNoDigits,"/waterAreaGraph.RData"))
graphSize <- data.frame(feature = c("edges", "vertices"), n = c(graph$nE, graph$nV))

# Load species data
speciesData <- readRDS(paste0("data/species/",speciesGroup,"FinalVersion.RDS"))
speciesData <- speciesData[speciesData$species_name %in% focalSpecies & speciesData$catchmentLocation %in% omradeNameNoDigits,]
speciesData <- speciesData[,!(colnames(speciesData) %in% c("catchmentLocation", "presence"))]

cat(" Setting up species data \n")
IPS <- graph$get_data(format = 'sf')
IPS <- fm_cprod(IPS, data.frame(species = unique(speciesData$species))) ##ADD THE SPECIES IN THE MODEL HERE
IPS$weight <- NULL; IPS$.block <- NULL; IPS$.block_origin <- NULL
IPS$.group <- paste0('IPS.', IPS$species)

cat("Adding species data to the graph \n")
graph$clear_observations()
graph$add_observations(speciesData,
                       distance_on_edge = '.distance_on_edge',
                       edge_number = '.edge_number', normalized = TRUE,
                       include_distance_to_graph = FALSE,
                       group = c('likelihood', 'species_name'))

# Need to do some minor renaming
cat("Minor renaming of species column \n")
IPS$species_name <- IPS$species
IPS <- IPS[,colnames(graph$get_data(format = 'sf'))]
speciesInput <- graph$get_data(format = 'sf')
modelData <- rbind(speciesInput, IPS)

# Start model diagnostics
speciesCounts <- st_drop_geometry(speciesInput) %>%
  group_by(likelihood, species_name) %>% tally()


cat(nrow(speciesInput), "observed data points found in the graph \n")
if (nrow(speciesInput) < 5) {
  stop("Too few observed data points found in the graph!")
}

cat("Importing covariate data \n")
covariates = st_read("data/finalData/environmentalData.gpkg")
cat("Preparing covariate information \n")
covariates <- st_transform(st_zm(covariates), st_crs(graph$get_data(format = 'sf')))
cat("Extracting covariate information \n")
modelData <- ExtractCovariates(data = modelData, covariates = covariates)

IPSlength <- nrow(modelData) - nrow(speciesInput)
  
cat("Preparing model formula \n")
covEffects <- paste0(names(covariates), '(main = species_name, weights = ', names(covariates), ', model = "iid", hyper = list(prec = list(fixed = TRUE, initial = 0)))',
                     collapse = ' + ')

##Or any subset
cat("Constructing formula \n")
Components <- formula(paste0('~ - 1 + PO_intercept(1, mean.linear = 0, prec.linear = 1) +
                             PA_intercept(1, mean.linear = 0, prec.linear = 1) +
                             Counts_intercept(1, mean.linear = 0, prec.linear = 1) +
                             field(cbind(.edge_number, .distance_on_edge), model = spdeMod) +
                             speciesIntercept(main = species_name, model = "iid", constr = TRUE,
                             hyper = list(prec = list(fixed = TRUE, initial = 0))) +',
                             covEffects))

# Save model parameters
modelParams <- list(speciesCounts = speciesCounts, graphSize = graphSize, IPSlength = IPSlength)
saveRDS(modelParams, paste0(modelOutputPath, "/", omradeName, "modelParams.RDS"))

cat("Setting up the SPDE model \n")
spdeMod <- rspde.metric_graph(graph_obj = graph, nu = 2)

cat("Setting up likelihoods \n")
covsIncl <- names(covariates)[unlist(sapply(head(covariates), class)) == 'numeric']

Likelihoods <- MakeLikelihoods(data = modelData, 
                               effectsList = list(PO = c('PO_intercept', 'field', 'speciesIntercept', covsIncl),
                                                  PA = c('PA_intercept', 'field', 'speciesIntercept', covsIncl),
                                                  Counts = c('Counts_intercept', 'field', 'speciesIntercept', covsIncl)),
                               predictionFormula = list(intensity = ~ Counts_intercept + elvelengde + elevation_variability + summer_temperature +
                                                          enhAreal + net_primary_productivity + summer_precipitation +
                                                          svært_kalkfattig + temmelig_kalkfattig + svæk_intermediær + litt_kalkrik + svært_kalkrik +
                                                          Built_up_area + Agro_forestry_areas + Constructed_green_space + Broad_leaved_forest +
                                                          Coniferous_forest + Mixed_forest + Natural_grasslands + Moors_and_heathland +
                                                          Transitional_woodland_shrub + Bare_rocks + Glaciers_and_perpetual_snow + Marsh_bog_fen + 
                                                          Water_bodies + speciesIntercept + field,
                                                        bias = ~ distance_to_roads)) 

#save.image("data/external/riverNetwork/processedNetworkTrondheim/trondheimImage.RData")


cat("Fitting the model \n")
model <- bru(Components, Likelihoods, options = list(verbose = TRUE))
#saveRDS(model, paste0(modelOutputPath, "/", omradeName, "modelExample.RDS"))

cat("Extracting fixed effects \n")

# Register model run status
modelParams[["modelRun"]] <- FALSE
saveRDS(modelParams, paste0(modelOutputPath, "/", omradeName, "modelParams.RDS"))


# Get fixed effects
fixedEffects <- do.call(rbind, model$summary.random[-1])
#fixedEffects$baseIntercept <- model$summary.fixed["PA_intercept","mean"]
fixedEffects$baseIntercept <- model$summary.fixed["Counts_intercept","mean"]

cat("Saving fixed effects \n")
saveRDS(fixedEffects, paste0(modelOutputPath, "/", omradeName, "fixedEffects.RDS"))

# Register model successful run status
modelParams[["modelRun"]] <- TRUE
saveRDS(modelParams, paste0(modelOutputPath, "/", omradeName, "modelParams.RDS"))


# OUTPUT 1: fixedEffects

cat("Extracting likelihood information \n")
# Get likelihoods
indexedModel <- bru_index(model, "intensity")
extractedMean <- model$summary.linear.predictor[indexedModel,"mean"]
extractedSD <- model$summary.linear.predictor[indexedModel,"sd"]

cat("Extracting spatial field effects \n")
# Get spatial field effects
indexedSField <- bru_index(model, "spatialField")
extractedSpatialMean <- model$summary.linear.predictor[indexedSField,"mean"]
extractedSpatialSD <- model$summary.linear.predictor[indexedSField,"sd"]

cat("Extracting bias effects \n")
# THis is a big question mark
indexedBiasField <- bru_index(model, "bias")
extractedBiasMean <- model$summary.linear.predictor[indexedBiasField,"mean"]
extractedBiasSD <- model$summary.linear.predictor[indexedBiasField,"sd"]

cat("Saving likelihood information \n")
lhoodData <- model$bru_info$lhoods$intensity$data
lhoodData$extractedMean <- extractedMean
lhoodData$extractedSD <- extractedSD
lhoodData$spatialFieldMean <- extractedSpatialMean
lhoodData$spatialFieldSD <- extractedSpatialSD
lhoodData$biasMean <- extractedBiasMean
lhoodData$biasSD <- extractedBiasSD
saveRDS(lhoodData, paste0(modelOutputPath, "/", omradeName, "likelihoods.RDS"))