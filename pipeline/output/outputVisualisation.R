
#####################
### VISUALISATION ###
#####################

# Visualise results that have been output by the output processing script.

# Vector this and then rasterize it
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)

# Import covariates and assign segment numbers
covariates = st_read("data/finalData/environmentalData.gpkg")
covariates <- covariates[!duplicated(covariates),]
covariates$streknLnr[is.na(covariates$streknLnr)] <- paste0("nastretch", seq(1:length(covariates$streknLnr[is.na(covariates$streknLnr)])))
stretchMatch <- covariates[, c("vassdragNr", "streknLnr")]

# Import species data
speciesGroup <- "fish"

modelFolder <- paste0("data/species/", speciesGroup)
modelOutputFolder <- paste0("data/species/modelOutputs/", speciesGroup)


cat("Vectorising output for rasterisation\n")

# Create raster grid for projection
regionGeometry <- readRDS("data/regionGeometry.RDS")
totalOutput <-  st_read(paste0(modelOutputFolder, "/", speciesGroup, "Richness.shp"))
vectTotalOutput <- vect(totalOutput)
er <- rast(ext(regionGeometry), resolution=1000, crs = crs(regionGeometry)) |>
  project(vectTotalOutput)
norwayRaster <- rasterize(vectTotalOutput, er, "richness") |> project(regionGeometry)

# Get an excerpt for a catchment
# 016 is Trondheim (500 x 400), 212 is Altavassdraget (400 x 300)
for (catchmentNumber in c("212", "016")) {
  catchmentName <- if (catchmentNumber == "212") "Altavassdraget" else "Skiensvassdraget/Langesundsfjorden"
  
  # Get only relevant streams
  subCatchment <- stretchMatch[substr(stretchMatch$vassdragNr,1,nchar(catchmentNumber)) %in% catchmentNumber, ]
  subStrk <- totalOutput[totalOutput$streknLnr %in% subCatchment$streknLnr,]
  totalScaled <- (max(subStrk$richness) - subStrk$richness)/  (max(subStrk$richness) - min(subStrk$richness))
  sdScaled <- (max(subStrk$sd) - subStrk$sd)/  (max(subStrk$sd) - min(subStrk$sd))
  catchment <- st_zm(cbind(subStrk, totalScaled, sdScaled))
  catchment32633 <- st_transform(catchment, 32633)
  catchmentTrunctated <- truncateValue(catchment32633, "sdScaled", c(0.01,0.99))
  
  
  richnessFigure <- ggplot(data = catchmentTrunctated["totalScaled"]) +
    geom_sf(aes(colour = totalScaled), lwd = 0.2)  +
    scale_colour_grass_c(palette = "viridis", na.value = NA, limits = c(0, 1), direction = -1) +
    coord_sf(datum = st_crs(catchment32633)) +
    theme_void() +
    labs(colour = "Scaled richness", title =  catchmentName)
  ggsave(richnessFigure, filename = paste0(modelOutputFolder, "/", gsub("/","",catchmentName), "_richness.png"), 
         units = "px", width = 1100, height = 1100)
  
  uncertaintyFigure <- ggplot(data = catchmentTrunctated["sdScaled"]) +
    geom_sf(aes(colour = sdScaled), lwd = 0.2)  +
    scale_colour_grass_c(palette = "byr", na.value = NA, limits = c(0, 1), direction = 1) +
    coord_sf(datum = st_crs(catchment32633)) +
    theme_void() +
    labs(colour = "Scaled uncertainty", title =  catchmentName)
  ggsave(uncertaintyFigure, filename = paste0(modelOutputFolder, "/", gsub("/","",catchmentName), "_uncertainty.png"), 
         units = "px", width = 1100, height = 1100)
  
  # Make hotspots map\
  quantiles <- quantile(catchment32633$totalScaled, c(0, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
  catchment32633$hotspot <- cut(catchment32633$totalScaled, breaks=quantiles)
  levels(catchment32633$hotspot) <- c(NA, "10%", "5%", "1%")
  
  hotspotMap <- ggplot(data = catchment32633) +
    geom_sf(aes(colour = hotspot), lwd = 0.2)  +
    scale_colour_manual(values = c("orange", "red", "black"), na.value = "gray")+
    coord_sf(datum = st_crs(catchment32633)) +
    theme_void() +
    labs(colour = "Hotspots", title =  catchmentName)
  ggsave(hotspotMap, filename = paste0(modelOutputFolder, "/", gsub("/","",catchmentName), "_hotspots.png"), 
         units = "px", width = 1100, height = 1100)
}
# Get likelihoods summarised for each catchment where this is possible


# Apply fixed effects to catchments where it is not
## THis also involves for each catchment, finding the closest catchment nearby


# Bring everything together as one