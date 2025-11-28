
#### RIVER TO CATCHMENT MATCHING ####

# We need to assign the various rivers we have along Elvenett to the right Nedbørfelt. For the majority
# of these we don't need to worry - it's already in the dataframe. But some don't have a valid catchment,
# or are missing entirely.

library(sf)
library(dplyr)

###----------------####
#### 1. DATA SETUP ####
###----------------####

rivers <- readRDS("data/scaledRiverDataFull.RDS")
catchments <- readRDS("data/scaledCatchmentDataFull.RDS")

rivers <- rivers[,c("streknLnr", "vassdragNr",  "elvelengde", "elveorden", "elevation", 
                    "elevation_variability", "summer_temperature", "distance_to_roads")]
catchments <- catchments[,!(colnames(catchments) %in% "ID")]

# Find which rivers don't have a corresponding catchment number
riverNoCatchment <- rivers[!(rivers$vassdragNr %in% catchments$vassdragNr),]

# Now we turn their current (invalid) catchment numbers into NAs 
rivers$vassdragNr[rivers$streknLnr %in% riverNoCatchment$streknLnr] <- NA

###----------------------------------####
#### 2a. ASSIGN CATCHMENTS TO RIVERS ####
###----------------------------------####

# Now find an appropriate catchment number for them by checking which catchments they lie in
riversWithNA <- riverNoCatchment[,c("vassdragNr", "streknLnr")] |> st_transform(st_crs(catchments))
intersections <- st_intersects(riversWithNA, catchments)

# Start match-up data frame
riverMatchUp <- data.frame(riverNumbers = riverNoCatchment$streknLnr)

# Get river stretches which only lie in one catchment
catchmentNumbersNA <- catchmentNumbers <- lapply(intersections, unique)
names(catchmentNumbersNA) <- names(catchmentNumbers) <- riversWithNA$streknLnr
catchmentsNotUnique <- unlist(lapply(intersections, FUN = function(x) {length(x) > 1}))

# Switch catchments lists for the rivers with multiple catchments to a single NA so there's just
# one value for each and we can produce a single vector.
catchmentNumbersNA[catchmentsNotUnique] <- NA
riverMatchUp$catchments <- unlist(catchmentNumbersNA)
riverMatchUp$vassdragNr <- catchments$vassdragNr[riverMatchUp$catchments]

# Now add a valid catchment number to rivers
rivers$catchmentMatch1 <- riverMatchUp$vassdragNr[match(rivers$streknLnr, riverMatchUp$riverNumbers)]
rivers$vassdragNr <- ifelse(is.na(rivers$vassdragNr), rivers$catchmentMatch1, rivers$vassdragNr)

###-------------------------------------------####
#### 2b. ASSIGN MULTIPLE CATCHMENTS TO RIVERS ####
###-------------------------------------------####

# Now we have to deal with the rivers that go through multiple catchments
# Instead of assigning a code to each river that we can use to pull catchments values in 
# later on, we need to create those catchment values now. We do that by taking the weighted means
# (weighted by area) of all the relevant catchments.
riverStillNoCatchment <- rivers[rivers$streknLnr %in% riverMatchUp$riverNumbers[is.na(riverMatchUp$catchments)],
                                c("vassdragNr", "elvelengde", "streknLnr")
]

# Get catchment combinations
catchmentCombinations <- catchmentNumbers[catchmentsNotUnique]
catchmentCombinationsNamed <- lapply(catchmentCombinations, FUN = function(x) {
  listNamed <- catchments$vassdragNr[x]
  listNamed
})

# Sample of what it need to look like :
catchmentKey <- data.frame(strkNr = names(catchmentCombinationsNamed), 
                           catchmentName = paste0("catchment", names(catchmentCombinationsNamed)))

##  Just using this for the moment
# scaledCatchmentDataFull <- scaledCatchmentDataFull[,c(2,4,20:40)]

# River by river, we isolate the relevant catchments, get them all in a table and take
# their weighted means. The result is a table we can pull from in the bext step.
mergedCatchments <- lapply(1:nrow(catchmentKey), FUN = function(catch) {
  focalStrk <- catchmentKey$strkNr[catch]
  relevantCatchments <- catchmentCombinationsNamed[[focalStrk]]
  #relevantCatchments <- c("123.B2B3", "123.F11", "123.A6")
  relevantCatchmentData <- catchments[catchments$vassdragNr %in% relevantCatchments,]
  newRow <- relevantCatchmentData %>% 
    summarise(across(net_primary_productivity:corine.corine_Water_bodies, ~weighted.mean(., w = enhAreal)),
              enhAreal = sum(enhAreal))
  newRow$vassdragNr <- focalStrk
  newRow$ID <- 
  newRow <- newRow[colnames(catchments)]
}
)
mergedCatchmentDF <- do.call(rbind, mergedCatchments)
totalCatchmentList <- rbind(catchments, mergedCatchmentDF)

# Lastly, if vassdragNr is still NA, the the StrekLnr becomes the vassdragNr to connect the river to the
# catchment
rivers$vassdragNr <- ifelse(is.na(rivers$vassdragNr), rivers$streknLnr, rivers$vassdragNr)

# And now we can merge all the catchment data to the river data
mergedEnvCovList <- merge(rivers, st_drop_geometry(catchments), all.x = TRUE, by = "vassdragNr")


###--------------####
#### 3. CLEAN-UP ####
###--------------####
# # Let get rid of some of the crap
mergedEnvCovList <- mergedEnvCovList[,!(colnames(mergedEnvCovList) %in% c("corine.corine_Sclerophyllous_vegetation", "catchmentMatch1"))]
colnames(mergedEnvCovList) <- gsub("corine.", "", colnames(mergedEnvCovList))
colnames(mergedEnvCovList) <- gsub("kalkinnhold.", "", colnames(mergedEnvCovList))
mergedEnvCovList$elveorden <- as.factor(as.character(mergedEnvCovList$elveorden))
mergedEnvCovList$elevation_variability[is.na(mergedEnvCovList$elevation_variability)] <- 0

for (col in c("elvelengde", "enhAreal")) {
  mergedEnvCovList[,col] <- scale(st_drop_geometry(mergedEnvCovList[,col]))
}

saveRDS(mergedEnvCovList, "data/environmentalDataExport.RDS")
st_write(obj = mergedEnvCovList, dsn = "data/finalData/environmentalData.gpkg", layer='network', append = FALSE)

