
#### CATCHMENT CREATION ####

library(ggplot2)
library(sf)

# The following script creates appropriate catchment regions for us to run our models separately in.

# Import vannomrade
a<- st_read("../HotspotsDerived/processes/overlays/data/waterRegions/Vannomrader_0000_norge_25833_FILEGDB.gdb") %>%
  dplyr::select(navn)

# Import catchment areas
nedborFelt <-  read_sf("data/external/riverNetwork/Nedborfelt/Nedborfelt_RegineEnhet.shp")

# We also have 3 catchments that need to be split up into smaller areas
bigWaterAreas <- c("002", "012", "016")
load("data/external/riverNetwork/REGINE_sub.rda")

# Narrow down to larger catchments
nedborFelt$largeAreas <- REGINE_sub$ID[match(nedborFelt$vassdragNr, REGINE_sub$vassdragsnummer)]
nedborFelt$vassDragOmradeMidpoint <- gsub("\\..*","",nedborFelt$vassdragNr)
nedborFelt$vassDragOmrade <- ifelse(nedborFelt$vassDragOmradeMidpoint %in% bigWaterAreas, nedborFelt$largeAreas, nedborFelt$vassDragOmradeMidpoint)

# Assign every catchment to a vannomrade. We do this by running an intersection of the catchment with
# a water area. If the catchment intersects with multiple water areas, we assign it to the one it
# intersects with the most.
vannomradeList <- lapply(unique(nedborFelt$vassDragOmrade), FUN = function(v) {
  allCatchments <- nedborFelt[nedborFelt$vassDragOmrade %in% v,c("vassdragNr", "enhAreal", "vassDragOmrade")]
  combinedCatchments <- st_union(allCatchments)
  
  # If it's a normal catchments, find the intersection
  if (nchar(v) == 3) {
    intersections <- st_intersects(combinedCatchments, a)
    if (length(intersections[[1]]) > 1) {
      areaSize <- c()
      for (i in intersections[[1]]) {
        areaSize <- c(areaSize, st_area(st_intersection(combinedCatchments, a[i,])))
      }
      correctVO <- intersections[[1]][which(areaSize == max(areaSize))]
    } else {correctVO <- intersections[[1]][1]}
    catchment <- st_as_sf(combinedCatchments)
    catchment$navn <-  st_drop_geometry(a)[correctVO, "navn"]
    
    # If it's a subsection of one of the big catchments, no need, just name it and move on
  } else {
    catchment <- st_as_sf(combinedCatchments)
    catchment$navn <-  v
  }
  catchment$vassdrag <-  v
  catchment
})
vannomrade <- do.call(rbind, vannomradeList)
#catchmentMatching <- st_drop_geometry(vannomrade)
#saveRDS(catchmentMatching, "data/catchmentsWaterAreas.RDS")



# Now create a map for our new areas. This will be used to later to see which ones we should run 
# for different species.
vannomradeList2 <- lapply(unique(vannomrade$navn), FUN = function(v) {
  allCatchments <- vannomrade[vannomrade$navn %in% v,]
  combinedCatchments <- st_as_sf(st_union(allCatchments))
  combinedCatchments$navn <-  v
  combinedCatchments
})

vannomrade2  <- do.call(rbind, vannomradeList2)


# Plot this to get an idea of how our catchment looks
ss_centroids <- st_centroid(vannomrade2)
areaPlot1 <- ggplot(vannomrade2)+
  geom_sf() +
 # geom_sf_label(aes(label = navn)) +
  coord_sf(datum = st_crs(vannomrade2)) +
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  ggrepel::geom_label_repel(
    aes(label = navn, geometry = x),
    stat = "sf_coordinates",
    min.segment.length = 0,
    label.size = NA,
    fill = NA
  )
ggsave(areaPlot1, filename = paste0("plot1.png" ), units = "px",
       width = 6000, height = 6000)

# CATCHMENT JOINS TO BE REMOVED LATER
vannomrade2$numbers <- 1:96
catchmentMerging <- read.csv("data/external/catchmentMerging.csv", sep = ",")
mergedList <- do.call(rbind,lapply(unique(catchmentMerging$match), FUN = function(cm) {
  catchmentNumbers <- catchmentMerging$original[catchmentMerging$match %in% cm]
  catchmentsToMerge <- vannomrade2[vannomrade2$numbers %in% catchmentNumbers,]
  mergedCatchment <- st_sf(st_union(catchmentsToMerge))
  st_geometry(mergedCatchment) <- "x"
  mergedCatchment$navn <- paste(vannomrade2$navn[catchmentNumbers], collapse = "AND")
  mergedCatchment
}))
totalList <- rbind(vannomrade2[!(vannomrade2$numbers %in% catchmentMerging$original),-3], mergedList)


# Plot this to get an idea of how our catchment looks
areaPlot2 <- ggplot(totalList)+
  geom_sf() +
  # geom_sf_label(aes(label = navn)) +
  coord_sf(datum = st_crs(totalList)) +
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  ggrepel::geom_label_repel(
    aes(label = navn, geometry = x),
    stat = "sf_coordinates",
    min.segment.length = 0,
    label.size = NA,
    fill = NA,
    max.overlaps = Inf
  )
ggsave(areaPlot2, filename = paste0("plot2.png" ), units = "px",
       width = 6000, height = 6000)

catchmentMerging$oldName <- vannomrade2$navn[catchmentMerging$original]
catchmentMerging$newName <- vannomrade2$navn[catchmentMerging$match]
catchmentMerging$mergedName <- unlist(lapply(1:nrow(catchmentMerging), FUN = function(name) {
  catchmentsMerged <- paste0(catchmentMerging$oldName[catchmentMerging$newName %in% catchmentMerging$newName[name]], collapse = "AND")
}))

# Create new merge table
# Connect vassdragNummer to 
mergeTable1 <- nedborFelt[,c("vassdragNr", "vassDragOmrade")]
mergeTable1$navn <- vannomrade$navn[match(mergeTable1$vassDragOmrade, vannomrade$vassdrag)]
mergeTable1$newName <- catchmentMerging$mergedName[match(mergeTable1$navn, catchmentMerging$oldName)]
mergeTable1$newName <- ifelse(is.na(mergeTable1$newName), mergeTable1$navn, mergeTable1$newName)
mergeTable <- as.data.frame(st_drop_geometry(mergeTable1[,c("vassdragNr", "newName")]))

st_write(totalList, "data/external/riverNetwork/waterAreas.gpkg", append = FALSE)
saveRDS(mergeTable, "data/catchmentsWaterAreas.RDS")
# 
# ss_centroids <- st_centroid(vannomrade2)
# library(ggplot2)
# ggplot()+ 
#   geom_sf(data=vannomrade2) +
#   geom_sf_text(data = ss_centroids,aes(label=numbers))
