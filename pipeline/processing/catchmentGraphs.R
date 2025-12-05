
#### WATER AREA GRAPH CREATION ####

# Here we create a metric graph for each water area

# Import libraries
library(sf)
library(MetricGraph)
library(FreshwaterTools)

vannomrade <- st_read("data/external/riverNetwork/waterAreas.gpkg")

proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'
args <- commandArgs(trailingOnly = TRUE)
#omrade <- as.numeric(args[1])
omrade <- 61
focalOmrade <- vannomrade[omrade,]
omradeName <-  st_drop_geometry(vannomrade$navn[omrade])

# Load in rivers
cat("Loading river network")
riversRaw <-  read_sf("data/external/riverNetwork/Elv/Elv_Elvenett.shp")
riversRaw$vassDragOmrade <- gsub("\\..*","",riversRaw$vassdragNr)

# Get catchment to water areas tool
catchmentMatching <- readRDS("data/catchmentsWaterAreas.RDS")

# Assign name to each river
riversRaw$vannOmradeName <- catchmentMatching$newName[match(riversRaw$vassdragNr, catchmentMatching$vassdragNr)]
rivers <- riversRaw[riversRaw$vannOmradeName %in% omradeName,]

rm("riversRaw")

# Create lsn path
cat("Creating lsn path\n")
omradeName <- gsub("/","", omradeName)
lsnPath <- paste0("data/external/riverNetwork/waterAreaGraphsNew/",omradeName)
#lsnPath <- paste0("data/external/riverNetwork/processedNetwork/processedNetwork",catchmentSet)
if (!dir.exists(lsnPath)) {
  dir.create(lsnPath, recursive = TRUE)
}

catchmentNames <- unique(rivers$vassdragNr)
catchmentList <- list()
cat("Starting catchment loop\n")
# Loop through catchments and process river networks therein
for (catchment in catchmentNames) {
  riverSubset <- rivers[rivers$vassdragNr %in% catchment, c("elvID", "vassdragNr", "elvelengde")]
  riverSubset <- riverSubset$geometry
  streamCastSubset <- st_sf(st_cast(st_transform(st_zm(riverSubset),  proj), 'LINESTRING'))
  lengths <- as.numeric(st_length(st_zm(streamCastSubset)))
  streamCastSubset <- streamCastSubset[lengths >  0.001,]
  if (nrow(streamCastSubset) == 1) {
    streamCastSubset$rid <- NA
    streamCastSubset$catchment <- catchment
    catchmentList[[catchment]] <- streamCastSubset
  } else {
    
    ##Create a river structure using SSNbler::lines_to_lsn
    network = SSNbler::lines_to_lsn(streams = streamCastSubset, remove_ZM = TRUE,
                                    verbose = TRUE, lsn_path = lsnPath,
                                    overwrite = TRUE, check_topology = TRUE,
                                    snap_tolerance = 0.001, #TESTING
                                    topo_tolerance = 0) #TESTING
    network$catchment <- catchment
    catchmentList[[catchment]] <- network
  }
}

# Merge all catchments in river
cat("Merging catchments \n")
catchmentsMerged <- do.call(rbind, catchmentList)
catchmentFileName <- paste0(lsnPath, "/catchmentsMerged.gpkg")
st_write(catchmentsMerged, dsn=catchmentFileName, layer='network', append=FALSE)

cat("Creating metric graph \n")
graph <- MetricGraph::metric_graph$new(edges = catchmentsMerged, merge_close_vertices = FALSE)
cat("Building the mesh \n")
graph$build_mesh(h = 1)


graph$plot(mesh = TRUE)
# Create integration points (based on the mesh)
cat("Creating integration points \n")

cat("Setting projection \n")
IPS <- CreateIPS(graph, sf = FALSE, proj = proj, species = NULL)

#Add integration points to object.
cat("Adding integration points to the graph \n")
graph$add_observations(IPS, data_coords = 'PtE',
                       distance_on_edge = '.distance_on_edge',
                       edge_number = '.edge_number',
                       group = c('likelihood'),
                       normalized = TRUE)

rm(list=setdiff(ls(), c("graph", "IPS", "lsnPath")))

save.image(paste0(lsnPath, "/waterAreaGraph.RData"))

