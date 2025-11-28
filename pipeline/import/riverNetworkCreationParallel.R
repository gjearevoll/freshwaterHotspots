
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
library(sf)

# Enable external arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 0) {
  # Set arguments
  catchmentSet <- as.integer(args[1])
  # Set the working directory
}

# Read in river data
cat("\nReading in river data\n")
rivers <-  read_sf("data/external/riverNetwork/Elv/Elv_Elvenett.shp")

# Check if catchment groups exist. If not, create them.
catchmentGroupLocation <- "data/catchmentGroups.RDS"
if (file.exists(catchmentGroupLocation)) {
  catchmentGroupings <- readRDS(catchmentGroupLocation)
} else {
  cat("\nCreate catchment groupings\n")
  catchmentGroupings <- data.frame(catchmentNumbers = unique(rivers$vassdragNr))
  catchmentGroupings$catchmentGroups <- rep(1:136, each = 164)
}

# Set projection
proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'

cat(paste0("\nProcessing catchment set ", catchmentSet, "\n"))
catchmentList <- list()
catchmentNames <- catchmentGroupings$catchmentNumbers[catchmentGroupings$catchmentGroups == catchmentSet]

# Create lsn path
lsnPath <- paste0("data/external/riverNetwork/processedNetwork/processedNetwork",catchmentSet)
if (!dir.exists(lsnPath)) {
  dir.create(lsnPath, recursive = TRUE)
}

# Loop through catchments and process river networks therein
for (catchment in catchmentNames) {
  riverSubset <- rivers[rivers$vassdragNr %in% catchment, c("elvID", "vassdragNr", "elvelengde")]
  riverSubset <- riverSubset$geometry
  streamCastSubset <- st_sf(st_cast(st_transform(st_zm(riverSubset),  proj), 'LINESTRING'))
  lengths <- as.numeric(st_length(st_zm(streamCastSubset)))
  streamCastSubset <- streamCastSubset[lengths >  0.001,]
  if (nrow(streamCastSubset) == 1) {
    streamCastSubset$rid <- NA
    catchmentList[[catchment]] <- streamCastSubset
  } else {
    
    ##Create a river structure using SSNbler::lines_to_lsn
    network = SSNbler::lines_to_lsn(streams = streamCastSubset, remove_ZM = TRUE,
                                    verbose = TRUE, lsn_path = lsnPath,
                                    overwrite = TRUE, check_topology = TRUE,
                                    snap_tolerance = 0.001, #TESTING
                                    topo_tolerance = 0) #TESTING
    catchmentList[[catchment]] <- network
  }
}

# Merge all catchments in river
catchmentsMerged <- do.call(rbind, catchmentList)
catchmentFileName <- paste0(lsnPath, "/catchmentsMergedSubset",catchmentSet,".gpkg")
st_write(catchmentsMerged, dsn=catchmentFileName, layer='network')

