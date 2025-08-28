
##Create river network
library(SSN2) 
library(SSNbler)
library(sf)

rivers <-  read_sf("data/NVEKartdata/NVEKartdata/NVEData/Elv/Elv_Elvenett.shp")

proj <- '+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9996 +datum=WGS84 +units=km +x_0=500 +y_0=0 +no_defs'

catchmentList <- list()
catchmentNames <- unique(rivers$vassdragNr)

for (catchment in catchmentNames) {
  riverSubset <- rivers[rivers$vassdragNr == catchment, c("elvID", "vassdragNr", "elvelengde")]
  streamCastSubset <- st_sf(st_cast(st_transform(st_zm(riverSubset),  proj), 'LINESTRING'))
  if (nrow(streamCastSubset) == 1) {next}
  lengths <- as.numeric(st_length(st_zm(streamCastSubset)))
  streamCastSubset <- streamCastSubset[lengths >  0.001,]
  #streamCastNoShort <- streamCast[unclass(st_length(streamCast)) > 0.01,]
  
  
  ##Create a river structure using SSNbler::lines_to_lsn
  network = SSNbler::lines_to_lsn(streams = streamCastSubset, remove_ZM = TRUE,
                                  verbose = TRUE, lsn_path = "data/NVEKartdata/NVEData/processedNetwork",
                                  overwrite = TRUE, check_topology = TRUE,
                                  snap_tolerance = 0.001, #TESTING
                                  topo_tolerance = 0) #TESTING
  catchmentList[[catchment]] <- network
}
catchmentsMerged <- do.call(rbind, catchmentList)

st_write(catchmentsMerged, dsn="data/external/riverNetwork/catchmentsMerged.gpkg", layer='network')

read_sf("data/external/riverNetwork/catchmentsMerged.gpkg")

