
#' @title \emph{get_worldclim}: This function downloads and merges CORINE land cover data for a study area

#' @description This function unzips and constructs a raster of land cover data. It requires the zip file with CORINE data to already be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018 and saved as corineRaw in the data/external/environmentalCovariates folder. 
#'
#' @param zip_path folder where downloaded CORINE zip file is stored
#' @param output_path path for where to save downloaded data (with default = NA, data is downloaded into temporary folder that is later deleted).
#'
#' 
#' @return A SpatRaster of WorldClim layers.
library(terra)

get_corine <- function(data_path = NA, output_path = NA, reclassify = TRUE, year = NA) {
  
  if(is.na(data_path)){
    message("'data_path' not specified, please select CORINE land cover folder. Data can be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018.")
    data_path <- file.choose()
  }
  
  if(!file.exists(data_path)) {
    stop("CORINE land cover data cannot be found. Data can be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018.")
  }
  
  availableYears <- list.dirs(data_path, recursive = FALSE, full.names = F)
  if (is.na(year)) {
    year <- max(as.integer(availableYears))
  }
  
  corineFolder <- paste0(data_path, "/", year)
    
  
  # by country (tested on norway)
  if("Info" %in% list.dirs(corineFolder, F, F)){
    # load
    corine <- rast(list.files(temp_wd, ".tif$", full.names = T))
    
    # read raster legend information and relabel values
    legend_path <- list.files(file.path(temp_wd, "Info/Legend", "Raster"), ".txt$", full.names = T,recursive = T)
    legend <- read.delim(legend_path, sep =  ",", header = FALSE)
    values(corine) <- legend$V6[values(corine)[,1]]
  } else {
    corine_path <- list.files(file.path(corineFolder, "DATA"), "\\.tif$", full.names = T)
    # load raster
    corine <- rast(corine_path)
    
    # remove commas from all category labels 
    levels(corine)[[1]][,2] <- gsub(",", "", levels(corine)[[1]][,2])
  }
  
  
  # If we want, we can now reclassifiy CORINE's layers. We do this using a csv file name "corineReclassification" that should be
  # uploaded to the same data/temp/corine folder you are storing corine rasters. The file should have two columns, one names 
  # corineCategory (with existing corine categories) and the second (with your reclassifications) called newCategory.
  if(!file.exists("data/external/covariates/corine/corineReclassification.csv")) {
    warning("No CORINE reclassification system has been provided. Using all 52 potential categories.")
  } else if (reclassify == TRUE) {
    corineReclassification <- read.csv("data/external/covariates/corine/corineReclassification.csv", header = TRUE)
    reclassTable <- levels(corine)[[1]]
    names(reclassTable) <- c("value", "label")
    reclassTable$newLabel <- corineReclassification$newCategory[match(reclassTable$label, corineReclassification$corineCategory)]
    reclassTable <- reclassTable[,c("value", "newLabel")] %>% rename(label = newLabel)
    levels(corine)[[1]][,2] <- reclassTable[,2]
  }
  
  
  if(!is.na(output_path)) {
    writeRaster(newRaster, file.path(output_path, "corine.tif"))
  }
  
  out <- corine
  return(out)
}


