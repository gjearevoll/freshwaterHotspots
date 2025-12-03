
#' @title \emph{truncateValue}: This function truncates species metrics found in an sf data frame

#' @description This function is built for situations where on or two freshwater segments have extremely low or high values, and
#' end up skewing the rest of the figures, whichmakes showing variation difficult. It truncates the given column in an sf 
#' data set to the percentiles given as an argument.
#'  
#'
#' @param data An sf data frame.
#' @param column The column of the data frame that should be truncated
#' @param percentiles The percentiles to use as the truncation values
#' 
#' @return An sf data frame
#' 



# data <- catchment32633
# percentiles <- c(0.01, 0.99)
# column <- "richness"

truncateValue <- function(data, column, percentiles = c(0.01, 0.99)) {
  # Get quantiles for data
  values <- as.vector(st_drop_geometry(data[,column])[[1]])
  limits <- quantile(values, probs = percentiles)
  data[,column] <- ifelse(values < limits[1], limits[1], ifelse(values > limits[2], limits[2], values))
  
  data[,column] <- (max(st_drop_geometry(data[,column])) - st_drop_geometry(data[,column]))/  (max(st_drop_geometry(data[,column])) - min(st_drop_geometry(data[,column])))
  return(data)
}
