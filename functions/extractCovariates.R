#' @title extractCovariates
#' @description Assign covariate information to the observation data and integration points
#' @param data A \code{metric_graph_data} object.
#' @param covariates A sf object of covariate data.
#' @param used Which spatial covariates to include in the model.
#'
#' @import MetricGraph
#' @export
#' @return A data.frame object containing the covariate information.

extractCovariates <- function(data, covariates, used = NULL) {
  
  if (!inherits(data, 'sf')) stop ('data needs to be a metric_graph_data object.')
  
  if (!inherits(covariates, 'sf')) stop ('covariates needs to be a sf object.')
  
  if (is.null(used)) used <- names(covariates)[!names(covariates) %in% attr(covariates, "sf_column")]
  else
    if (!all(used %in% names(covariates))) stop ('Some of the covariates specified in used are not in the raster stack.')
  
  NearFeature <- st_nearest_feature(x = data, y = covariates)
  
  dataCovs <- st_drop_geometry(covariates)[NearFeature,]
  
  data <- cbind(data, dataCovs)
  
  data
  
  
}
