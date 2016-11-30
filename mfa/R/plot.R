#' plot
#'
#' Create summary plots for the result of the multiple factor analysis
#' @param mfa - the mfa object
#' @return a three plots with the projection of each observation onto
#'         the first two extracted components, the partial factor scores,
#'         and the variable loadings
#' @export
plot.mfa <- function(mfa, dims = 1:2){
  plot_compromise(mfa, dims)
  plot_partial_factor(mfa, dims)
  plot_variable_loadings(mfa, dims)
}

# plot(mfa1)