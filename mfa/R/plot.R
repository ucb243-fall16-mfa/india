#' plot
#'
#' Create summary plots for the result of the multiple factor analysis
#' @param x - the mfa object
#' @return a three plots with the projection of each observation onto
#'         the first two extracted components, the partial factor scores,
#'         and the variable loadings
#' @export
plot.mfa <- function(mfa){
  plot_compromise(mfa)
  plot_partial_factor(mfa)
  plot_variable_loadings(mfa)
}

# plot(mfa1)

