#' plot
#'
#' Create summary plots for the result of the multiple factor analysis
#' @param x - the mfa object
#' @return a three plots with the projection of each observation onto the first two
#' extracted components, the partial factor scores, and the variable loadings

plot <- function(x, ...) UseMethod('plot')

plot.mfa <- function(x){
  plot_compromise(x)
  plot_partial_factor(x)
  plot_variable_loadings(x)
}

# plot(mfa1)

