#' plot
#'
#' Create summary plots for the result of the multiple factor analysis
#' @param x - the mfa object
#' @param color - optional vector of equal length to the rows of the original data
#' @return a three plots with the projection of each observation onto the first two
#' extracted components, the partial factor scores, and the variable loadings

plot <- function(x, ...) UseMethod('plot')

plot.mfa <- function(x, color = NULL){
  plot_compromise(x, color)
  plot_partial_factor(x, color)
  plot_variable_loadings(x)
}

# plot(mfa1, color = substr(wine$ID, 1, 2))