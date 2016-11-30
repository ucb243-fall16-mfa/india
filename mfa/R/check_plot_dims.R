#' check_plot_dims
#'
#' make sure plot dimensions has a viable value
#' @param dims - numeric vector of dimensions
#' @param nGroups - numberic value of how many total groups there are
#' @return boolean - did the test pass
check_plot_dims <- function(nGroups, dims){
  if(length(dims) != 2){
    stop('plotting only supports two dimensions')
  }
  
  if(max(dims) > nGroups){
    stop('There are only ', nGroups,' tables in the data')
  }
  TRUE
}