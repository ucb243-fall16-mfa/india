#' contribution_var_dim
#'
#' Calculate the contributions of a variable to a dimension
#' All contributions are scaled by multiplying by 1000
#' @param x - the mfa object
#' @param l_range - how many dimensions should be returned?
#' @return a matrix with the contributions of a variable to a dimension

contribution_var_dim <- function(x, ...) UseMethod('contribution_var_dim')

contribution_var_dim.mfa <- function(x, l_range = 1:2){
  j_range <- 1:nrow(x$loadings)
  sapply(l_range, function(l){
    sapply(j_range, function(j){
      x$a[j] * x$loadings[j, l]^2 * 1000
    })
  })
}

# contribution_var_dim(mfa1)
