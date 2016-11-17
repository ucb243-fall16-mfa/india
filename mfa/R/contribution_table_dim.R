#' contribution_table_dim
#'
#' Calculate the contributions of a table to a dimension
#' @param x - the mfa object
#' @param l_range - how many dimensions should be returned?
#' @return a matrix with the contributions of a table to a dimension

contribution_table_dim <- function(x, ...) UseMethod('contribution_table_dim')

contribution_table_dim.mfa <- function(x, l_range = 1:2){
  
  if(is.null(x$sets) | !is.numeric(x$sets[[1]])){
    stop('mfa object must include numeric sets')
  }
  
  if(is.null(x$a)){
    stop('mfa object must include column weights')    
  }
  
  sapply(l_range, function(l){
    sapply(x$sets, function(k){
      sum(x$a[k - 1] * x$loadings[k - 1, l]^2)
    })
  })
  
}

# contribution_table_dim(mfa1)
