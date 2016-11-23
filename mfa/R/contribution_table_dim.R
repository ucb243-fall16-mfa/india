#' contribution_table_dim
#'
#' Calculate the contributions of a table to a dimension
#' @param x - the mfa object
#' @param l_range - how many dimensions should be returned?
#' @return a matrix with the contributions of a table to a dimension

contribution_table_dim <- function(x, ...) UseMethod('contribution_table_dim')

contribution_table_dim.mfa <- function(mfa, l_range = 2){
    check_contribution_params(mfa, l_range)
    sets <- attributes(mfa)$sets
    a <- attributes(mfa)$colWeights
    sapply(1:l_range, function(l){
        sapply(sets, function(k){
            sum(a[k] * mfa$Q[k, l]^2)
        })
    })
}

# contribution_table_dim(mfa1)
