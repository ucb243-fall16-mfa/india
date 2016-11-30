#' contribution_table_dim
#'
#' Calculate the contributions of a table to a dimension
#' @param mfa - the mfa object
#' @param l_max - how many dimensions should be returned?
#' @return a matrix with the contributions of a table to a dimension
#' @export

contribution_table_dim <- function(mfa, l_max = 2){
    check_contribution_params(mfa, l_max)
    sets <- attributes(mfa)$sets
    a <- attributes(mfa)$colWeights
    sapply(1:l_max, function(l){
        sapply(sets, function(k){
            sum(a[k] * mfa$Q[k, l]^2)
        })
    })
}
