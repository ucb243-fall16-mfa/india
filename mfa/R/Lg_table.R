#' Lg_table
#'
#' computes a matrix of Lg scores for all the tables used in an
#' multi-factor analysis.
#' @param mfa - the mfa object containing the data
#' @param sets - a numeric list indicating the groups of columns
#'               that correspond to each table
#' @export
Lg_table <- function(mfa, sets){
    check_sets(sets)
    dataset <- mfa$commonFactorScores %*% t(mfa$Q)
    lgMat <- matrix(rep(0, length(sets)^2), length(sets))
    for(i in 1:length(sets)){
        for(j in i:length(sets)){
            lgMat[i,j] <- Lg(dataset[, sets[[i]]], dataset[, sets[[j]]])
            if(i != j){
                lgMat[j, i] <- lgMat[i,j]
            }
        }
    }
    lgMat
}
