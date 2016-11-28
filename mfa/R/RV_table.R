#' RV Table function
#'
#' Function to calculate the RV coefficient between sets of variables
#' @param mfa the mfa object
#' @param sets list of vectors containing the variables between which the RV
#'             coefficient
#' will be calculated
#' @keywords RV similarity measure table set
#' @export
#' @examples RV_table(data, sets = list(1:3, 4:5, 6:7))
RV_table <- function(mfa, sets = NULL){
    ## Everyone: Would you say the sets here have to be
    ## mutually exclusive? check_sets function checks for that.
    check_mfa(mfa)
    if(is.null(sets)){
        sets <- attributes(mfa)$sets
    }
    check_sets(sets)
    dataset <- mfa$commonFactorScores %*% t(mfa$Q)
    rvMat <- matrix(rep(0, length(sets)^2), length(sets))
    for(i in 1:length(sets)){
        for(j in i:length(sets)){
            rvMat[i,j] <- RV(dataset[, sets[[i]]], dataset[, sets[[j]]])
            if(i != j){
                rvMat[j, i] <- rvMat[i, j]
            }
        }
    }
    rvMat
}
