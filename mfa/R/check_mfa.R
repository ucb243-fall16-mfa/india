#' check_mfa
#'
#' check function to make sure an mfa object is valid
#' @param mfa - a mfa object
check_mfa <- function(mfa){
    if(!("mfa" %in% class(mfa))){
        stop("mfa must be an object of class mfa")
    }
    if(!all(c("lambda", "commonFactorScores",
              "partialFactorScores", "Q", "P") %in% names(mfa))){
        stop("given mfa object does not have correct contents")
    }
    TRUE
}
