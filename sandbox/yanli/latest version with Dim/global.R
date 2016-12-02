## install the package from github. If installed locally, sometimes 
## there would be error using some functions.
devtools::install_github("fussballball/stat243FinalProject/mfa", 
                         force_deps = FALSE)

library(shiny)
library(mfa)

#' LoadToEnvironment
#'
#' load the file of saved mfa object into the environment
#' @param RData - the file path
#' @param env - the destination environment, default value is a new environment.
#' @return a new envrionemt with the mfa object in it.
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

#' check_mfa
#'
#' check function to make sure an mfa object is valid
#' @param mfa - a (mfa) object
#' @return if the object is an mfa object, return TRUE, otherwise, return FALSE.
check_mfa <- function(mfa){
  if(!("mfa" %in% class(mfa))){
    return(FALSE)
  }
  if(!all(c("lambda", "commonFactorScores",
            "partialFactorScores", "Q", "P") %in% names(mfa))){
    return(FALSE)
  }
  TRUE
}



