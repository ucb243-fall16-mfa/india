#' check_ncomps
#'
#' make sure ncomps has a viable value
#' @param ncomps - numeric integer value number of component
#' @return boolean - did the test pass
check_ncomps <- function(ncomps){
    if(!(ncomps %% 1 == 0)){
        stop("ncomps must be an integer")
    }
    TRUE
}

