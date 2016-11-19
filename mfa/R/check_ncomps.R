#' check_ncomps
#'
#' make sure ncomps has a viable value
#' @param ncomps
#' @return boolean - did the test pass
check_ncomps <- function(ncomps){
    if(!(ncomps %% 1 == 0)){
        stop("ncomps must be an integer")
    }
    TRUE
}

