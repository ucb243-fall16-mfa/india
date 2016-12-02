#' check_B
#'
#' check that the B input is valid
#' @param B the (hopefully) numeric integer to test
#' @return boolean - does the test pass?
check_B <- function(B){
    if(length(B) != 1 | B %% 0 != 1 | B < 1){
        stop("B must be a positive numeric integer")
    }
    TRUE
}
