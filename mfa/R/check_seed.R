#' check_seed
#'
#' check that the seed input is valid
#' @param seed the (hopefully) numeric integer to test
#' @return boolean - does the test pass?
check_seed <- function(seed){
    if(length(seed) != 1 | seed %% 1 != 0 | seed < 0){
        stop("seed must be a positive numeric integer")
    }
    TRUE
}
