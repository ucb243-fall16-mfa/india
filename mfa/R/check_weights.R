#' check_weights
#'
#' check that the weights variable is viable
#' @param weights - numeric vector or null
#' @return boolean - does the test pass?
check_weights <- function(weights){
    if(!is.null(weights) & (sum(weights) != 1 | !all(weights > 0))){
        stop("weights must be null or positive numbers that sum to 1")
    }
    TRUE
}
