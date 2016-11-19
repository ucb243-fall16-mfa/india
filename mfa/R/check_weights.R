#' check_weights
#'
#' check that the weights variable is viable
#' @param weights - numeric vector or null
#' @return boolean - does the test pass?
check_weights <- function(weights){
    if(!is.null(weights) & sum(weights) != 1){
        stop("weights must be null or sum to 1")
    }
    TRUE
}
