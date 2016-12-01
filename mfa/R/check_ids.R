#' check_ids
#'
#' check that the id variable is viable
#' @param ids - character/numeric vector
#' @return boolean - did the test pass?
check_ids <- function(ids){
    if(!is.null(ids) & !all(is.character(ids) | is.numeric(ids))){
        stop("ids must be null, or a character/numeric vector")
    }
    TRUE
}

