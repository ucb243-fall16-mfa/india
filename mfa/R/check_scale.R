#' check_scale
#'
#' make sure scale has a viable value
#' @param scale boolean or numeric vector
#' @return boolean - does the check pass?
check_scale <- function(scale){
    if(!(is.logical(scale) | is.numeric(scale))){
        stop("scale must be TRUE or FALSE")
    }
    TRUE
}
