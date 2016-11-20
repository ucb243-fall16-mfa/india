#' check_scale
#'
#' make sure scale has a viable value
#' @param scale boolean
#' @return boolean - does the check pass?
check_scale <- function(scale){
    if(!is.logical(scale)){
        stop("scale must be TRUE or FALSE")
    }
    TRUE
}
