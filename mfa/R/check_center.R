#' check_center
#'
#' make sure center has a viable value
#' @param center boolean
#' @return boolean - does the check pass?
check_center <- function(center){
    if(!is.logical(center)){
        stop("center must be TRUE or FALSE")
    }
    TRUE
}
