#' check_center
#'
#' make sure center has a viable value
#' @param center boolean or numeric vector
#' @return boolean - does the check pass?
check_center <- function(center){
    if(!(is.logical(center) | is.numeric(center))){
        stop("center must be TRUE or FALSE")
    }
    TRUE
}
