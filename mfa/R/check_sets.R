#' check_sets
#'
#' make sure sets has viable value
#' @param sets - list of numeric/character vectors
#' @return boolean - did the test pass?
check_sets <- function(sets){
    if(!all(lapply(sets, is.numeric)) | !all(lapply(sets, is.character)){
        stop(paste0("sets must either be a list containing numeric",
                    " or character vectors"))
    }
    setContents <- unlist(sets)
    if(length(unique(setContents)) != length(setContents)){
        stop("the vectors in sets must be mutually exclusive")
    }
    TRUE
}
