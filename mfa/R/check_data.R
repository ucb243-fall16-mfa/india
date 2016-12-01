#' check_data
#'
#' private function to make sure data is entered correctly
#' @param data - the data to check
#' @param sets - the blocks the data should be broken into
#' @param ncomps - integer number of components to use
#' @param weights - numeric vector of weights
#' @param ids - character/numeric vector of sample id's
#' @return boolean - TRUE if test passes
check_data <- function(data, sets, ncomps, weights, ids){
    if(!is.data.frame(data) | is.matrix(data)){
        stop("data must be a data.frame or a matrix")
    }
    if(any(!apply(data[,unlist(sets)], 2, is.numeric))){
        stop("data must be numeric")
    }
    check_sets(sets)
    setContents <- unlist(sets)
    if(!all(setContents %in% colnames(data) | setContents %in% 1:ncol(data))){
        stop(paste0("all vectors in sets must contain either",
                    " column positions or column names of the data"))
    }
    check_ncomps(ncomps)
    if(ncomps > nrow(data)){
        stop("cannot request more components than rows in data")
    }
    check_weights(weights)
    if(!is.null(weights) & length(weights) != nrow(data)){
        stop("there must be as many weights as rows of data")
    }
    check_ids(ids)
    if(!is.null(ids) & length(ids) != nrow(data)){
        stop("there must be as many ids as rows of data")
    }
    if(!is.null(ids) & length(ids) != length(unique(ids))){
        stop("ids must be unique")
    }
    TRUE
}
