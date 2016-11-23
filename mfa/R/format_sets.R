#' format_sets
#'
#' private function to put the sets variable into a standard format
#' @param sets - the blocks the data should be broken into
#' @param nGroups - The number of groups of variables (i.e., number of tables)
#' @return sets - sets as a list, adjusted to be in standard format


format_sets <- function(sets, nGroups){
  
  ## Adjust set index if there are df variables before the first active var.
  if(is.numeric(sets[[1]])){
    adjust_amt <- sets[[1]][1] - 1
    sets <- lapply(1:nGroups, function(k){
      sets[[k]] - adjust_amt
    })
    return(sets)
  }
  
  ## Convert set index to numeric if it is a character
  if(is.character(sets[[1]])){
    sets <- lapply(1:nGroups, function(k){
        shift <- sum(sapply(sets[1:k], function(k_2){length(k_2)})) -
            length(sets[[k]])
        1:length(sets[[k]]) + shift
    })
    return(sets)
  }
}
