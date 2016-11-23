#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
check_contribution_params <- function(mfa, l_range){
    if(class(mfa) != "mfa"){
        stop("mfa must be an object of class mfa")
    }
    
    if(is.null(attributes(mfa)$sets) |
       !all(is.numeric(attributes(mfa)$sets))){
        stop('mfa object must include numeric sets')
    }
  
    if(is.null(attributes(mfa)$colWeights)){
        stop('mfa object must include column weights')    
    }

    if(l_range %% 1 != 0){
        stop("l_range must be an integer")
    }

    if(l_range > ncol(mfa$Q) | l_range < 1){
        stop(paste0("l_range must be greater than 0 ",
                    "and no greater than the number columns in Q"))
    }

    TRUE
}
