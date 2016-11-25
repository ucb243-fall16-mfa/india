#' check_contribution_params
#'
#' make sure the inputs to contribution_table_dim are
#' viable
#' @param mfa - a valid mfa object
#' @param l_max - numeric integer
check_contribution_params <- function(mfa, l_max){
    check_mfa(mfa)
    
    if(is.null(attributes(mfa)$sets) |
       !all(sapply(attributes(mfa)$sets, is.numeric))){
        stop('mfa object must include numeric sets')
    }
  
    if(is.null(attributes(mfa)$colWeights)){
        stop('mfa object must include column weights')    
    }

    if(l_max %% 1 != 0){
        stop("l_max must be an integer")
    }

    if(l_max > ncol(mfa$Q) | l_max < 1){
        stop(paste0("l_max must be greater than 0 ",
                    "and no greater than the number columns in Q"))
    }

    TRUE
}
