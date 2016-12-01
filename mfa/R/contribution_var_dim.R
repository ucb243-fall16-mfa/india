#' contribution_var_dim
#'
#' Calculate the contributions of a variable to a dimension
#' All contributions are scaled by multiplying by 1000
#' @param mfa - the mfa object
#' @param l_max - how many dimensions should be returned?
#' @return a matrix with the contributions of a variable to a dimension
#' @export

contribution_var_dim <- function(mfa, l_max = 2){
  
  ## perform checking
  check_mfa(mfa)
  check_contribution_params(mfa, l_max)
  
  ## compute contributions
  j_range <- 1:nrow(mfa$Q)
  sapply(1:l_max, function(l){
      sapply(j_range, function(j){
          attributes(mfa)$colWeights[j] * mfa$Q[j, l]^2 * 1000
      })
  })
}

# contribution_var_dim(mfa1)
