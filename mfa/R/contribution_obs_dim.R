#' contribution_obs_dim
#'
#' Calculate the contributions of an observation to a dimension
#' @param mfa - the mfa object
#' @param l_max - how many dimensions should be returned?
#' @return a matrix with the contributions of an observation to a dimension
#' @export

contribution_obs_dim <- function(mfa, l_max = 2){
  ## perform checking
  check_mfa(mfa)
  check_contribution_params(mfa, l_max)
  
  
  m <- attributes(mfa)$rowWeights
  m * mfa$commonFactorScores[, 1:l_max]^2 / 
    sum(m * mfa$commonFactorScores[, 1:l_max]^2)
}

# contribution_obs_dim(mfa1)
