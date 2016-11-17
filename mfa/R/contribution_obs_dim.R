#' contribution_obs_dim
#'
#' Calculate the contributions of an observation to a dimension
#' @param x - the mfa object
#' @param l_range - how many dimensions should be returned?
#' @return a matrix with the contributions of an observation to a dimension

contribution_obs_dim <- function(x, ...) UseMethod('contribution_obs_dim')

contribution_obs_dim.mfa <- function(x, l_range = 1:2){
  m <- 1/length(x$varNames)
  m * x$factorScores[, l_range]^2 / sum(m * x$factorScores[, l_range]^2)
}

# contribution_obs_dim(mfa1)