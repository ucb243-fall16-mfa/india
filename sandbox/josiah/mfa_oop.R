rm(list = ls())
SETS_1 <- list(
  2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45 , 46:50, 51:54
  )

SETS_2 <- list(
  c('V1', 'V2', 'V3', 'V4', 'V5', 'V6')
  , c('V1.1', 'V2.1', 'V3.1', 'V4.1', 'V7', 'V8')
  , c('V1.2', 'V2.2', 'V3.2', 'V4.2', 'V9', 'V10')
  , c('V1.3', 'V2.3', 'V3.3', 'V4.3', 'V8.1')
  , c('V1.4', 'V2.4', 'V3.4', 'V4.4', 'V11', 'V12')
  , c('V1.5', 'V2.5', 'V3.5', 'V4.5', 'V13')
  , c('V1.6', 'V2.6', 'V3.6', 'V4.6')
  , c('V1.7', 'V2.7', 'V3.7', 'V4.7', 'V14', 'V5.1')
  , c('V1.8', 'V2.8', 'V3.8', 'V4.8', 'V15')
  , c('V1.9', 'V2.9', 'V3.9', 'V4.9')
  )

file_read <- function(file_loc = 'STAT243/project/', file_name = 'wines.csv'){
  df <- read.csv(paste0(file_loc, file_name))
}

scale_mfa <- function(x){
  scale(x) / sqrt(nrow(x) - 1)
}

scaled_tables <- function(dl){
  dl <- lapply(dl, function(x) {
    scale_mfa(x)
  })
  do.call(cbind, dl)
}

col_weights <- function(dl){
  diag(
    unlist(
      lapply(dl, function(x) {
        X <- scale(x) / sqrt(nrow(x) - 1)
        SVD <- svd(X)
        d <- SVD$d[1]
        alpha <- 1 / d^2
        rep(1, ncol(X)) * alpha
      }
      )
    )
  )
}

get_alpha <- function(dl){
  unlist(
    lapply(dl, function(x) {
      X <- scale(x) / sqrt(nrow(x) - 1)
      SVD <- svd(X)
      d <- SVD$d[1]
      1 / d^2
    }
    )
  )
}

matrix_loadings <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  V_hat <- sqrt(solve(A)) %*% svd_2$v
  V_hat
}

eigenvalues <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  U_hat <- sqrt(solve(M)) %*% svd_2$u
  svd_2$d^2
}

factor_scores <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  U_hat <- sqrt(solve(M)) %*% svd_2$u
  d_hat <- svd_2$d
  V_hat <- sqrt(solve(A)) %*% svd_2$v
  factor_scores <- U_hat %*% diag(d_hat)
  factor_scores[, c(1, 2)]
}

get_alpha <- function(dl){
  unlist(
    lapply(dl, function(x) {
      X <- scale(x) / sqrt(nrow(x) - 1)
      SVD <- svd(X)
      d <- SVD$d[1]
      1 / d^2
    }
    )
  )
}

partial_factor_scores <- function(K, alpha, X, Q){
  lapply(1:length(alpha), function(k){
    res <- K * alpha[k] * X[,SETS_1[[k]] - 1] %*% Q[SETS_1[[k]] - 1,]
  })
}

mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  if(!is.data.frame(data) & !is.matrix(data)){
    stop('Data must be either dataframe or matrix')
  }
  
  # Create the untransformed dataset
  Y <- lapply(sets, function(x) {
    data[,x]
  })
  
  # Create transformed dataset
  X = scaled_tables(Y)
  
  # Get the alpha values
  alpha <- get_alpha(Y)
  
  # Create column weight diagonal matrix
  A <- col_weights(Y)
  
  # Create diagonal matrix of observation weights
  M <-  diag(rep(1 / NROW(Y[[1]]), NROW(Y[[1]])))
  
  # Get the matrix loadings
  Q <- matrix_loadings(A = A, M = M, X = X)
  
  object <- list(
    eigenValues = eigenvalues(A = A, M = M, X = X)
    , factorScores = factor_scores(A = A, M = M, X = X)
    , partialFactorScores = partial_factor_scores(10, alpha, X, Q)
    , loadings = Q
    , sets = sets
    , a = diag(A)
  )
  
  attr(object, "ids") <- as.character(data[,'ID'])
  class(object) <- 'mfa'
  object
}

print.mfa <- function(x){
  cat('object: "mfa"\n\n')
  cat('First Two Eigenvalues:', x$eigenValues[1:2], '\n')
  cat(sprintf('Variance Explained by first two eigenvalues: (%.1f, %.1f)'
              , 100*x$eigenValues[1] / sum(x$eigenValues)
              , 100*x$eigenValues[2] / sum(x$eigenValues))
      , '\n\n')
  cat('First Two Factor Scores\n')
  print(x$factorScores[,1:2])
  invisible(x)
}

plot_factor.mfa <- function(x, title = 'Factor Scores'){
  plot(x$factorScores[,1], x$factorScores[,2]
       , main = title, xlab = 'First Factor', ylab = 'Second Factor')
  text(x$factorScores[,1], x$factorScores[,2], labels = x$varNames) 
  abline(v = 0, h = 0)
}

# Summaries of Eigenvalues ------------------------------------------------
summary_eigenvalues <- function(x, ...) UseMethod('summary_eigenvalues')
summary_eigenvalues.mfa <- function(x){
  evs_sum <- matrix(NA, ncol = length(x$eigenValues), nrow = 5)
  colnames(evs_sum) <- 1:length(x$eigenValues)
  rownames(evs_sum) <- c('Singular Value', 'Eigenvalue', '  cumulative'
                         , '% Inertia', '  cumulative')
  
  evs_sum[1, ] <- sqrt(x$eigenValues)
  evs_sum[2, ] <- x$eigenValues
  evs_sum[3, ] <- cumsum(x$eigenValues)
  evs_sum[4, ] <- 100 * evs_sum[2,] / sum(evs_sum[2,])
  evs_sum[5, ] <- cumsum(evs_sum[4,])
  print(evs_sum, digits = 3)
}


# Contributions -----------------------------------------------------------
ctr_obs_dim <- function(x, ...) UseMethod('ctr_obs_dim')
ctr_obs_dim.mfa <- function(x, l_range = 1:2){
  # contribution of observation to dimension
  m <- 1/length(attr(x, 'ids'))
  m * x$factorScores[, l_range]^2 / sum(m * x$factorScores[, l_range]^2)
}

ctr_var_dim <- function(x, ...) UseMethod('ctr_var_dim')
ctr_var_dim.mfa <- function(x, l_range = 1:2){
  # contribution of variable to dimension
  # product of alpha weight[j] and loading[j,l]^2
  
  j_range <- 1:nrow(x$loadings)
  sapply(l_range, function(l){
    sapply(j_range, function(j){
      x$a[j] * x$loadings[j, l]^2 * 1000
    })
  })
}

ctr_table_dim <- function(x, ...) UseMethod('ctr_table_dim')
ctr_table_dim.mfa <- function(x, l_range = 1:2){
  # contribution of table to dimension
  # defined as the sum of the contribution of each variable from the table
  sapply(l_range, function(l){
    sapply(x$sets, function(k){
      sum(x$a[k - 1] * x$loadings[k - 1, l]^2)
    })
  })
}

# RV Coefficient ----------------------------------------------------------




# Run Everything ----------------------------------------------------------
wine <- file_read()
mfa1 <- mfa(wine, SETS_1)
mfa1
summary_eigenvalues(mfa1)

ctr_obs_dim(mfa1)
ctr_var_dim(mfa1)
ctr_table_dim(mfa1)

dim(mfa1$loadings)
mfa1$a
str(mfa1)
