SETS <- list(1:6, 7:12, 13:18, 19:23, 24:29, 30:34
             , 35:38, 39:44 , 45:49, 50:53)

# Functions ---------------------------------------------------------------

file_read <- function(file_loc = 'STAT243/project/', file_name = 'wines.csv'){
  df <- read.csv(paste0(file_loc, file_name))
  dl <- list(
    df[, c('V1', 'V2', 'V3', 'V4', 'V5', 'V6')]
    , df[, c('V1.1', 'V2.1', 'V3.1', 'V4.1', 'V7', 'V8')]
    , df[, c('V1.2', 'V2.2', 'V3.2', 'V4.2', 'V9', 'V10')]
    , df[, c('V1.3', 'V2.3', 'V3.3', 'V4.3', 'V8.1')]
    , df[, c('V1.4', 'V2.4', 'V3.4', 'V4.4', 'V11', 'V12')]
    , df[, c('V1.5', 'V2.5', 'V3.5', 'V4.5', 'V13')]
    , df[, c('V1.6', 'V2.6', 'V3.6', 'V4.6')]
    , df[, c('V1.7', 'V2.7', 'V3.7', 'V4.7', 'V14', 'V5.1')]
    , df[, c('V1.8', 'V2.8', 'V3.8', 'V4.8', 'V15')]
    , df[, c('V1.9', 'V2.9', 'V3.9', 'V4.9')]
  )
  attr(dl, 'wine_names') <- as.character(df[,'ID'])
  dl
}

plot_f_scores <- function(x, title, labels = wine_names){
  plot(x[,1], x[,2], main = title)
  text(x[,1], x[,2], labels = labels) 
  abline(v = 0, h = 0)
}

scale_mfa <- function(x){
  scale(x) / sqrt(nrow(x) - 1)
}

Y <- file_read()
wine_names <- attr(Y, 'wine_names')

# METHOD 1: Division of PCA by singular value -------------------------

k_tables <- lapply(Y, function(x) {
  
  # Scale each table
  X <- scale_mfa(x)
  
  # Compute PCA on each table (d[1] is the first singular value of each table)
  d_1 <- svd(X)$d[1]
      
  # Normalize each table by dividing by it's first singular value
  X / d_1
  }
  )

# Concatenate the normalized tables
combined_table <- do.call(cbind, k_tables)

# Compute the SVD
svd_1 <- svd(combined_table)

# Calculate the F scores
F_scores_1 <- svd_1$u %*% diag(svd_1$d)

# Visualize factor scores
plot_f_scores(F_scores_1, 'Method #1: Division by Singular Value')

# METHOD 2: GSVD ------------------------------

get_combined_tables <- function(dl){
  # Take a list of matrices, center and scale each one
  dl <- lapply(dl, function(x) {
    scale_mfa(x)
  })
  do.call(cbind, dl)
}

col_weights <- function(dl){
  diag(
    unlist(
      lapply(dl, function(x) {
        # Center and scale K tables of J[k] variables collected on the 
        # same observations
        X <- scale(x) / sqrt(nrow(x) - 1)
        
        # Compute generalized PCA on each of the K tables 
        # (where d is the first singular value of each table)
        SVD <- svd(X)
        d <- SVD$d[1]
        
        # Weights for each table, one for each column
        alpha <- 1 / d^2
        rep(1, ncol(X)) * alpha
        }
      )
    )
  )
}

# Processed Data
X <- get_combined_tables(Y)

# Column weights
A <- col_weights(Y)

# Observation Weights
M <- diag(rep(1 / NROW(Y[[1]]), NROW(Y[[1]])))

# DONE: VECTOR CONTAINING THE EIGENVALUES
eigenvalues <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  U_hat <- sqrt(solve(M)) %*% svd_2$u
  svd_2$d^2
}

eigenvalues(A = A, M = M, X = X)

# DONE: MATRIX OF COMMON FACTOR SCORES
factor_scores <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  U_hat <- sqrt(solve(M)) %*% svd_2$u
  d_hat <- svd_2$d
  V_hat <- sqrt(solve(A)) %*% svd_2$v
  factor_scores <- U_hat %*% diag(d_hat)
  factor_scores[, c(1, 2)]
}

factorScores <- factor_scores(A = A, M = M, X = X)
plot_f_scores(factorScores, 'Method #2: GSVD', wine_names)

# CHECK: MATRIX OF LOADINGS (a.k.a. factor loadings).
matrix_loadings <- function(A, M, X){
  X_hat <- sqrt(M) %*% X %*% sqrt(A)
  svd_2 <- svd(X_hat)
  V_hat <- sqrt(solve(A)) %*% svd_2$v
  V_hat
}

Q <- matrix_loadings(A = A, M = M, X = X)
dim(Q)
print(Q[1:5, 1:2], digits = 3)


# IN PROGRESS: MATRIX OF PARTIAL FACTORS SCORES
get_alpha <- function(dl){
  unlist(
    lapply(dl, function(x) {
      # Center and scale K tables of J[k] variables collected on the 
      # same observations
      X <- scale(x) / sqrt(nrow(x) - 1)
      
      # Compute generalized PCA on each of the K tables 
      # (where d is the first singular value of each table)
      SVD <- svd(X)
      d <- SVD$d[1]
      
      # Weights for each table, one for each column
      1 / d^2
      }
    )
  )
}

alpha <- get_alpha(Y)
K <- 10

reshape_pfs <- function(x, dims){
  # This isn't necessary if I can make partial_factor_scores cleaner
  colnames(x) <- paste0('assessor_', 1:ncol(x))
  x <- as.data.frame(x)
  x$wine <- rep(wine_names, max(dims))
  x$dim <- sort(rep(dims, length(wine_names)))
  x <- reshape(x, idvar = c("wine", 'dim')
                , varying = list(1:ncol(x))
                , v.names = "score"
                , direction = "long")
  row.names(x) <- NULL
  x <- reshape(x, v.names = 'score', idvar = c('wine', 'time')
                , timevar = 'dim', direction = 'wide')
  names(x) <- c('wine', 'assessor', paste0('dim_', dims))
  x
}

partial_factor_scores <- function(K, alpha, X, Q, dims){
  dm <- sapply(1:length(alpha), function(k){
    res <- K * alpha[k] * X[,SETS[[k]]] %*% Q[SETS[[k]],]
    res[, dims]
  })
  dm
  # reshape_pfs(dm, dims)
}

partialFactorScores <- partial_factor_scores(10, alpha, X, Q, 1:2)

# summaries of eigenvalues  -----------------------------------------------
summary_eigenvalues <- function(evs){
  evs_sum <- matrix(NA, ncol = length(evs), nrow = 5)
  colnames(evs_sum) <- 1:length(evs)
  rownames(evs_sum) <- c('Singular Value', 'Eigenvalue', '  cumulative'
                         , '% Inertia', '  cumulative')
  
  evs_sum[1, ] <- sqrt(evs)
  evs_sum[2, ] <- evs
  evs_sum[3, ] <- cumsum(evs)
  evs_sum[4, ] <- 100 * evs_sum[2,] / sum(evs_sum[2,])
  evs_sum[5, ] <- cumsum(evs_sum[4,])
  print(evs_sum, digits = 3)
}

evs <- eigenvalues(A = A, M = M, X = X)
summary_eigenvalues(evs = evs)

# contributions -----------------------------------------------------------
# i: observation (e.g., 12)
# j: variable (e.g., 53)
# l: extracted dimension (e.g., 11)
# k: table

# j x 1 vector of weights
a <- diag(A)

ctr_i_l <- function(f, m = 1/12, dims = 1:2){
  # contribution of observation to dimension
  f <- f[,dims]
  m * f^2 / sum(m * f^2)
}

ctr_i_l(factorScores)

ctr_j_l <- function(a, loading, j_range = 1:53, l_range = 1:2){
  # contribution of variable to dimension
  # product of alpha weight[j] and loading[j,l]^2
  sapply(l_range, function(l){
    sapply(j_range, function(j){
      a[j] * loading[j, l]^2 * 1000
    })
  })
}

ctr_j_l(a, Q)

ctr_k_l <- function(alpha, loading, sets, dims = 1:2){
  # contribution of table to dimension
  # defined as the sum of the contribution of each variable from the table
  sapply(dims, function(l){
    sapply(sets, function(k){
      sum(alpha[k] * loading[k, l]^2)
    })
  })
}

ctr_k_l(a, Q, SETS)

# RV coefficient ----------------------------------------------------------
tr <- function(m){
  if(!is.matrix(m) | nrow(m) != ncol(m)) stop('m must be a square matrix')
  sum(diag(m %*% t(m)))
}

RV <- function(m1, m2){
  num <- tr(m1 %*% t(m1) %*% m2 %*% t(m2))
  denom1 <- tr(m1 %*% t(m1) %*% m1 %*% t(m1))
  denom2 <- tr(m2 %*% t(m2) %*% m2 %*% t(m2))
  num / sqrt(denom1 * denom2)
}

RV(X[,1:6], X[,7:12])

RV_table <- function(x, sets){
  sapply(sets, function(i){
    sapply(sets, function(j){
      RV(x[,i], x[,j])
    })
  })
}

RV_table(X, SETS)
