file_read <- function(file_loc = 'STAT243/project/', file_name = 'wines.csv'){
  df <- read.csv(paste0(file_loc, file_name))
  list(
    df[, c('ID')]
    , df[, c('V1', 'V2', 'V3', 'V4', 'V5', 'V6')]
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
}

plot_f_scores <- function(x, title, labels = wine_names){
  plot(x[,1], x[,2], main = title)
  text(x[,1], x[,2], labels = labels) 
  abline(v = 0, h = 0)
}

scale_mfa <- function(x){
  scale(x) / sqrt(nrow(x) - 1)
}

data <- file_read()
wine_names <- data[[1]]
Y <- data[2:length(data)]

# Try a Single Person -------------------------------
Y_1 <- Y[[1]]
X_1 <- scale(Y_1) / sqrt(nrow(Y_1) - 1)
svd_single <- svd(X_1)

# U: left singular vectors of X
U <- svd_single$u

# Singular values
d <- diag(svd_single$d)

# V: right singular vectors of X
V <- svd_single$v

# G: Factor scores
G <- U %*% diag(d)

# alpha: Weights for each table
alpha <- 1 / d[1]^2

# a/A: Weights for each table, one for each column
a <- rep(1, ncol(X_1)) * alpha
A <- diag(a)

# m/M: Weights for each observation
m <- rep(1 / nrow(X_1), nrow(X_1))
M <- diag(m)

# METHOD 1: Division of PCA by singular value -------------------------

result <- lapply(Y, function(x) {
  
  # Step 1: K tables of J[k] variables collected on the 
  # same observations
  X <- scale(x) / sqrt(nrow(x) - 1)
  
  # Step 2: Compute generalized PCA on each of the K tables 
  # (where d is the first singular value of each table)
  SVD <- svd(X)
  d <- SVD$d[1]
      
  # Step 3: Normalize each table by dividing by its 
  # first singular value
  X / d
  }
  )

# Step 4: Concatenate the K normalized tables
result <- do.call(cbind, result)

# Compute the simple SVD
svd_1 <- svd(result)
F_scores_1 <- svd_1$u %*% diag(svd_1$d)

# Visualize factor scores
plot_f_scores(F_scores_1, 'Method #1: Division by Singular Value')

# METHOD 2: GSVD ------------------------------

center_scale <- function(dl, dtype = 'matrix'){
  # Take a list of matrices, center and scale each one
  # Returns a matrix or list
  dl <- lapply(dl, function(x) {
    scale_mfa(x)
  })
  if(dtype == 'matrix'){
    do.call(cbind, dl)
  } else if (dtype == 'list') {
    dl
  }
}

col_weights <- function(dl){
  diag(
    unlist(
      lapply(dl, function(x) {
        # Center ans scale K tables of J[k] variables collected on the 
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
X <- center_scale(Y, 'matrix')

# Column weights
A <- col_weights(Y)

# Observation Weights
M <- diag(rep(1 / NROW(Y[[1]]), NROW(Y[[1]])))

# Compute the SVD of the weighted data
X_hat <- sqrt(M) %*% X %*% sqrt(A)
svd_2 <- svd(X_hat)
U_hat <- sqrt(solve(M)) %*% svd_2$u
d_hat <- svd_2$d
V_hat <- sqrt(solve(A)) %*% svd_2$v

# Compute and visualize factor scores
F_scores_2 <- U_hat %*% diag(d_hat)
F_scores_2[, c(1, 2)]

plot_f_scores(F_scores_2, 'Method #2: GSVD', wine_names)

