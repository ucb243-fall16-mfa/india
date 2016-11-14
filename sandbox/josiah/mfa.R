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
  attr(dl, 'wine_names') <- df[,'ID']
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

# METHOD 1: Division of PCA by singular value -------------------------

data <- file_read()
wine_names <- attr(data, 'wine_names')
Y <- data

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
X <- get_combined_tables(Y)

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