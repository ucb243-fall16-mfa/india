#implements the OOP approach
sets <- c(1,2)
pat <- paste("V",sets,"($|\\.)",sep ="")

nam <- colnames(data)

grep(pattern = pat[2], x = nam)

#select the chosen variables:
datTemp <- lapply(pat, FUN = function(y){
  num <- grep(pattern = y,x = colnames(data))
  ret <- data[,num]
  return(ret)
})

is.numeric(c("V1","V2"))


do.call(cbind, datTemp)

#dataSets is from my mfa.R file
sets <- lapply(dataSets, function(x)colnames(x))

mfa = function(data, sets = NULL, ncomps = NULL, center = TRUE, scale = TRUE){
  MFA <- NULL
  #class(MFA) = "mfa"
  Y <- lapply(sets, function(x){
    return(data[,x])
  })
  #implement the set methods
  scale_mfa <- function(x){
    #this only works the way we planned it to when center = TRUE and scale = TRUE?
    scale(x, center = center, scale = scale) / sqrt(nrow(x) - 1)
  }

  #gives the right factor scores, but for some reason the eigenvalues and so on are incorrect
  XTilde <- getResult(Y)

  X <- XTilde %*% diag(1/sqrt(alpha))
  #make the SVD of the Matrix
  SVD <- svd(XTilde)
  SVDTemp <- svd(X)
  SVD$d
  #get the factor scores
  MFA$factor_scores <- SVD$u %*% diag(SVD$d)

  #get the matrix Q
  #Important note for me: A^-.5 is only 1/sqrt(A) because A is a diagional matrix.
  #in this case the inverse is just 1/each element and the root is just the root of
  #each element
  alpha <- getAlpha(Y)
  A <- diag(alpha)
  alpha
  #Calculate Q
  Q <- t(SVD$v) %*% diag(1/sqrt(alpha))
  #get the eigenvalues. Not Yet working.
  MFA$eigen <- sqrt(SVD$d)
  #this function is not yet working
  MFA$partial_scores <- getPartialFactorScores(Y,Q = Q)



  return(MFA)
}


getResult = function(Y){
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
return(result)
}

getAlpha <- function(Y){
  temp <- lapply(Y, FUN = function(x){
    x <- scale(x) / sqrt(nrow(x) - 1)
    svdTemp <- svd(x)
    alphaTemp <- 1/(svdTemp$d[1]^2)
    return(rep(alpha, ncol(x)))
  })
  res <- do.call(c, temp)
  return(res)
}

getPartialFactorScores = function(Y,Q){
  K <- length(Y)
  ret <- lapply(Y, FUN = function(x){
    x <- scale(x) / sqrt(nrow(x) - 1)
    svdTemp <- svd(x)
    gammaTemp <- svdTemp$d[1]
    alphaTemp <- gammaTemp^(-2)

    #the mistake is in one of these lines...
    cols <- match(colnames(x), colnames(data))
    res <- K*alphaTemp*x %*% t(Q[,cols])
    return(res)
  })
  return(ret)
}

summaryEigenvalues.mfa = function(x){
  return(summary(x$eigen))
}
