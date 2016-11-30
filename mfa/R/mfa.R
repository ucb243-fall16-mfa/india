#' mfa
#'
#' Perform multiple factor analysis
#' @param data - a numeric data.frame/matrix containing the data
#' @param sets - a list containing vectors of variable names/positions
#'               corresponding to each block
#' @param ncomps - number of components to report
#' @param weights -
#' @param center - logical value: should the data should be centered?
#' @param scale - logical value: should the data be scaled?
#' @param ids - optional character vector of row names
#' @param color - optional character vector for mapping a discrete color scale
#' @return an mfa object contaning the pieces of the resulting analysis
#' @export
mfa <- function(data, sets, ncomps = NULL, weights = NULL,
                center = TRUE, scale = TRUE, ids = NULL
                , color = NULL){
  
    ## check that all the inputs are acceptable:
    check_data(data, sets, ncomps, weights, ids)
    check_center(center)
    check_scale(scale)
    
    ## get variable names
    if(any(is.character(sets[[1]]))){
      # Use the sets variable if it's column name
      var_names <- lapply(sets, function(k){
        gsub('\\.[0-9]+', '', k) # This removes any numbers from duplicate names
      })
    }else if (is.data.frame(data) | !is.null(colnames(data))){
      # get the variable names from the data if it has column names
      var_names <- lapply(sets, function(k){
        gsub('\\.[0-9]+', '', colnames(data)[k])
      })
    }else{
      var_names <- NULL
    }
    
    ## keep track of the number of blocks
    nGroups <- length(sets)

    ## extract the datasets
    Xk <- lapply(sets, function(g){
        as.matrix(scale(data[, g], center = center,
                        scale = scale) / sqrt(nrow(data) - 1))
    })

    ## create mass matrix
    if(is.null(weights)){
        weights <- rep(1/dim(data)[1], dim(data)[1])
    } 
    M <- diag(weights)
    
    ## calculate vector of weights
    a <- unlist(lapply(Xk, function(d){
        SVD <- svd(d)
        rep((SVD$d[1])^(-2), dim(d)[2])
    }))

    ## create weight matrix
    A <- diag(a)

    ## prepare matrix for GSVD
    X <- do.call(cbind, Xk)
    S <- X %*% A %*% t(X)

    ## perform GSVD
    SVD <- svd(S)
    P <- sqrt(solve(M)) %*% SVD$u
    delta <- sqrt(diag(solve(P) %*% S %*% solve(t(P))))
    
    ## put sets variable into standard format
    sets <- format_sets(sets, nGroups)
    
    ## calculate common factor score and loading matrices
    Q <- t(X) %*% M %*% P %*% solve(diag(delta))
    Fs <- S %*% M %*% P %*% solve(diag(delta))

    ## calculate partial factor score matrices
    Fk <- lapply(1:nGroups, function(i){
        nGroups * unique(a)[i] * Xk[[i]] %*% Q[sets[[i]],]
    })
    
    ## collect results and return list with class "mfa"
    ret <- list(lambda = delta^2,
                commonFactorScores = Fs,
                partialFactorScores = Fk,
                Q = Q, P = P)
    class(ret) <- c("mfa", class(ret))
    attr(ret, "ncomps") <- ncomps
    attr(ret, "ids") <- ids
    attr(ret, "sets") <- sets
    attr(ret, "colWeights") <- a
    attr(ret, "rowWeights") <- weights
    attr(ret, "var_names") <- var_names
    attr(ret, "color") <- color
    ret
}
