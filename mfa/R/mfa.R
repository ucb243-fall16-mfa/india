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
#' @param ids - character vector of row names
#' @return an mfa object contaning the pieces of the resulting analysis
mfa <- function(data, sets, ncomps = NULL, weights = NULL,
                center = TRUE, scale = TRUE, ids = NULL){

    ## check that all the inputs are acceptable:
    check_data(data, sets, ncomps, weights, ids)
    check_center(center)
    check_scale(scale)
    
    ## keep track of the number of blocks
    nGroups <- length(sets)
    
    ## extract the datasets
    Xk <- lapply(sets, function(g){
        as.matrix(scale(data[, g], center = center,
                        scale = scale) / sqrt(length(IDs) - 1))
    })

    ## create mass matrix
    if(is.null(weights)){
        weights <- rep(1/nGroups, nGroups)
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

    ## calculate common factor score and loading matrices
    Q <- t(X) %*% M %*% P %*% solve(diag(delta))
    F <- S %*% M %*% P %*% solve(diag(delta))

    ## calculate partial factor score matrices
    Fk <- lapply(1:nGroups, function(i){
        nGroups * unique(a)[i] * Xk[[i]] %*% Q[sets[[i]],]
    })

    ## collect results and return list with class "mfa"
    ret <- list(lambda = delta^2,
                commonFactorScores = F,
                partialFactorScores = Fk,
                Q = Q, P = P)
    class(ret) <- c("mfa", class(ret))
    attr(ret, "ncomps") <- ncomps
    attr(ret, "ids") <- ids
    ret
}
