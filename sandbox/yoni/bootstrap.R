#' bootstrap
#'
#' Perform a boostrap approximation for the compromise of the
#' given mfa object
#' @param mfa an mfa object
#' @param B the number of bootstrap samples to run
#' @export
#' @examples
bootstrap <- function(mfa, B = 100){

    ## run the necessary checks
    check_mfa(mfa)
    check_B(B)

    ## extract all the necessary info from the mfa object
    data <- mfa$commonFactorScores %*% t(mfa$Q)
    sets <- attributes(mfa)$sets
    nSets <- length(sets)
    ids <- attributes(mfa)$ids
    ncomps <- attributes(mfa)$ncomps
    weights <- attributes(mfa)$rowWeights
    A <- diag(attributes(mfa)$colWeights)

    ## these are important for keeping track of
    ## each bootstrap sample, so make sure they're useful
    ## (not null)
    if(is.null(ids)){
        ids <- 1:nrow(data)
    }
    
    ## make a temporary boot strap function (just a segment
    ## of the mfa function)
    boot_fun <- function(sets){
        Xk <- lapply(sets, function(g){
            as.matrix(scale(data[, g], center = center,
                            scale = scale) / sqrt(nrow(data) - 1))
        })

        X <- do.call(cbind, Xk)
        S <- X %*% A %*% t(X)

        ## perform GSVD
        SVD <- svd(S)
        sqrt(solve(M)) %*% SVD$u
    }

    ## run the bootstrap
    pBoot <- lapply(1:B, function(x) NA)
    for(b in 1:B){
        i <- sample(1:nSets, nSets, replace = TRUE)
        boot <- boot_fun(sets[i])[,1:ncomps]
        boot$id <- ids
        pBoot[b] <- boot
    }
    
    do.call(rbind, pboot)
}
