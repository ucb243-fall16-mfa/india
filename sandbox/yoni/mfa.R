#' mfa
#'
#' Perform multiple factor analysis
#' @param data - a numeric data.frame/matrix containing the data
#' @param sets - a list containing vectors of variable names/positions
#'               corresponding to each block
#' @param ncomps - number of components to report
#' @param center - logical value: should the data should be centered?
#' @param scale - logical value: should the data be scaled?
#' @return an mfa object contaning the pieces of the resulting analysis
mfa <- function(data, sets, ncomps = NULL,
                center = TRUE, scale = TRUE, ids = NULL){

    ## keep track of the number of blocks
    nGroups <- length(sets)
    
    ## extract the datasets
    Xk <- try(lapply(sets, function(g){
        as.matrix(scale(data[, g], center = center,
                        scale = scale) / sqrt(length(IDs) - 1))
    }))
    if(identical(class(Xk), "try-error")){
        stop(paste0("please check that arguments",
                    " scale and center are logical,",
                    " sets is a list of vectors containing",
                    " variable names or positions,",
                    " and that data is a numeric data.frame",
                    " or matrix."))
    }
    
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
    ret <- list(lambda = delta,
                commonFactorScores = F,
                partialFactorScores = Fk,
                Q = Q, P = P)
    class(ret) <- c("mfa", class(ret))
    attr(ret, "ncomps") <- ncomps
    attr(ret, "ids") <- ids
    ret
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
plot.mfa <- function(mfa){
    require(ggplot2)
    compromise <- as.data.frame(mfa$P)
    ids <- attributes(mfa)$ids
    colnames(compromise)[1:2] <- c("x", "y")
    if(!is.null(ids)){
        compromise$id <- ids
        ggplot(compromise, aes(x, y, label = id)) +
            geom_text()
    } else {
        ggplot(compromise, aes(x, y)) +
            geom_point()
    }
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
print.mfa <- function(mfa){
    cat(paste0("a successful mfa!"))
    cat("\n")
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
eigen_summary <- function(mfa){
    singularValues <- mfa$lambda
    eigenValues <- singularValues^2
    cumulativeLambda <- cumsum(eigenValues)
    percInertia <- 100*(singularValues^2 / sum(singularValues^2))
    cumulativeInertia <- cumsum(percInertia)
    
    rows <- list(singularValues,
                 eigenValues,
                 cumulativeLambda,
                 percInertia,
                 cumulativeInertia)

    rows <- lapply(rows, function(r){ round(r, 2)})
    d <- as.data.frame(do.call(rbind, rows))
    colnames(d) <- gsub("V", "Comp ", colnames(d))
    rownames(d) <- c("SingularValue", "Eigenvalue", "CumEigVal",
                     "PercentInertia", "CumPercIner")
    print.data.frame(d)
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
RV <- function(table1, table2){
    mix <- sum(diag((table1 %*% t(table1)) %*% (table2 %*% t(table2))))
    hom1 <- sum(diag((table1 %*% t(table1)) %*% (table1 %*% t(table1))))
    hom2 <- sum(diag((table2 %*% t(table2)) %*% (table2 %*% t(table2))))
    mix / sqrt(hom1 %*% hom2)
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
RV_table <- function(mfa, sets){
    dataset <- mfa$P %*% diag(mfa$lambda) %*% t(mfa$Q)
    rvMat <- matrix(rep(0, length(sets)^2), length(sets))
    for(i in 1:length(sets)){
        for(j in i:length(sets)){
            rvMat[i,j] <- RV(dataset[, sets[[i]]], dataset[, sets[[j]]])
            if(i != j){
                rvMat[j, i] <- rvMat[i, j]
            }
        }
    }
    rvMat
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
Lg <- function(table1, table2){
    num <- sum(diag((table1 %*% t(table1)) %*% (table2 %*% t(table2))))
    denom <- (svd(table1)$d[1])^2 * (svd(table2)$d[1])^2
    num / denom
}

#' Function Name
#'
#' Function Description
#' @param
#' @keywords
#' @export
#' @examples
Lg_table <- function(mfa, sets){
    dataset <- mfa$P %*% diag(mfa$lambda) %*% t(mfa$Q)
    lgMat <- matrix(rep(0, length(sets)^2), length(sets))
    for(i in 1:length(sets)){
        for(j in i:length(sets)){
            lgMat[i,j] <- Lg(dataset[, sets[[i]]], dataset[, sets[[j]]])
            if(i != j){
                lgMat[j, i] <- lgMat[i,j]
            }
        }
    }
    lgMat
}

