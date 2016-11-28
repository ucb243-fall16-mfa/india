#' summary_eigenvalues
#'
#' provide a print-out summary of the eigenvalues of an mfa object
#' @param mfa - the mfa object to summarize
summary_eigenvalues <- function(mfa){
    ## create the values:
    eigenValues <- mfa$lambda
    singularValues <- sqrt(eigenValues)
    cumulativeLambda <- cumsum(eigenValues)
    percInertia <- 100*(eigenValues / sum(eigenValues))
    cumulativeInertia <- cumsum(percInertia)
    ## make them into rows
    rows <- list(singularValues,
                 eigenValues,
                 cumulativeLambda,
                 percInertia,
                 cumulativeInertia)
    ## round, combine into a data.frame and print
    rows <- lapply(rows, function(r){ round(r, 2)})
    d <- as.data.frame(do.call(rbind, rows))
    colnames(d) <- gsub("V", "Comp ", colnames(d))
    rownames(d) <- c("SingularValue", "Eigenvalue", "CumEigVal",
                     "PercentInertia", "CumPercIner")
    print.data.frame(d)
}
