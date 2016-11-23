#' print.mfa
#'
#' print function for the mfa class
#' @param mfa - mfa object to print
print.mfa <- function(mfa){
    check_mfa(mfa)
    cat('object: "mfa"\n\n')
    cat('number of tables:', length(attributes(mfa)$sets), '\n')
    cat('Truncated Eigenvalues of GSVD:',
        mfa$lambda[attributes(mfa)$ncomps], '\n')
    cat(sprintf('Variance Explained by first two eigenvalues: (%.1f, %.1f)'
                , 100*mfa$lambda[1] / sum(mfa$lambda)
                , 100*mfa$lambda[2] / sum(mfa$lambda))
        , '\n\n')
    cat('Truncated Factor Scores\n')
    print(mfa$commonFactorScores[,attributes(mfa)$ncomps])
    invisible(mfa)
}
