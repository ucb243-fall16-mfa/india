#' Lg
#'
#' Function to compute the Lg coefficient between two numeric tables
#' @param table1 - first numeric data.frame/matrix
#' @param table2 - second numeric data.frame/matrix
Lg <- function(table1, table2){
    table1 <- as.matrix(table1) # Might be unnecessary
    table2 <- as.matrix(table2)
    
    num <- sum(diag((table1 %*% t(table1)) %*% (table2 %*% t(table2))))
    denom <- (svd(table1)$d[1])^2 * (svd(table2)$d[1])^2
    num / denom
}
