#' RV function
#'
#' Calculates RV coefficient for 2 tables, thereby returns a measure of
#' similarity
#' @param table1 the first table
#' @param table2 the second table
#' @return returns the RV coefficient, a measure of similarity for the 2 tables
#' @keywords RV similarity measure
#' @examples
#' table1 <- matrix(c(1,2,3,4), nrow = 2)
#' table2 <- matrix(c(9,5,2,8), nrow = 2)
#' RV(table1,table2)
#' @export

RV = function(table1, table2){
  table1 <- as.matrix(table1) # Might be unnecessary
  table2 <- as.matrix(table2)
  
  #check if input consists of only numerical values
  if(!is.numeric(table1) | !is.numeric(table2)){
    stop("Please make sure table1 and table2 are numeric matrices or a data.frame that consists
          only of numerical values")
  }
  
  numerator <- tr((table1 %*% t(table1)) %*% (table2 %*% t(table2))) # Need to check online
  denom1 <- tr((table1 %*% t(table1)) %*% (table1 %*% t(table1)))
  denom2 <- tr((table2 %*% t(table2)) %*% (table2 %*% t(table2)))

  res <- numerator/sqrt(denom1 * denom2)
  return(res)
}

tr = function(x){
  return(sum(diag(x)))
}
