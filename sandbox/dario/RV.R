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
  numerator <- trace((table1 %*% t(table1)) * (table2 %*% t(table2)))
  denom1 <- trace((table1 %*% t(table1)) * (table1 %*% t(table1)))
  denom2 <- trace((table2 %*% t(table2)) * (table2 %*% t(table2)))

  res <- numerator/sqrt(denom1 * denom2)
  return(res)
}
