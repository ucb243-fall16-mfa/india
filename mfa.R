#mfa function

#' mfa function to create an object
#' returns an object of class mfa() with certain properties
#' ev contains the eigenvalues of the data
#' cfs contains the common factor scores
#' pfs contains the partial factor scores
#' fl contains the factor loadings (right singular values)
#' @data: data set (matrix or data frame).
#' @sets list of vectors indicating the sets of variables (i.e. the blocks).
#' @ncomps integer indicating how many number of components (i.e. factors) are to be extracted.
#' @center either a logical value or a numeric vector of length equal to the number of active variables in the analysis
#' @scale either a logical value or a numeric vector of length equal to the number of active variables in the analysis
data <- read.csv("C:/Users/Dario/Desktop/stat243-fall-2016/problem-sets/final-project/data/wines.csv")


#first, do everything for 1 wine expert:
#data from person 2
grep(pattern = "\\.2", x = colnames(data),value = TRUE)
data[,grep(pattern = "\\.2", x = colnames(data),value = TRUE)]


#get the singular value decomposition of this table
SVD <- svd(data[,grep(pattern = "\\.2", x = colnames(data),value = TRUE)])

#get matrices u,v, gamma
U <- SVD$u
V <- SVD$v
GAMMA <- diag(SVD$d)
gamma <- SVD$d

#get the factor scores:
G <- U%*%GAMMA

#get the weight of the table
alpha[2] <- 1/gamma[1]^2





mfa(data, sets, ncoms = NULL, center = TRUE, scale = TRUE){




}



