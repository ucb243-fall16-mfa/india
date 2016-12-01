## ------------------------------------------------------------------------
#library(devtools)
#install("mfa")
#library(mfa)

#devtools::build_vignettes()


## ------------------------------------------------------------------------
library(mfa)
data <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv")
SETS <- list(c(1:6),c(7:12), c(13:18),c(19:23),c(24:29),c(30:34),c(35:38),c(39:44),c(45:49),c(50:53))

#remove the first row because it contains non-numerical values
data <- data[,-1]
mfa1 <- mfa(data = data, sets = SETS )

mfa1


## ------------------------------------------------------------------------
#grid <- par(mar = rep(2, 2))
#plot(mfa1)
#par(grid)

## ------------------------------------------------------------------------
#plot_compromise(mfa1)
#plot_partial_factor(mfa1)
#plot_variable_loadings(mfa1)


## ------------------------------------------------------------------------
summary_eigenvalues(mfa1)

## ------------------------------------------------------------------------
contribution_obs_dim(mfa = mfa1, l_max = 3)


## ------------------------------------------------------------------------
contribution_var_dim(mfa = mfa1, l_max = 2)

## ------------------------------------------------------------------------
contribution_table_dim(mfa = mfa1, l_max = 4)

## ------------------------------------------------------------------------
RV(data[,SETS[[1]]], data[,SETS[[2]]])

## ------------------------------------------------------------------------
RV_table(mfa = mfa1, SETS[1:4])

