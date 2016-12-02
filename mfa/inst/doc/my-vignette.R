## ------------------------------------------------------------------------
library(mfa)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6, fig.align = "center")

## ---- eval=FALSE---------------------------------------------------------
#  library(devtools)
#  install("mfa")
#  library(mfa)
#  #after loading the library, change the working directory to the directory
#  #of the package. In most cases the following line should suffice:
#  setwd("./mfa/")
#  #execute the following line to knit the vignette
#  devtools::build_vignettes() # Note: I don't think they should have to build the vignette or change their working directory

## ------------------------------------------------------------------------
setwd('/Users/josiahdavis/Documents/LearningDataScience/Berkeley/STAT243/stat243FinalProject')
library(mfa)
wine <- read.csv('../project/wines.csv')
SETS <- list(c(1:6),c(7:12), c(13:18),c(19:23),c(24:29),c(30:34),c(35:38),c(39:44),c(45:49),c(50:53))

#remove the first row because it contains non-numerical values
data <- wine[,-1]
mfa1 <- mfa(data = data, sets = SETS )

mfa1


## ------------------------------------------------------------------------
par(mar = rep(1, 4))
plot(mfa1)

## ------------------------------------------------------------------------
plot_compromise(mfa1)


## ------------------------------------------------------------------------
par(mar = rep(1, 4))
plot_partial_factor(mfa1)

## ------------------------------------------------------------------------
# plot_variable_loadings(mfa1)

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

## ------------------------------------------------------------------------
Lg(data[,SETS[[1]]], data[,SETS[[2]]])

## ------------------------------------------------------------------------
Lg_table(mfa = mfa1, SETS[1:4])


