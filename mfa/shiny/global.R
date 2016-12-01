library(shiny)
library(mfa)


LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}


