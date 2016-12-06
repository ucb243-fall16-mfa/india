
library(shiny)
library(mfa)

#' LoadToEnvironment
#'
#' load the file of saved mfa object into the environment
#' @param RData - the file path
#' @param env - the destination environment, default value is a new environment.
#' @return a new envrionemt with the mfa object in it.
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

#' check_mfa
#'
#' check function to make sure an mfa object is valid
#' @param mfa - a (mfa) object
#' @return if the object is an mfa object, return TRUE, otherwise, return FALSE.
check_mfa <- function(mfa){
  if(!("mfa" %in% class(mfa))){
    return(FALSE)
  }
  if(!all(c("lambda", "commonFactorScores",
            "partialFactorScores", "Q", "P") %in% names(mfa))){
    return(FALSE)
  }
  TRUE
}


#' plot_compromise
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param mfa - the mfa object
#' @param title - chart title
#' @param dims - Numeric vector of two values signifying dimensions to plot
#' @return a plot with the projection of each observation onto the first two
#'         extracted components
#' @export

# Compromise isn't showing title and margins are off for color only
plot_compromise <- function(mfa, dims = 1:2, title = 'Common Factor Scores'){
  
  ## perform checking
  check_mfa(mfa)
  check_plot_dims(length(attributes(mfa)$sets), dims)
  
  par(mfrow = c(1, 1),  mar = c(5, 4, 4, 2))
  
  color <- attributes(mfa)$color
  
  if(!is.null(color)){
    
    # Create color scheme for plotting
    color_pallette <- c('#e31a1c', '#1f78b4', '#33a02c', '#b2df8a'
                        , '#fb9a99','#a6cee3', '#fdbf6f')
    
    color_scheme <- rep(NA, length(color))
    for(i in 1:length(unique(color))){
      color_scheme[color == unique(color)[i]] <- color_pallette[i]
    }
    
    # Plot the data
    plot(mfa$commonFactorScores[,dims[1]]
         , mfa$commonFactorScores[,dims[2]]
         , pch = 16
         , cex = 2
         , xlab = paste('Factor', dims[1])
         , ylab = paste('Factor', dims[2])
         , col = color_scheme)
    
    # Create the legend
    legend(x = min(mfa$commonFactorScores[,dims[1]])
           , y = max(mfa$commonFactorScores[,dims[2]])
           , legend = unique(color)
           , col = unique(color_scheme)
           , pch = 16)
  } else {
    plot(mfa$commonFactorScores[,dims[1]]
         , mfa$commonFactorScores[,dims[2]]
         , xlab = paste('Factor', dims[1])
         , ylab = paste('Factor', dims[2]))
  }
  if(!is.null(attr(mfa, 'ids'))){
    text(mfa$commonFactorScores[,dims[1]]
         , mfa$commonFactorScores[,dims[2]]
         , pos = 1
         , labels = attr(mfa, 'ids'))     
  }
  
  abline(v = 0, h = 0)
  mtext(title, cex = 1.5)
}

# plot_compromise(mfa1)



#' plot_partial_factor
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param mfa - the mfa object
#' @param title - optional chart title
#' @param dims - Numeric vector of two values signifying dimensions to plot
#' @return a plot with the projection of each observation onto the first two
#'         extracted components
#' @export

# Include sets as an argument
plot_partial_factor <- function(mfa, dims = 1:2, title = 'Partial Factor Scores'){
  
  ## perform checking
  check_mfa(mfa)
  check_plot_dims(length(attributes(mfa)$sets), dims)
  
  ## set plotting parameters
  tables <- length(mfa$partialFactorScores)
  cols <- 5 # Number of columns in the grid
  rows <- ceiling(tables / cols) # Number of rows in the grid
  par(mfrow = c(rows, cols)
      , oma = c(4, 2, 4, 2) # Outer margin (b, r, u, l)
      , mar = c(2, 2, 2, 2)) # Inner margins (b, r, u, l)
  
  ## min and max plotting dimensions
  pfs_combined <- do.call(rbind, mfa$partialFactorScores)
  x_range <- c(min(pfs_combined[,dims[1]]), max(pfs_combined[,dims[1]]))
  y_range <- c(min(pfs_combined[,dims[2]]), max(pfs_combined[,dims[2]]))
  
  ## if color argument is given
  color <- attributes(mfa)$color
  if(!is.null(color)){
    
    ## create color scheme for plotting
    color_pallette <- c('#e31a1c'
                        , '#1f78b4'
                        , '#33a02c'
                        , '#b2df8a'
                        , '#fb9a99'
                        ,'#a6cee3'
                        , '#fdbf6f')
    color_scheme <- rep(NA, length(color))
    
    for(i in 1:length(unique(color))){
      color_scheme[color == unique(color)[i]] <- color_pallette[i]
    }
    
    ## loop through and plot each table
    for (k in 1:tables){
      
      ## subset the relevant matrix
      pfs_k <- mfa$partialFactorScores[[k]]
      
      ## plot the data
      plot(pfs_k[,dims[1]]
           , pfs_k[,dims[2]]
           , pch = 16
           , cex = 2
           , xlim = x_range
           , ylim = y_range
           , xlab = ''
           , ylab = ''
           , col = color_scheme)
      
      ## create the legend (first plot only)
      if(k == 1){
        legend(x = x_range[1]
               , y = y_range[2]
               , legend = unique(color)
               , col = unique(color_scheme)
               , pch = 16
               , cex = 1
        )
      }
      
      ## text to identify the table number
      text(x = x_range[2] * .8
           , y = y_range[2]
           , labels = paste0('k=', k))
      
      ## plot center lines
      abline(v = 0, h = 0)
    }
    
    ## no color argument given
  } else {
    
    ## loop through and plot each table
    for (k in 1:tables){
      
      ## subset the relevant matrix
      pfs_k <- mfa$partialFactorScores[[k]]
      
      ## plot the data
      plot(pfs_k[,dims[1]]
           , pfs_k[,dims[2]]
           , pch = 1
           , cex = 2
           , xlim = x_range
           , ylim = y_range
           , xlab = ''
           , ylab = '')
      
      ## text to identify the table number
      text(x = x_range[2] * .8
           , y = y_range[2]
           , labels = paste0('k=', k))
      
      ## plot center lines
      abline(v = 0, h = 0)
    }
  }
  mtext('Partial Factor Scores', outer = TRUE, cex = 1.5)
}

# plot_partial_factor(mfa1, substr(wine$ID, 1, 2))
# plot_partial_factor(mfa1)




#' plot_variable_loadings
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param mfa - the mfa object
#' @param title - chart title
#' @param dims - Numeric vector of two values signifying dimensions to plot
#' @return a plot with the variable loadings for each table
#' @export

plot_variable_loadings <- function(mfa, dims = 1:2, title = 'Variable Loadings'){
  
  ## perform checking
  check_mfa(mfa)
  check_plot_dims(length(attributes(mfa)$sets), dims)
  
  # Set plotting parameters
  tables <- length(mfa$partialFactorScores)
  cols <- 5 # Number of columns in the grid
  rows <- ceiling(tables / cols) # Number of rows in the grid
  par(mfrow = c(rows, cols)
      , oma = c(4, 2, 4, 2) # Outer margin (b, r, u, l)
      , mar = c(2, 2, 2, 2) ) # Inner margins (b, r, u, l)
  
  # Min and max plotting dimensions
  x_range <- c(min(mfa$Q[,dims[1]]), max(mfa$Q[,dims[1]]))
  y_range <- c(min(mfa$Q[,dims[2]]), max(mfa$Q[,dims[2]]))
  
  # Loop through and plot each table
  for (k in 1:tables){
    
    # Subset the relevant matrix
    loadings_k <- mfa$Q[attributes(mfa)$sets[[k]],]
    
    
    # Plot the data
    plot(loadings_k[,dims[1]]
         , loadings_k[,dims[2]]
         , pch = 1
         , cex = 2
         , xlim = x_range
         , ylim = y_range
         , xlab = ''
         , ylab = '')
    
    # Text to identify the table number
    text(x = x_range[2] * .8
         , y = y_range[2]
         , labels = paste0('k=', k))
    
    # Text to identify the variable if it exists
    if(!is.null(attributes(mfa)$var_names)){
      text(x = loadings_k[,dims[1]]
           , y = loadings_k[,dims[2]]
           , labels = attributes(mfa)$var_names[[k]]
           , pos = 1
      )
    }
    
    # Plot center lines
    abline(v = 0, h = 0)
  }
  mtext(title, outer = TRUE, cex = 1.5)
}

# plot_variable_loadings(mfa1)
# plot_variable_loadings(mfa2)



#' plot
#'
#' Create summary plots for the result of the multiple factor analysis
#' @param mfa - the mfa object
#' @return a three plots with the projection of each observation onto
#'         the first two extracted components, the partial factor scores,
#'         and the variable loadings
#' @export
plot.mfa <- function(mfa, dims = 1:2){
  check_mfa(mfa)
  check_plot_dims(length(attributes(mfa)$sets), dims)
  plot_compromise(mfa, dims)
  plot_partial_factor(mfa, dims)
  plot_variable_loadings(mfa, dims)
}

# plot(mfa1)


#' check_plot_dims
#'
#' make sure plot dimensions has a viable value
#' @param dims - numeric vector of dimensions
#' @param nGroups - numberic value of how many total groups there are
#' @return boolean - did the test pass
check_plot_dims <- function(nGroups, dims){
  if(length(dims) != 2){
    stop('plotting only supports two dimensions')
  }
  
  if(max(dims) > nGroups){
    stop('There are only ', nGroups,' tables in the data')
  }
  TRUE
}



