#' plot_partial_factor
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param x - the mfa object
#' @param title - chart title
#' @param color - optional vector of equal length to the rows of the original 
#' data
#' @return a plot with the projection of each observation onto the first two
#' extracted components

# Include sets as an argument

plot_partial_factor <- function(x, ...) UseMethod('plot_partial_factor')

plot_partial_factor.mfa <- function(x, color = NULL
                                    , title = 'Partial Factor Scores'){
  
  # Set plotting parameters
  tables <- length(x$partialFactorScores)
  cols <- 5 # Number of columns in the grid
  rows <- tables / cols # Number of rows in the grid
  par(mfrow = c(rows, cols)
      , oma = c(4, 2, 4, 2) # Outer margin (b, r, u, l)
      , mar = c(2, 2, 2, 2) ) # Inner margins (b, r, u, l)
  
  # Min and max plotting dimensions
  pfs_combined <- do.call(rbind, mfa1$partialFactorScores)
  x_range <- c(min(pfs_combined[,1]), max(pfs_combined[,1]))
  y_range <- c(min(pfs_combined[,2]), max(pfs_combined[,2]))
  
  # If color argument is given
  if(!is.null(color)){
    
    # Create color scheme for plotting
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
    
    # Loop through and plot each table
    for (k in 1:tables){
      
      # Subset the relevant matrix
      pfs_k <- x$partialFactorScores[[k]]
      
      # Plot the data
      plot(pfs_k[,1]
           , pfs_k[,2]
           , pch = 16
           , cex = 2
           , xlim = x_range
           , ylim = y_range
           , xlab = ''
           , ylab = ''
           , col = color_scheme)
      
      # Create the legend (first plot only)
      if(k == 1){
        legend(x = x_range[1]
               , y = y_range[2]
               , legend = unique(color)
               , col = unique(color_scheme)
               , pch = 16
               , cex = 1
               )
      }
      
      # Text to identify the table number
      text(x = x_range[2] * .8
           , y = y_range[2]
           , labels = paste0('k=', k))
      
      # Plot center lines
      abline(v = 0, h = 0)
    }
    
    # No Color Argument Given
  } else {
    
    # Loop through and plot each table
    for (k in 1:tables){
      
      # Subset the relevant matrix
      pfs_k <- x$partialFactorScores[[k]]
      
      # Plot the data
      plot(pfs_k[,1]
           , pfs_k[,2]
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
      
      # Plot center lines
      abline(v = 0, h = 0)
    }
  }
  mtext('Partial Factor Scores', outer = TRUE, cex = 1.5)
}

# plot_partial_factor(mfa1, substr(wine$ID, 1, 2))
# plot_partial_factor(mfa1)