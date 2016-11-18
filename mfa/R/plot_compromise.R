#' plot_compromise
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param x - the mfa object
#' @param title - chart title
#' @param color - optional vector of equal length to the rows of the original data
#' @return a plot with the projection of each observation onto the first two
#' extracted components

# Compromise isn't showing title and margins are off for color only

plot_compromise <- function(x, ...) UseMethod('plot_compromise')

plot_compromise.mfa <- function(x, color = NULL, title = 'Compromise'){
  
  par(mfrow = c(1, 1), mar = c(1, 2, 1, 2))

  if(!is.null(color)){
    
    # Create color scheme for plotting
    color_pallette <- c('#e31a1c', '#1f78b4', '#33a02c', '#b2df8a'
                        , '#fb9a99','#a6cee3', '#fdbf6f')
    
    color_scheme <- rep(NA, length(color))
    for(i in 1:length(unique(color))){
      color_scheme[color == unique(color)[i]] <- color_pallette[i]
    }
    
    # Plot the data
    plot(x$factorScores[,1]
         , x$factorScores[,2]
         , pch = 16
         , cex = 2
         , xlab = 'First Factor'
         , ylab = 'Second Factor'
         , col = color_scheme)
    
    # Create the legend
    legend (x = min(x$factorScores[,1])
            , y = max(x$factorScores[,2])
            , legend = unique(color)
            , col = unique(color_scheme)
            , pch = 16)
  } else {
    plot(x$factorScores[,1]
         , x$factorScores[,2]
         , xlab = 'First Factor'
         , ylab = 'Second Factor')
  }
  if(!is.null(attr(mfa1, 'ids'))){
    text(x$factorScores[,1]
         , x$factorScores[,2]
         , pos = 1
         , labels = attr(mfa1, 'ids'))     
  }
  
  abline(v = 0, h = 0)
  mtext(title, outer = TRUE, cex = 1.5)
}

# plot_compromise(mfa1, substr(wine$ID, 1, 2))
