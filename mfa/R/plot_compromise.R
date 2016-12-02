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
