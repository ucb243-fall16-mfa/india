#' plot_variable_loadings
#'
#' Plot the compromise scores for the first two extracted dimensions
#' @param x - the mfa object
#' @param title - chart title
#' @return a plot with the variable loadings for each table

plot_variable_loadings <- function(x, ...) UseMethod('plot_variable_loadings')

plot_variable_loadings.mfa <- function(x, title = 'Variable Loadings'){
  
  # Set plotting parameters
  tables <- length(x$partialFactorScores)
  cols <- 5 # Number of columns in the grid
  rows <- tables / cols # Number of rows in the grid
  par(mfrow = c(rows, cols)
      , oma = c(4, 2, 4, 2) # Outer margin (b, r, u, l)
      , mar = c(2, 2, 2, 2) ) # Inner margins (b, r, u, l)
  
  # Min and max plotting dimensions
  x_range <- c(min(x$Q[,1]), max(x$Q[,1]))
  y_range <- c(min(x$Q[,2]), max(x$Q[,2]))
  
  # TEMPORARY STOP-GAP MEASURE
  var_names <- gsub('\\.[0-9]', '', unlist(SETS_2))
  
  # Loop through and plot each table
  for (k in 1:tables){
    
    # Subset the relevant matrix
    loadings_k <- x$Q[attributes(x)$sets[[k]],]
    var_names_k <- var_names[attributes(x)$sets[[k]]]
    
    # Plot the data
    plot(loadings_k[,1]
         , loadings_k[,2]
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
    
    # Text to identify the variable
    text(x = loadings_k[,1]
         , y = loadings_k[,2]
         , labels = var_names_k
         , pos = 1
         )
    
    # Plot center lines
    abline(v = 0, h = 0)
    }
  mtext(title, outer = TRUE, cex = 1.5)
}

# plot_variable_loadings(mfa1, substr(wine$ID, 1, 2))
# plot_variable_loadings(mfa1)