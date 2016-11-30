# how to let user to define sets

library(shiny)

function(input, output) {
  
  # this commented part are the codes could be considered for openning the mfa 
  # object file(currently we are using the mfa object available in the global 
  # environment)
  
  #  output$ui <- renderUI({
  #    inFile <- input$file
  #    if (is.null(inFile))
  #      return(NULL)
  # open file
  #    data <- read.rda(inFile$datapath) 
  #    if(!is.data.frame(data) & !is.matrix(data)) {
  #      stop('Data must be either dataframe or matrix')
  #    }
  
  
  # source all the mfa functions and methods, can replace with calling the mfa 
  # package after it's available. 
  
  for( f in list.files(paste0("/Users/yanlifan/Desktop/STAT243/Final Project/",
                              "stat243FinalProject/mfa/R"), pattern = "plot", 
                       full.names = TRUE)) {
    source(f)
  }
  
  # Depending on input$input_type, we'll creat the plot user demanded.
    output$plot <- renderPlot({
      if(input$select == "Eigenvalues"){
        barplot(mfa1$eigenValues,
                            las = 1,
                            main="Barplot of Eigenvalues",
                            ylab="Eigenvalues",
                            xlab="");
      }else if (input$select == "Variable loadings") {
        plot_variable_loadings.mfa(mfa1,)
      }else if (input$select == "Common factor scores") {
        plot_compromise(mfa1)
      }else if (input$select == "Partial factors scores") {
        plot_partial_factor(mfa1)
      }
    })
}