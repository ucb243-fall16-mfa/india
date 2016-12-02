
## load the object to the environment

function(input, output, session) {
  
  # define the output plot depending on the selection of the mfa object and the 
  # type of plot chosen by the user.
    output$plot <- renderPlot({
      inFile <- input$file1
      
      if (is.null(inFile)){
        return(NULL)
      }
      
      # the LoadToEvvironment function could be found in the global.R file in 
      # shiny app folder.
      env_fn <- reactiveFileReader(1000, session, inFile$datapath, 
                                   LoadToEnvironment)
      env <- env_fn()

      # the content in the environment is the mfa object.
      object <- ls(env)
      print(class(object))
      mfa1 <- get(object[1], env)
      sets <- as.numeric(length(mfa1$partialFactorScores))

      ## check whether the object is of mfa class and has the right contents.
      if (check_mfa(mfa1) != TRUE) {
        helpText("the object in the Rdata file is not mfa object")
      } else {
        ## Depending on the user's selection, we'll creat the plot 
        ## by calling the corresponding mfa plot function.
        if(input$select == "Eigenvalues") {
          barplot(mfa1$lambda,
                              las = 1,
                              main="Barplot of Eigenvalues",
                              ylab="Eigenvalues",
                              xlab="");
        }else if (input$select == "Variable loadings") {
          plot_variable_loadings(mfa1, c(input$Dim1, input$Dim2))
        }else if (input$select == "Common factor scores") {
          plot_compromise(mfa1, c(input$Dim1, input$Dim2))
        }else if (input$select == "Partial factors scores") {
          plot_partial_factor(mfa1, c(input$Dim1, input$Dim2))
        }
      }
    })
}

