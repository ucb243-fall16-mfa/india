
# need to include figure out how to pass the object in the saved file as the 
# object "mfa1"

# add the tables, check the colors



# load the object to the environment


function(input, output, session) {
  
  # install the package from github. If installed locally, sometimes there would 
  # be error using some functions.
  devtools::install_github("fussballball/stat243FinalProject/mfa", 
                           force_deps = FALSE)
  

    output$plot <- renderPlot({
      inFile <- input$file1
      
      if (is.null(inFile)){
        return(NULL)
      }
      
      print(inFile$datapath)
      env_fn <- reactiveFileReader(1000, session, inFile$datapath, LoadToEnvironment)
      env <- env_fn()
      contents <- ls(env)
      mfa1 <- get(contents[1], env)

      # Depending on input$input_type, we'll creat the plot user demanded.
      if(input$select == "Eigenvalues"){
        barplot(mfa1$lambda,
                            las = 1,
                            main="Barplot of Eigenvalues",
                            ylab="Eigenvalues",
                            xlab="");
      }else if (input$select == "Variable loadings") {
        plot_variable_loadings(mfa1)
      }else if (input$select == "Common factor scores") {
        plot_compromise(mfa1)
      }else if (input$select == "Partial factors scores") {
        plot_partial_factor(mfa1)
      }
      
    })
}