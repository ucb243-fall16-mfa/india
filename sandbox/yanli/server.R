# how to let user to define sets

library(shiny)

function(input, output) {
  
  #  output$ui <- renderUI({
  #    inFile <- input$file
  
  #    if (is.null(inFile))
  #      return(NULL)
  # open file
  #    data <- read.rda(inFile$datapath) 
  #    if(!is.data.frame(data) & !is.matrix(data)) {
  #      stop('Data must be either dataframe or matrix')
  #    }
  
  for( f in list.files(paste0("/Users/yanlifan/Desktop/STAT243/Final Project/",
                              "stat243FinalProject/mfa/R"), pattern = "plot", 
                       full.names = TRUE)) {
    source(f)
  }
  
  getUIData <- reactive(
    {
      
    
      
      list(tableData2,plotHandle)
      #tableData
    })
  

  # Depending on input$input_type, we'll generate a different
  # UI component and send it to the client.
    output$plot <- renderPlot({
      if(input$select == "Eigenvalues"){
        plotHandle= barplot(mfa1$eigenValues,
                            las = 1,
                            main="Barplot of Eigenvalues",
                            ylab="Eigenvalues",
                            xlab="");
      }else if (input$select == "Variable loadings") {
        plotHandle= plot_variable_loadings.mfa(mfa1)
      }else if (input$select == "Common factor scores") {
        plotHandle = plot_compromise(mfa1)
      }else if (input$select == "Partial factors scores") {
        plotHandle = plot_partial_factor(mfa1)
      }
      
    })
    # tried to show original data, but seems too many numbers. we could discuss
    # and decide the whether to show the data and what would be a better way if 
    # we do want to show them.
    
    # output$table <-DT::renderDataTable(DT::datatable({
    #   if(input$select == "Eigenvalues"){
    #     tableData2 = data.frame(Eigenvalues=round(mfa1$eigenValues,2))
    #   } else if (input$select == "Variable loadings") {
    #     tableData2 = data.frame(VariableLoadings=round(mfa1$loadings, 2))
    #   } else if (input$select == "Common factor scores") {
    #     tableData2 = data.frame(CommonScore=round(mfa1$factorScores,2))
    #   } else if (input$select == "Partial factors scores") {
    #     rawData = mfa1$partialFactorScores[[as.numeric(input$groupSelect)]]
    #     tableData2 = data.frame(PartialScore=round(rawData,2))
    #   }
    #   
    # }, options = list(searching = FALSE, paging=FALSE)))
    
}