
library(shiny)

fluidPage(
  titlePanel("Multiple Factor Analysis Results"),
  
  #  fileInput('file', 'Choose File',
  #            accept=c('text/csv', 
  #                     'text/comma-separated-values,text/plain', 
  #                     '.csv')),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select the results to plot"), 
                  c("Eigenvalues", "Common factor scores", 
                    "Partial factors scores", "Variable loadings"))
      ),
    
      mainPanel(
        plotOutput('plot')
      )
  )
)