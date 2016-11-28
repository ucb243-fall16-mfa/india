
library(shiny)

fluidPage(
  titlePanel("Multiple Factor Analysis Results"),
  #  fileInput('file', 'Choose CSV File',
  #            accept=c('text/csv', 
  #                     'text/comma-separated-values,text/plain', 
  #                     '.csv')),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select the results to plot"), 
                  c("Eigenvalues", "Common factor scores", 
                    "Partial factors scores", "Variable loadings"))
      # selectInput("groupSelect", label = h4("Group"), 
      #             c(1:10)),
      # 
      # DT::dataTableOutput("table")
      ),
    
      mainPanel(
        plotOutput('plot')
      )
  )
)