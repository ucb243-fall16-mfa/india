
library(shiny)

fluidPage(
  titlePanel("Multiple Factor Analysis Results"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Please load the saved mfa object from the .Rdata 
                file:', accept = c("txt/csv", ".csv")),
      selectInput("select", label = h3("Select the results to plot"), 
                  c("Eigenvalues", "Common factor scores", 
                    "Partial factors scores", "Variable loadings"))
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
)