
fluidPage(
  titlePanel(h1("Multiple Factor Analysis Result", 
                style = "font-family: 'Times New Roman';
       font-weight: 500; line-height: 1.1; 
       color: #000000;")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', label = h3('Choose Rdata File:'), accept = c(".Rdata")),
      helpText("Loading the saved mfa object"),
      
      selectInput("select", label = h3("Select the Results to Plot"), 
                  c("Eigenvalues", "Common factor scores", 
                    "Partial factors scores", "Variable loadings"))
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
)