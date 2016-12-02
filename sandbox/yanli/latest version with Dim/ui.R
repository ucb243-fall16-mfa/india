
fluidPage(
  titlePanel(h1("Multiple Factor Analysis Result", style = "font-family: 'chalkboard';
       font-weight: 500; line-height: 1.1;
       color: #000000;")),
  sidebarLayout(
               sidebarPanel(
                 # choose the file with saved object
                 fileInput('file1', label = h3('Choose Rdata File:'), 
                                accept = c(".Rdata")),
                 # choose the results to plot.
                 selectInput("select", label = h3("Select the Results to Plot"), 
                                  c("Eigenvalues", "Common factor scores", 
                                    "Partial factors scores", "Variable loadings")),
                 
                 # let user to choose the dimensions they want to compare.
                 splitLayout(
                        uiOutput("Dim1"),
                        uiOutput("Dim2")
                      )
                    ),
               
               # main plot which will show the result of the interested object.
               mainPanel(
                 plotOutput('plot')
               )
  )
)