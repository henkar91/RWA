
library(shiny)

# UI
ui <- fluidPage(
   
   fluidRow(
     column(3,
            fileInput("infile",
                      label = "upload data", 
                      accept = c(".sav", ".xlsx", ".csv")),
            selectInput("indep", "Select Dependant", choices = "Pending Upload"),
            selectInput("inmeasure", "Select Other Vars", choices = "Pending Upload"),
            selectInput("inweight", "Select Weight", choices = "Pending Upload")
     ),
     column(6,
            div(align = "center",
            p("RWA", style = "font-size: 76pt; color: #99ffaa"),
            tableOutput("contents"),
     column(3))
     )
   )
   )


# Server
server <- function(input, output, session) {
   
  contentsrea <- reactive({
    inFile <- input$infile
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
    
    
  })
  output$contents<-renderTable({contentsrea()})
    
  observe({
    updateSelectInput(session, "indep", choices = names(contentsrea()))
    updateSelectInput(session, "inmeasure", choices = names(contentsrea()))
    updateSelectInput(session, "inweight", choices = names(contentsrea()))
  })
} 

# Run the application 
shinyApp(ui = ui, server = server)

