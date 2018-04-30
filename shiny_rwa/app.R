
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
            tableOutput("filecontents"),
            hr(),
            tableOutput("showdata")
            )
            ),

     column(3)
     )
     )


# Server
server <- function(input, output, session) {
   
  dat <- reactive({
    inFile <- input$infile
    if (is.null(inFile)) {return()}
    read.csv(inFile$datapath)
    })
  
  output$filecontents <- renderTable({
    if (is.null(dat())) {return ()}
    input$infile
      })
  
  
  observe({
    updateSelectInput(session, "indep", choices = names(dat()))
    updateSelectInput(session, "inmeasure", choices = names(dat()))
    updateSelectInput(session, "inweight", choices = names(dat()))
        })

  output$showdata <- renderTable({
    if (is.null(dat())) {return ()}
    dvar <- input$indep
    mvar <- input$inmeasure
    m1 <- mean(dat()[, dvar], na.rm = TRUE) 
    m2 <- mean(dat()[, mvar], na.rm = TRUE)
    m1 + m2
  })
  
  
  } 

# Run the application 
shinyApp(ui = ui, server = server)

