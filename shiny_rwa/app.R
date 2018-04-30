
library(shiny)

# UI
ui <- fluidPage(
   
   fluidRow(
     column(3),
     column(6,
            div(align = "center",
            p("RWA", style = "font-size: 76pt; color: #99ffaa"),
            tableOutput("tbl"),
     column(3))
     )
   )
   )


# Server
server <- function(input, output) {
   
   output$tbl <- renderTable({
    data(iris)
    head(iris)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

