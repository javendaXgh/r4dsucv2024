

library(shiny)
library(plotly)
library(DT)

# Crea la interfaz de usuario UI
ui <- fluidPage(
  titlePanel("Visualización de mtcars"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xVar", "Variable en el eje X:", choices = names(mtcars)),
      selectInput("yVar", "Variable en el eje Y:", choices = names(mtcars), multiple = TRUE),
      actionButton("submit", "Generar gráfico y tabla")
    ),
    mainPanel(
      tableOutput("mtcarsTable"),
      plotlyOutput("mtcarsPlot")
    )
  )
)

# Crea la función servidor SV
server <- function(input, output) {
  filteredData <- reactive({
    subset(mtcars, select = c(input$xVar, input$yVar))
  })
  
  output$mtcarsTable <- renderDT({
    datatable(filteredData(), options = list(dom = 't'), caption = "Tabla de mtcars")
  })
  
  output$mtcarsPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes_string(x = input$xVar, y = paste0(input$yVar, collapse=" +"))) +
      geom_point() +
      labs(title = "Gráfico de dispersión")
    
    if (length(input$yVar) > 1) {
      p <- p + facet_wrap(~ paste0(input$yVar, collapse=":"))
    }
    
    ggplotly(p)
  })
  
  observeEvent(input$submit, {
    message("Gráfico y tabla generados.")
  })
}

# Lanza la aplicación
shinyApp(ui = ui, server = server)