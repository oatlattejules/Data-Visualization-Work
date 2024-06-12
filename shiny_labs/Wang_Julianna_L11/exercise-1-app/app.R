# Load packages / datasets -----------------------------------------------------------
library(shiny)
library(bslib)

# Define UI ---------------------------------------------------------------
ui <- page_sidebar(
  
  title = 'censusVis',
  
  sidebar = sidebar(
    helpText(
      'Create demographic maps with information from the 2010 US Census.'
    ),
    
    selectInput(
      inputId = 'var',
      label = 'Choose a variable to display',
      choices = c('Percent White',
                  'Percent Black',
                  'Percent Hispanic',
                  'Percent Asian'),
      selected = 'Percent White'
    ),
    
    sliderInput(
      inputId = 'range',
      label = 'Range of interest:',
      min = 0, max = 100, value = c(0, 100)
    )
  ),
  
  textOutput('selected_var'),
  textOutput('min_max') 
)

server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste('You have selected', input$var)
  })
  
  output$min_max <- renderText({
    paste('You have chosen a range that goes from',
          input$range[1], "to", input$range[2])
  })
  
}

# Run the application ----
shinyApp(ui = ui, server = server)