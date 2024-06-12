
# Load packages -----------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)


# Define UI ---------------------------------------------------------------
ui <- page_sidebar( #first should always be 'page_*'

    # Application title ---- 
    title = 'Hello World!',

    # Sidebar panel ----
    sidebar = sidebar(
      sliderInput(
        inputId = 'bins', 
        label = 'Number of bins: ',
        min = 5, 
        max = 50, 
        value = 30,
        step = 5,
        animate = TRUE
      )
    ),
    
    # Output: histogram ----
    plotOutput(outputId = 'distPlot')
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful |> pull(waiting)
        bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)

      # build plot ----
        ggplot(faithful, aes(x = waiting)) + 
          geom_histogram(
            breaks = bin_breaks,
            fill = '#007bc2', 
            color = 'orange') + 
          theme_minimal() +
          labs(
            title = 'Histogram of waiting times',
            x = 'Waiting time to next eruption (in mins)',
            y = 'frequency'
          ) + 
          theme(
            plot.title = element_text(size = 30, hjust = 0.5) 
          )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

