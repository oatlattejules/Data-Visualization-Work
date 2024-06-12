# Load packages / datasets -----------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
cdc <- read_delim('/Users/juliannawang/Desktop/STAT 302/data_vis_labs/data/cdc.txt', delim = '|')

# Define UI ---------------------------------------------------------------
ui <- page_sidebar( #first should always be 'page_*'
  
  # Application title ---- 
  title = 'CDC BRSFF: Histogram of Weight Grouped by Gender',
  
  # Sidebar panel ----
  sidebar = sidebar(
    
    # move to the right ----
    position = 'right', 
    
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
    x    <- cdc |> pull(weight)
    bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # build plot ----
    ggplot(cdc, aes(x = weight)) + 
      geom_histogram(
        aes(fill = gender),
        color = 'black',
        breaks = bin_breaks) + 
      scale_fill_discrete(labels = c('Female', 'Male')) +
      theme_minimal() +
      labs(
        title = NULL,
        x = 'Weight in Pounds',
        y = 'Count',
        fill = 'Gender'
      ) + 
      theme(
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = 'inside',
        legend.justification.inside = c(0.5, 0.78)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
