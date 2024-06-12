# Load packages / datasets -----------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(ggthemes)

cdc <- read_delim('data/cdc.txt', delim = '|') |>
  mutate(
    gender = factor(gender, levels = c('f', 'm'), labels = c('Female', 'Male')),
    hlthplan = factor(hlthplan, levels = c(1, 0), labels = c('Yes', 'No')),
    exerany = factor(exerany, levels = c(1, 0), labels = c('Yes', 'No')),
    smoke100 = factor(smoke100, levels = c(1, 0), labels = c('Yes', 'No')),
    genhlth = factor(genhlth, 
      levels = c('excellent', 'very good', 'good', 'fair', 'poor'),
      labels = str_to_title(c('excellent', 'very good', 'good', 'fair', 'poor'))
    )
  )

# Define UI ---------------------------------------------------------------
ui <- fluidPage( 
  
  # Application title ---- 
  titlePanel('CDC BRFSS Histograms'),
  
  # Sidebar panel ----
  sidebarLayout(
    
    # move to the right ----
    position = 'right', 
    
    sidebarPanel(
    selectInput(
      inputId = 'x_var',
      label = 'Select Variable: ', 
      choices = list(
        'Actual Weight',
        'Desired Weight',
        'Height'
      )
    ),
    
    sliderInput(
      inputId = 'bins', 
      label = 'Number of bins:',
      min = 5, 
      max = 50, 
      value = 30,
      step = 5,
      animate = TRUE
    ),
    
    radioButtons(
      inputId = 'fill_var',
      label = 'Select Fill/Legend Variable:',
      choices = list(
        'General Health', 
        'Health Coverage',
        'Exercised in Past Month',
        'Smoked 100 Cigarettes', 
        'Gender'),
      selected = 'Gender'
    )
  ),
  
  # Output: histogram ----
  mainPanel(
    plotOutput(outputId = 'distPlot')
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x_variable <- case_when(
      input$x_var == 'Actual Weight' ~ 'weight',
      input$x_var == 'Desired Weight' ~ 'wtdesire',
      input$x_var == 'Height' ~ 'height')
    
    x_axis_title <- case_when(
      input$x_var == 'Actual Weight' ~ str_c(input$x_var, ' in Pounds'),
      input$x_var == 'Desired Weight'~ str_c(input$x_var, ' in Pounds'),
      input$xvar == 'Height' ~ str_c(input$x_var, ' in Inches')
    )
    
    fill_variable <- switch(
      input$fill_var,
      'General Health' = 'genhlth',
      'Health Coverage' = 'hlthplan',
      'Exercised in Past Month' = 'exerany',
      'Smoked 100 Cigarettes' = 'smoke100',
      'Gender' = 'gender'
    ) |>
      sym()
    
    # generate bins based on input$bins from ui.R
    x    <- cdc |> pull(x_variable)
    bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # build plot ----
    ggplot(cdc, aes(x = !!sym(x_variable))) + 
      geom_histogram(
        aes(fill = {{fill_variable}}),
        color = 'black',
        breaks = bin_breaks) + 
      labs(
        x = x_axis_title,
        y = 'Count',
        fill = input$fill_var
      ) + 
      theme_fivethirtyeight(base_size = 20) +
      theme(
        axis.title = element_text(),
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = 'top',
        legend.title.position = 'top',
        legend.title = element_text(hjust = 0.5)
      )
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)