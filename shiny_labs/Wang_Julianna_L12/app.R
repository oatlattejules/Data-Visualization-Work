# Load packages -----------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(sf)

# Load datasets -----------------------------------------------------------
county_data <- sf::read_sf('data/county_data.shp') |>
  mutate(state = str_to_title(state))

state_list <- unique(county_data$state)

# UI ----
ui <- page_sidebar(

    # Application title
    title = 'County Demographic Map by State',

    # Sidebar 
    sidebar = sidebar(
      width = '3in',
      helpText(
        'Create demographic maps with information from the US Census.'
      ),
      
      # State input ----
      selectInput(
        inputId = 'state',
        label = 'Choose a state to display',
        choices = state_list,
        selected = 'North Carolina'
      ),
      
      # Race demographic input ----
      selectInput(
        inputId = 'var',
        label = 'Choose a variable to display',
        choices = c(
          'Percent White' = 'white', 
          'Percent Black' = 'black',
          'Percent Hispanice' = 'hispanic', 
          'Percent Asian' = 'asian'
        ),
        selected = 'white'
      )
    ),
    
    # Map in main panel ----
    card(
      plotOutput('map')
    )
)

# Server ---- 
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    # demographic to fill counties
    fill_var <- input$var
      
    # switch for color
    color <- c(
      'white' = 'darkgreen',
      'black' = 'black',
      'hispanic' = 'darkorange',
      'asian' = 'darkviolet'
    )
    
      county_data |>
        filter(state == input$state) |>
        ggplot(
          aes(
            geometry = geometry,
            fill = !!sym(fill_var)
            )
          ) + 
        geom_sf() + 
        scale_fill_gradient(
          low = 'white',
          high = color[[fill_var]],
          limit = c(0, 100)
        ) + 
        theme_void(base_size = 14) + 
        labs(title = input$state,
             fill = paste0('% ', str_to_title(input$var))) + 
        theme(plot.title = element_text(hjust = 0.5, size = 40))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
