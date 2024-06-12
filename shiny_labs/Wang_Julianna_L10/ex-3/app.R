# Load packages -----------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)

# Define UI ---------------------------------------------------------------

ui <- page_sidebar(
  title = "Where I'm From",
  
  #side bar ---- 
  sidebar = fluidRow(
    column(
      width = 12,
      h3('Fun Facts'),
      tags$b('State Nickname:'),
      'The Tar Heel State (Go Heels!) and the Old North State (from 1710, when the colonists divided the Carolinas into North and South)',
      tags$br(),
      tags$b('State Motto:'),
      'Esse quam videri, to be rather than to seem',
      tags$br(),
      tags$b('Population:'),
      '10,835,491 (est 2023)',
      tags$br(),
      tags$b('State Fossil:'),
      'Megalodon', 
      tags$br(),
      tags$b('State Insect:'),
      'Honey Bee',
      tags$br(),
      tags$b('State Flag:'),
      tags$img(src = 'nc_flag.jpg', 
               height = '100px',
               width = '200px')
    )),
  
  # cards ----
  layout_columns(
    card(
      width = 2000,
      card_header('North Carolina'),
      card_body(
        plotOutput('plot_output') 
      ),
      card_footer(
        HTML("For more information, visit North Carolina's <a href='https://www.nc.gov'>government website </a>.")
        )
  ),
  card(
      width = 2000,
      card_header('North Carolina is home to Wilbur and Orville Wright, the brothers who completed the first airplane ride on the dunes of Kitty Hawk, NC, near the Outer Banks.'),
      card_image('www/WrightBrothers.jpg')
    )
  )
)

#Define server logic ----
  server <- function(input, output) {
   output$plot_output <- renderPlot({
    nc_state <- map_data('county', 'north carolina') |>
      select(lon = long, lat, group, id = subregion)

    # building the plot ----
    ggplot(data = nc_state, aes(x = lon, y = lat)) + 
      geom_polygon(aes(group = group), fill = '#7BAFD4', color = 'white') +
      coord_quickmap() +
      theme_void()
   })
  }

# Run the app ----
shinyApp(ui = ui, server = server)
