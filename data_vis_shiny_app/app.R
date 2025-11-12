library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(leafgl)

# ---- Load Data and Functions ----
df_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)


# ---- Define UI ----
ui <- fluidPage(
  titlePanel("Interactive Visualization Project"),
  theme = bs_theme(version = 5),
  
  selectInput(
    "var",
    "Choose a variable to display:",
    choices = c(
      "Happiness",
      "Education",
      "Race",
      "Marital Status",
      "Work"
    )
  ),
  
  accordion(
    open = FALSE,
    accordion_panel(
      title = "Histograms",
      wellPanel(
        style = "overflow-x:scroll",
        splitLayout(
          card(
            card_body(
              div(
                plotOutput("bar_educ")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_happy")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_work")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_race")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_marital")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_age")
              )
            )
          ),
          card(
            card_body(
              div(
                plotOutput("bar_year")
              )
            )
          ),
          cellWidths = "40%"
        )
      )
    )
  ),
  
  card(
    card_body(
      div(
        # Create a CSS grid: 6 columns, 3 rows
        style = "
        display: grid;
        grid-template-columns: repeat(6, 2fr);
        grid-template-rows: repeat(3, 100px);
        gap: 10px;
        width: 100%;
      ",
        
        # Generate 18 leafletOutput placeholders dynamically
        lapply(1:18, function(i) {
          leafletOutput(paste0("map", i), width = "100%", height = "100%")
        })
      )
    )
  )
)

# ---- Define server logic ----
server <- function(input, output) {
  
  # Bar chart for Box 1 (Education Groups)
  output$bar_educ <- renderPlot({
    histogram_discrete(x = "educ", title = "Education Groups", df = df_clean)
  })
  
  # Bar chart for Box 2 (Happiness Groups)
  output$bar_happy <- renderPlot({
    histogram_discrete(x = "happiness", title = "Happiness Groups", df = df_clean)
  })
  
  # Bar chart for Box 3 (Race Groups)
  output$bar_race <- renderPlot({
    histogram_discrete(x = "race", title = "Race Groups", df = df_clean)
  })
  
  # Bar chart for Box 4 (Marital Groups)
  output$bar_marital <- renderPlot({
    histogram_discrete(x = "marital", title = "Marriage Type", df = df_clean)
  })
  
  # Bar chart for Box 5 (Work Group)
  output$bar_work <- renderPlot({
    histogram_discrete(x = "work", title = "Working Classes", df = df_clean)
  })
  
  # Bar chart for Age (continuous)
  output$bar_age <- renderPlot({
    histogram_continous(x = "age", title = "Ages", df = df_clean)
  })
  
  # Bar chart for Year (continuous)
  output$bar_year <- renderPlot({
    histogram_continous(x = "year", title = "Year", df = df_clean)
  })
  
  
  # Map plot
  
  observe({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Race" = "race",
                       "Marital Status" = "marital",
                       "Work" = "work")
    my_maps_list <- switch(variable,
                       "happiness" = as.list(sprintf("map_files_happy/map_happy_%d.shp", 1:18)),
                       "educ" = rep(list("map_files/map_happy.shp"), 18),
                       "race" = rep(list("map_files/map_happy.shp"), 18),
                       "marital" = rep(list("map_files/map_happy.shp"), 18),
                       "work" = rep(list("map_files/map_happy.shp"), 18))
    
    legend_titles <- switch(variable,
                           "happiness" = rep(list("pretty happy"), 18),
                           "educ" = rep(list("pretty happy"), 18),
                           "race" = rep(list("pretty happy"), 18),
                           "marital" = rep(list("pretty happy"), 18),
                           "work" = rep(list("pretty happy"), 18))
    
  
  for (i in seq_along(my_maps_list)) {
    local({
      my_i <- i
      map_file <- my_maps_list[[my_i]]          # dataframe for this map
      legend_title_i <- legend_titles[[my_i]]
      output[[paste0("map", my_i)]] <- renderLeaflet({


    #Read the correct shapefile
    my_map <- sf::read_sf(map_file)
    

    leaflet(my_map, options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 2) %>%
      addGlPolygons(
        data = my_map,
        fillColor = ~region_palette(percent),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0(region, ": ", percent, "%"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
      })
      
    })
  }
  }) # end observe
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)


# %>%
#   addLegend(
#     pal = region_palette,
#     values = my_map$percent,
#     title = legend_title,
#     opacity = 1

