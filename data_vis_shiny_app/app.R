library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(htmltools)        # For rich popup content

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
        style = "
    display: grid;
    grid-template-columns: 60px repeat(6, 1fr); /* first column for row legends, rest equally divide width */
    grid-template-rows: 30px repeat(3, 110px);   /* first row for column legends, rest for maps */
    gap: 5px;
    width: 100%;
    align-items: center;
    justify-items: center;
  ",
        
        # Column legends (years)
        lapply(1:length(years), function(j) {
          div(
            style = paste0(
              "grid-column: ", j + 1, ";",  # skip first column
              "grid-row: 1;",
              "font-weight:bold; text-align:center;",
              "font-size:10px;"
            ),
            years[j]
          )
        }),
        
        # Row legends (happiness)
        lapply(1:length(happiness_levels), function(i) {
          div(
            style = paste0(
              "grid-column: 1;",   # first column
              "grid-row: ", i + 1, ";",
              "writing-mode: vertical-rl;",  # vertical text
              "text-orientation: mixed;",
              "transform: rotate(180deg);",  # rotate text
              "font-weight:bold;",
              "font-size:10px;",
              "text-align:center;"
            ),
            happiness_levels[i]
          )
        }),
        
        # Maps (18 in total)
        lapply(1:18, function(i) {
          row <- ((i - 1) %/% 6) + 2   # +2 because first row is column legend
          col <- ((i - 1) %% 6) + 2    # +2 because first column is row legend
          div(
            style = paste0(
              "grid-column: ", col, "; grid-row: ", row, ";",
              "width: 100%; height: 100%;"
            ),
            leafletOutput(paste0("map", i), width = "100%", height = "100%")
          )
        })
      )
    ),
    uiOutput("shared_legend")
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
  # Create dynamic HTML legend
  output$shared_legend <- renderUI({
    div(
      style = "width:300px; background:white; padding:10px; border:1px solid #ccc; border-radius:5px;",
      tags$h4("Difference in percent"),
      # Gradient bar
      tags$div(
        style = paste0(
          "height:20px; background:linear-gradient(to right, ",
          paste(custom_colors, collapse = ", "),
          "); margin-bottom:5px; border:1px solid #000;"
        )
      ),
      # Dynamic tick labels: min, mid, max
      tags$div(
        style = "display:flex; justify-content: space-between; font-size:12px; font-weight:bold;",
        tags$span("-10%"),
        tags$span("0%"),
        tags$span("10%")
      )
    )
  })
  
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
    
    cat("Loading map", "\n")
    leaflet(my_map, options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>% 
      setView(lng = -98.5, lat = 39.8, zoom = 2) %>% 
      # Add polygons with hover and popup
      addPolygons(
        fillColor = ~region_palette(percent),
        color = "black",            # polygon border
        weight = 1,
        opacity = 0.7,
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333",
          fillOpacity = 0.6,
          bringToFront = TRUE
        ),
        label = ~paste0(region, ": ", round(percent, 1), "%"),  # only shows on hover
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto",
          opacity = 0.9
        )
      )
      #%>%
      # # Add a legend
      # addLegend(
      #   pal = region_palette,
      #   values = my_map$percent,
      #   opacity = 1,
      #   title = "Percent",
      #   position = "bottomright"
      # )
    })
    })
  }
  }) # end observe
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)