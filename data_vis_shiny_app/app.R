library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(dplyr)

# ---- Load Data and Functions ----
df_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)
df_happy <- read.csv("df_happy.csv", stringsAsFactors = FALSE)
#df_map <- read.csv("map_data.csv", seperator=";")

source('functions.R')

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
        plotOutput("temp", height = "100%", width = "100%"),
        style = "
          display: flex;
          justify-content: space-between;
          gap: 10px;
          background-color: #cceeff;
          border: 1px solid #00000040;
          width: 1500px;
          height: 500px;
          justify-content: center;
          align-items: center;
          font-size: 20px;"
      )
    )
  ),
  
  card(
    card_body(
      div(
        leafletOutput("map"),
        style = "
          display: flex;
          justify-content: space-between;
          gap: 10px;
          background-color: #cceeff;
          border: 1px solid #00000040;
          width: 1500px;
          height: 500px;
          justify-content: center;
          align-items: center;
          font-size: 20px;"
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
  
  # Main plot that switches by selected variable
  output$temp <- renderPlot({
    x <- switch(
      input$var,
      "Happiness" = "happiness",
      "Education" = "educ",
      "Race" = "race",
      "Marital Status" = "marital",
      "Work" = "work"
    )
    
    title <- switch(
      input$var,
      "Happiness" = "Happiness Levels",
      "Education" = "Educational Levels",
      "Race" = "Race Distribution",
      "Marital Status" = "Marital Status",
      "Work" = "Work Distribution"
    )
    
    temp_plot_year(x = x, df = df_clean, title = title)
  })
  
  # Map plot 
  output$map <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addPolygons(
        fillColor = ~region_palette(region),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = region_palette,
        values = map_data$region,
        title = "Region",
        opacity = 1
      )
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)

