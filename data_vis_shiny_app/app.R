library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ---- Load Data ----
data_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)


# ---- Define UI ----
ui <- fluidPage(
  titlePanel("Interactive Visualization Project"),
  
  # Main card container (no sidebar)
  card(
    card_header("Gender distributions across regions and time"),
    card_body(
      
      # ---- CARD CONTENT ----
      div(
        style = "
          display: flex;
          flex-direction: column;
          gap: 15px;
        ",
        
        # Row 1: Box 10 (same width as one quadrant) + select input
        div(
          style = "
            display: flex;
            justify-content: flex-start;
            align-items: start;
            gap: 15px;
            width: 30%;
            height: 80px;
            display: flex;
            justify-content: start;
            align-items: start;
            font-size: 14px;
          ",
          
          
          # Variable selector inside same row
          selectInput(
            "variable",
            "Choose a variable to display:",
            choices = c(
              "Happiness Level" = "happiness",
              "Education Level" = "educ"
            )
          )
        ),
        
        # Row 2: Boxes 1–5 (quadrants)
        div(
          style = "
            display: flex;
            justify-content: space-between;
            gap: 10px;
          ",
          
          # Box 1 (education bar chart)
          div(
            plotOutput("bar_educ", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 200px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          
          # Box 2 (happiness bar chart)
          div(
            plotOutput("bar_happy", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 200px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          
          # Boxes 3–5 (placeholders)
          lapply(3:5, function(i) {
            div(
              i,
              style = "
                background-color: #cceeff;
                border: 1px solid #00000040;
                width: 200px;
                height: 200px;
                display: flex;
                justify-content: center;
                align-items: center;
                font-size: 20px;
              "
            )
          })
        ),
        
        # Row 3: Boxes 6–7
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
                display: flex;
                justify-content: center;
                align-items: center;
                font-size: 20px;"
          )
        )
      )
    )
  )

# ---- Define Server Logic ----
server <- function(input, output) {
  
  # Box 1: Education distribution
  output$bar_educ <- renderPlot({
    ggplot(data_clean, aes(x = educ, fill = sex)) +
      geom_bar(position = position_dodge(width = 0.9)) +
      coord_cartesian(ylim = c(0, 15000)) +
      theme_minimal() +
      labs(x = "Education Level", title = "Education Distribution by Gender")
  })
  
  # Box 2: Happiness distribution
  output$bar_happy <- renderPlot({
    ggplot(data_clean, aes(x = happiness, fill = sex)) +
      geom_bar(position = position_dodge(width = 0.9)) +
      coord_cartesian(ylim = c(0, 20000)) +
      theme_minimal() +
      labs(x = "Happiness", title = "Happiness Distribution by Gender")
  })
  
  # Box 8: map
  output$map <- renderLeaflet({
    leaflet(my_map) %>%
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
        values = my_map$region,
        title = "Region",
        opacity = 1
      )
  })
  
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
