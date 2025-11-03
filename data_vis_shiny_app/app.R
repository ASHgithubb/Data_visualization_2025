library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ---- Load Data ----
data_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)
df_region <- read.csv("df_regions.csv", stringsAsFactors = FALSE)


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
          style = "
            display: flex;
            justify-content: space-between;
            gap: 10px;
          ",
          lapply(6:7, function(i) {
            div(
              i,
              style = "
                background-color: #cceeff;
                border: 1px solid #00000040;
                width: 50%;
                height: 300px;
                display: flex;
                justify-content: center;
                align-items: center;
                font-size: 20px;
              "
            )
          })
        ),
        
        # Row 4: Box 8 (map)
        div(
          style = "
            display: flex;
            justify-content: space-between;
            gap: 10px;
          ",
          div(
            plotOutput("map", height = 300),
            style = "
              border: 1px solid #00000040;
              width: 50%;
              height: 300px;
            "
          ),
          
          # Box 9 placeholder
          div(
            "9",
            style = "
              background-color: #cceeff;
              border: 1px solid #00000040;
              width: 50%;
              height: 300px;
              display: flex;
              justify-content: center;
              align-items: center;
              font-size: 20px;
            "
          )
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
  
  output$map <- renderPlot({
    # --- Plot each region in a unique color ---
    plot_usmap(data = df_region, regions = "state", values = "region", color = "black") +
      scale_fill_manual(
        name = "Region",
        values = c(
          "New England" = "#6BAED6",
          "Middle Atlantic" = "#FD8D3C",
          "East North Central" = "#31A354",
          "West North Central" = "#756BB1",
          "South Atlantic" = "#E6550D",
          "East South Central" = "#636363",
          "West South Central" = "#9ECAE1",
          "Mountain" = "#74C476",
          "Pacific" = "#E377C2"
        )
      ) +
      labs(
        title = "U.S. Regions by State",
        subtitle = "Each region colored uniquely"
      ) +
      theme(legend.position = "right")
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
