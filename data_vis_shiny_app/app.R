library(shiny)
library(bslib)
library(ggplot2)

# ---- Load Data ----
data_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)


# Define UI ----
ui <- page_sidebar(
  title = "Interactive Visualization Project",
  sidebar = sidebar("Sidebar"),
  
  # Main card container
  card(
    card_header("Gender distributions across regions and time"),
    card_body(
      
      # ---- CARD CONTENT ----
      # The grid of boxes 1–10 goes here
      div(
        style = "
          display: flex;
          flex-direction: column;
          gap: 15px;
        ",
        
        # Row 1: Box 10 (same width as one quadrant)
        div(
          style = "
            display: flex;
            justify-content: flex-start;
          ",
          div("10",
              style = "
                background-color: #cceeff;
                border: 1px solid #00000040;
                width: 20%;
                height: 20px;
                display: flex;
                justify-content: center;
                align-items: center;
                font-size: 10px;
              ")
        ),
        
        # Row 2: Boxes 1–5 (quadrants, full row)
        div(
          style = "
            display: flex;
            justify-content: space-between;
            gap: 10px;
          ",
          # Box 1 (contains bar chart)
          div(
            plotOutput("bar_educ", height = "100%", width = "100%"),
            style = "
              background-color: #cceeff;
              border: 1px solid #00000040;
              width: 200px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          # Box 2 (contains bar chart)
          div(
            plotOutput("bar_happy", height = "100%", width = "100%"),
            style = "
              background-color: #cceeff;
              border: 1px solid #00000040;
              width: 200px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
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
        
        # Row 3: Boxes 6–7 (equal width)
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
        
        # Row 4: Boxes 8–9 (equal width)
        div(
          style = "
            display: flex;
            justify-content: space-between;
            gap: 10px;
          ",
          lapply(8:9, function(i) {
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
        )
      )
    )
  )
)

# 'page_navbar' creates a multi-page user interface that includes a navigation bar

# Define server logic ----
server <- function(input, output) {
  # Bar chart for Box 1
  output$bar_educ <- renderPlot({
    req(df_clean)
    ggplot(df_clean, aes(x = educ, fill=sex)) + 
      theme_minimal() + 
      labs(x = "educ", title = "educ groups with NA's removed") + 
      geom_bar(position = position_dodge(width = 0.9))+
      coord_cartesian(ylim = c(0, 15000))  # sets y-axis range
  })
  # Bar chart for Box 2
  output$bar_happy <- renderPlot({
    req(df_clean)
    happy_clean <- ggplot(df_clean, aes(x = happiness, fill=sex)) + 
      theme_minimal() + 
      labs(x = "Happiness", title = "Happiness groups with NA's removed") + 
      geom_bar(position = position_dodge(width = 0.9))+
      coord_cartesian(ylim = c(0, 20000))  # sets y-axis range
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
