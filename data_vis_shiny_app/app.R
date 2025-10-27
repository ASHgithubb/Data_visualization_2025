library(shiny)
library(bslib)
library(ggplot2)

# ---- Load data from Rmd ----

# For demonstration purposes, let's create an example df_clean
set.seed(123)
df_clean <- data.frame(
  happiness = sample(1:10, 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  sex = sample(c("Male", "Female"), 100, replace = TRUE),
  own_income = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  income_range = sample(c("<20k", "20k-50k", "50k-100k", ">100k"), 100, replace = TRUE)
)


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
            plotOutput("bar_region", height = "100%", width = "100%"),
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
          lapply(2:5, function(i) {
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
  output$bar_region <- renderPlot({
    req(df_clean)
    ggplot(df_clean, aes(x = region)) +
      geom_bar(fill = "#2c7fb8") +
      theme_minimal(base_size = 12) +
      labs(title = "Count by Region", x = "Region", y = "Count") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
