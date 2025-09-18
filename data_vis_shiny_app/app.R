library(shiny)
library(bslib)

# Define UI ----
ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  card(
    card_header("Card header"),
    "Card content")
)

# 'page_navbar' creates a multi-page user interface that includes a navigation bar

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
