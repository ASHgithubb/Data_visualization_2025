library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ---- Load Data ----
df_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)


# Define UI ----
ui <- page_sidebar(
  title = "Interactive Visualization Project",
  sidebar = sidebar("Sidebar"),
  
  # Main card container
  # things to do in card: https://rstudio.github.io/bslib/articles/cards/index.html
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
          div(
            # to use for large nr variables
            # https://shiny.posit.co/r/articles/build/selectize/#server-side-selectize
            selectInput(
            "var",
            label="Choose a variable to display",
            choices=
              c(
                "Happiness",
                "Education",
                "Race"
              )
            ),
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
          # Box 1 (contains bar chart / Education)
          div(
            plotOutput("bar_educ", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          # Box 2 (contains bar chart / Happiness)
          div(
            plotOutput("bar_happy", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          
          # Box 3 (contains bar chart / Race)
          div(
            plotOutput("bar_race", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          # Box 3 (contains bar chart / marital)
          div(
            plotOutput("bar_marital", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          
          div(
            plotOutput("bar_age", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),
          
          div(
            plotOutput("bar_work", height = "100%", width = "100%"),
            style = "
              border: 1px solid #00000040;
              width: 260px;
              height: 200px;
              display: flex;
              justify-content: center;
              align-items: center;
            "
          ),

          #lapply(5:5, function(i) {
          #  div(
          #    i,
          #    style = "
          #      background-color: #cceeff;
          #      border: 1px solid #00000040;
          #      width: 260px;
          #      height: 200px;
          #      display: flex;
          #      justify-content: center;
          #      align-items: center;
          #      font-size: 20px;
          #    "
          # )
          #})
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
  # Bar chart for Box 1 (Education Groups)
  output$bar_educ <- renderPlot({
    ggplot(df_clean %>%
             group_by(sex, educ) %>% 
             summarise(count = n(), .groups = "drop") %>% 
             group_by(educ) %>%
             mutate(perc = count / sum(count) * 100),
           aes(x = educ, y=perc, fill=sex)) + 
      geom_col(position = position_dodge(width = 0.9))+
      theme_minimal() + 
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(title = "Education Groups", fill="Gender") + 
      coord_cartesian(ylim = c(0, 100))
    
  })
  
  
  # Bar chart for Box 2 (Happiness Groups)
  output$bar_happy <- renderPlot({
  ggplot(df_clean%>%
         group_by(sex, happiness) %>% 
          summarise(count = n(), .groups = "drop") %>% 
          group_by(happiness) %>%
          mutate(perc = count / sum(count) * 100),
          aes(x = happiness, y=perc, fill=sex)) + 
    geom_col(position = position_dodge(width = 0.9))+
    #theme_minimal() + 
      #https://ggplot2.tidyverse.org/reference/element.html
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.95, size = rel(.80)))+
    labs(title = "Happiness Groups", fill="Gender") + 
    coord_cartesian(ylim = c(0, 100))
    
  })
  
  # Bar chart for Box 3 (Race Groups)
  output$bar_race <- renderPlot({
    ggplot(df_clean%>%
             group_by(sex, race) %>% 
             summarise(count = n(), .groups = "drop") %>% 
             group_by(race) %>%
             mutate(perc = count / sum(count) * 100),
           aes(x = race, y=perc, fill=sex)) + 
    geom_col(position = position_dodge(width = 0.9))+
      theme_minimal() + 
      labs(x = "race", title = "Race Groups") + 
      coord_cartesian(ylim = c(0, 100))
    
  })
  
  output$bar_marital <- renderPlot({
    ggplot(df_clean%>%
             group_by(sex, marital) %>% 
             summarise(count = n(), .groups = "drop") %>% 
             group_by(marital) %>%
             mutate(perc = count / sum(count) * 100),
           aes(x = marital, y=perc, fill=sex)) + 
      geom_col(position = position_dodge(width = 0.9))+
      theme_minimal() + 
      labs(x = "marital", title = "Marraige Type") + 
      coord_cartesian(ylim = c(0, 100))
  })
  
  output$bar_age <- renderPlot({
    all_ages <- unique(df_clean$age)
    diverging_age <- df_clean %>%
      group_by(sex, age) %>% 
      summarise(count = n(), .groups = "drop") %>% 
      group_by(age) %>%
      mutate(perc = count / sum(count) * 100) %>%
      mutate(perc_diverging = ifelse(sex == "F", -perc, perc))
    ggplot(diverging_age,
           aes(x = age, y = perc_diverging, fill = sex)) + 
      geom_col() +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = rel(.80))) +
      labs(x = "Age", y = "Percentage", title = "Sex responses per Age") +
      coord_cartesian(ylim = c(-100, 100))+
      scale_x_continuous(breaks = all_ages) +
      scale_y_continuous(labels = function(x) abs(x))
  })
  
  output$bar_work <- renderPlot({
    ggplot(df_clean%>%
             group_by(sex, work) %>% 
             summarise(count = n(), .groups = "drop") %>% 
             group_by(work) %>%
             mutate(perc = count / sum(count) * 100),
           aes(x = work, y=perc, fill=sex)) + 
      geom_col(position = position_dodge(width = 0.9))+
      theme_minimal() + 
      labs(x = "work", title = "Working Classes") + 
      coord_cartesian(ylim = c(0, 100))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
