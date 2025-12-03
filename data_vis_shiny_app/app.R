library(shiny)
library(bslib)
library(ggiraph)
library(dplyr)

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
    open = TRUE,
    accordion_panel(
      title = "Histograms",
      wellPanel(
        style = "overflow-x:scroll",
        splitLayout(
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_year")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_age")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_happy")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
                plotOutput("bar_educ")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_race")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_marital")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
                plotOutput("bar_work")
            )
          ),
          cellWidths = "35%"
        )
      )
    )
  ),
  card(
    card_body(
      uiOutput("map_grid")
    )
  ),
  uiOutput("legend")
)

# ---- Define server logic ----
server <- function(input, output) {

  # Bar chart for Box 1 (Education Groups)
  output$bar_educ <- renderPlot({
    histogram_discrete(x = "educ", title = "Education Groups", df = df_clean, levels=educ_levels)
  })
  
  # Bar chart for Box 2 (Happiness Groups)
  output$bar_happy <- renderPlot({
    histogram_discrete(x = "happiness", title = "Happiness Groups", df = df_clean, levels = happiness_levels)
  })
  
  # Bar chart for Box 3 (Race Groups)
  output$bar_race <- renderPlot({
    histogram_discrete(x = "race", title = "Race Groups", df = df_clean, levels=race_levels)
  })
  
  # Bar chart for Box 4 (Marital Groups)
  output$bar_marital <- renderPlot({
    histogram_discrete(x = "marital", title = "Marriage Type", df = df_clean, levels=marital_levels)
  })
  
  # Bar chart for Box 5 (Work Group)
  output$bar_work <- renderPlot({
    histogram_discrete(x = "work", title = "Working Classes", df = df_clean, levels=work_levels)
  })
  
  # Bar chart for Age (continuous)
  output$bar_age <- renderPlot({
    histogram_continuous(x = "age_ranges", title = "Age ranges", df = df_clean)
  })
  
  # Bar chart for Year (continuous)

  output$bar_year <- renderPlot({
    histogram_continuous(x = "year_ranges", title = "Year ranges", df = df_clean)
    })
  
  # Map plot
  # Render map grid dynamically
  output$map_grid <- renderUI({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Marital Status" = "marital",
                       "Race" = "race",
                       "Work" = "work")
    
    levels_selected <- level_list[[variable]]
    n_rows <- length(levels_selected)
    n_cols <- length(years)
    n_maps <- n_rows * n_cols
    
    grid_css <- sprintf("
      display:grid;
      grid-template-columns: 60px repeat(%d, 1fr);
      grid-template-rows: 40px repeat(%d, 185px); #40 px is the labels row
      gap: 5px;
      width: 100%%;
    ", n_cols, n_rows)
    
    div(
      style = grid_css,
      
      # Column labels
      lapply(seq_along(years), function(j) {
        div(
          style = paste0("grid-column:", j+1, "; grid-row:1; text-align:center; font-size:15px; font-weight:bold;"),
          years[j]
        )
      }),
      
      # Row labels
      lapply(seq_along(levels_selected), function(i){
        div(
          style = paste0(
            "grid-column:1; grid-row:", i+1, ";",
            "writing-mode:vertical-rl; transform:rotate(180deg); font-size:15px; font-weight:bold;"
          ),
          levels_selected[i]
        )
      }),
      
      # Map outputs
      lapply(1:n_maps, function(i){
        row <- ((i-1) %/% n_cols) + 2
        col <- ((i-1) %% n_cols) + 2
        div(
          style = paste0("grid-column:", col, "; grid-row:", row, ";"),
          girafeOutput(paste0("map", i), width="100%", height="100%")
        )
      })
    )
  })
  
  # Render shared legend
  output$legend <- renderUI({
    div(
      style="width:300px; background:white; padding:10px; border:1px solid #ccc; border-radius:5px;",
      tags$h4("Difference in percent"),
      tags$div(
        style = paste0(
          "height:20px; background:linear-gradient(to right, ",
          paste(custom_colors, collapse = ","),
          "); border:1px solid #000; margin-bottom:4px;"
        )
      ),
      tags$div(
        style="display:flex; justify-content:space-between; font-size:12px; font-weight:bold;",
        tags$span("-50%"),
        tags$span("0%"),
        tags$span("50%")
      )
    )
  })
  
  # Reactive rendering of all maps
  observe({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Marital Status" = "marital",
                       "Race" = "race",
                       "Work" = "work")
    
    df_final <- function_filter(df_var = variable, df_data = data_list[[variable]])
    
    levels_selected <- level_list[[variable]]
    n_rows <- length(levels_selected)
    n_cols <- length(years)
    n_maps <- n_rows * n_cols
    
    for (i in 1:n_maps) local({
      my_i <- i
      output[[paste0("map", my_i)]] <- renderGirafe({
        row <- ((my_i - 1) %/% n_cols) + 1
        col <- ((my_i - 1) %% n_cols) + 1
        
        level_i <- levels_selected[row]
        year_i  <- years[col]
        
        df_filtered <- df_final %>%
          filter(!!sym(variable) == level_i, year == year_i)
        
        map_joined <- map_data_gg %>%
          left_join(df_filtered, by = "region")
        
        plot_map_ggiraph(map_joined)
      })
    })
  })
}


# ---- Run the App ----
shinyApp(ui = ui, server = server)