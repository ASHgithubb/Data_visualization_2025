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
  actionButton("reset", "Reset",
               icon = icon("refresh")),
  
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
              plotOutput("bar_year", click= "bar_year_click")
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
  
  ##### Filtering the data #######
  filtered_data <- reactiveVal(data_list)
  clicked_info <- reactiveVal(list(variable = NULL, category=NULL))
  
  observeEvent(input$bar_year_click,{
    y_click <- input$bar_year_click$y
    cat("Y click position:", y_click, "\n")
    
    unique_years <- sort(unique(df_clean$year_ranges))
    cat("Available year ranges:", paste(unique_years), "\n")
    
    
    category_index <- length(unique_years) - round(y_click) + 1
    cat("Category index:", category_index, "\n")
    
    if(category_index >= 1 && category_index <= length(unique_years)){
      selected_year_range <- unique_years[category_index]
      cat("Selected year range:", selected_year_range, "\n")
      
      clicked_info(list(variable = "year_ranges", category= selected_year_range))
      
      filtered_df <- df_clean  %>% filter(year_ranges == selected_year_range)
      filtered_data(filtered_df)
      cat("Filtered dataset now has", nrow(filtered_df), "rows\n")
    }
  })
  
  
  observeEvent(input$reset,{
    filtered_data(data_list)
    clicked_info(list(variable=NULL, category=NULL))
    cat("Data reset to full dataset\n")
  })
  
  df_hist <- reactive({filtered_data()[["df_clean"]]})
  
  
  # Bar chart for Box 1 (Education Groups)
  education_levels <- c("3rd grade or less", 
                        "4th to 7th grade", 
                        "8th to 11th grade", 
                        "12th to 3 yrs of college", 
                        "4 to 7 yrs of college", 
                        "8+ yrs of college")
  
  output$bar_educ <- renderPlot({
    histogram_discrete(x = "educ", title = "Education Groups", df = df_hist(), levels=education_levels)
  })
  
  # Bar chart for Box 2 (Happiness Groups)
  happiness_levels <- c("Not too happy", "Pretty happy", "Very happy")
  output$bar_happy <- renderPlot({
    histogram_discrete(x = "happiness", title = "Happiness Groups", df = df_hist(), levels = happiness_levels)
  })
  
  # Bar chart for Box 3 (Race Groups)
  race_levels <- c("White", "Black", "Other")
  output$bar_race <- renderPlot({
    histogram_discrete(x = "race", title = "Race Groups", df = df_hist(), levels=race_levels)
  })
  
  # Bar chart for Box 4 (Marital Groups)
  output$bar_marital <- renderPlot({
    histogram_discrete(x = "marital", title = "Marriage Type", df = filtered_data())
  })
  
  # Bar chart for Box 5 (Work Group)
  work_levels <- c("Working full time", "Working part time", "Unemployed", "With a job, but home", 
                   "Retired", "In school", "Keeping house", "Other")
  output$bar_work <- renderPlot({
    histogram_discrete(x = "work", title = "Working Classes", df = df_hist(), levels=work_levels)
  })
  
  # Bar chart for Age (continuous)
  output$bar_age <- renderPlot({
    histogram_continuous(x = "age_ranges", title = "Age ranges", df = df_hist())
  })
  
  # Bar chart for Year (continuous)

  output$bar_year <- renderPlot({
    current_click <- clicked_info()
    
    if (!is.null(current_click$variable) && current_click$variable == "year_ranges"){
      histogram_continuous(x = "year_ranges", title = "Year ranges", 
                           df = df_clean, selected_category = current_click$category)
    } else{
      histogram_continuous(x = "year_ranges", title = "Year ranges", 
                           df = df_hist())
    }
   
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
  
  df_clean <- df_clean
  
  # Reactive rendering of all maps
  
  # Add this to see what's in your data
  
  observe({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Marital Status" = "marital",
                       "Race" = "race",
                       "Work" = "work")
    
    df_final <- function_filter(df_var = variable, df_data = data_list[[variable]], df_clean_filtered = df_clean)
    
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