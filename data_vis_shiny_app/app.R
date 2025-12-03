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
              plotOutput("bar_age", click = "bar_age_click")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_happy", click = "bar_happy_click")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
                plotOutput("bar_educ", click = "bar_educ_click")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_race", click = "bar_race_click")
            )
          ),
          card(
            full_screen=TRUE,
            card_body(
              plotOutput("bar_marital", click = "bar_marital_click")
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
  filtered_data <- reactiveVal(df_clean)
  clicked_info <- reactiveVal(list(variable = NULL, category=NULL))
  
  observeEvent(input$bar_year_click,{
    yr_click <- input$bar_year_click$y

    unique_years <- sort(unique(df_clean$year_ranges))
    
  
    category_index <- length(unique_years) - round(yr_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_years)){
      selected_year_range <- unique_years[category_index]
      
      clicked_info(list(variable = "year_ranges", category= selected_year_range))
      
      filtered_df <- df_clean  %>% filter(year_ranges == selected_year_range)
      filtered_data(filtered_df)
    }
  })
  
  observeEvent(input$bar_age_click,{
    age_click <- input$bar_age_click$y
    
    unique_age <- sort(unique(df_clean$age_ranges))
    
    
    category_index <- length(unique_age) - round(age_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_age)){
      selected_age_range <- unique_age[category_index]
      
      clicked_info(list(variable = "age_ranges", category= selected_age_range))
      
      filtered_df <- df_clean  %>% filter(age_ranges == selected_age_range)
      filtered_data(filtered_df)
    }
  })
  
  
  observeEvent(input$bar_happy_click,{
    happy_click <- input$bar_happy_click$y
    
    unique_happy <- sort(unique(df_clean$happiness))
    
    
    category_index <- length(unique_happy) - round(happy_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_happy)){
      selected_happy <- unique_happy[category_index]
      
      clicked_info(list(variable = "happy", category= selected_happy))
      
      filtered_df <- df_clean  %>% filter(happiness == selected_happy)
      filtered_data(filtered_df)
    }
  })
  
  observeEvent(input$bar_educ_click,{
    educ_click <- input$bar_educ_click$y
    
    unique_educ <- sort(unique(df_clean$educ))
    
    
    category_index <- length(unique_educ) - round(educ_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_educ)){
      selected_educ_range <- unique_educ[category_index]
      
      clicked_info(list(variable = "educ", category= selected_educ_range))
      
      filtered_df <- df_clean  %>% filter(educ == selected_educ_range)
      filtered_data(filtered_df)
    }
  })
  
  observeEvent(input$bar_race_click,{
    race_click <- input$bar_race_click$y
    
    unique_race <- sort(unique(df_clean$race))
    
    
    category_index <- length(unique_race) - round(race_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_race)){
      selected_race_range <- unique_race[category_index]
      
      clicked_info(list(variable = "race", category= selected_race_range))
      
      filtered_df <- df_clean  %>% filter(race == selected_race_range)
      filtered_data(filtered_df)
    }
  })
  
  
  observeEvent(input$bar_marital_click,{
    marital_click <- input$bar_marital_click$y
    
    unique_race <- sort(unique(df_clean$marital))
    
    
    category_index <- length(unique_marital) - round(marital_click) + 1
    
    if(category_index >= 1 && category_index <= length(unique_marital)){
      selected_marital_range <- unique_marital[category_index]
      
      clicked_info(list(variable = "marital", category= selected_marital_range))
      
      filtered_df <- df_clean  %>% filter(marital == selected_marital_range)
      filtered_data(filtered_df)
    }
  })


  
  
  
  observeEvent(input$reset,{
    filtered_data(df_clean)
    clicked_info(list(variable=NULL, category=NULL))
    cat("Data reset to full dataset\n")
  })
  
  
  
  
  
  
  
  # Bar chart for Box 1 (Education Groups)
  education_levels <- c("Kindergarten", "Elementary School", 
                        "High School", "College Degree", 
                        "Bachelor's Degree", "Master's Degree", "Advanced Professional Degree")
  
  output$bar_educ <- renderPlot({
    histogram_discrete(x = "educ", title = "Education Groups", df = filtered_data(), levels=education_levels)
  })
  
  # Bar chart for Box 2 (Happiness Groups)
  happiness_levels <- c("Not too happy", "Pretty happy", "Very happy")
  filtered_levels <- 
  output$bar_happy <- renderPlot({
    histogram_discrete(x = "happiness", title = "Happiness Groups", df = filtered_data(), levels = happiness_levels)
  })
  
  # Bar chart for Box 3 (Race Groups)
  race_levels <- c("White", "Black", "Other")
  output$bar_race <- renderPlot({
    histogram_discrete(x = "race", title = "Race Groups", df = filtered_data(), levels=race_levels)
  })
  
  # Bar chart for Box 4 (Marital Groups)
  marital_levels <- c("Married", "Widowed", "Divorced", "Sepreated", "Never married")
  output$bar_marital <- renderPlot({
    histogram_discrete(x = "marital", title = "Marriage Type", df = filtered_data(), levels=marital_levels)
  })
  
  # Bar chart for Box 5 (Work Group)
  work_levels <- c("Working full time", "Working part time", "Unemployed", "With a job, but home", 
                   "Retired", "In school", "Keeping house", "Other")
  output$bar_work <- renderPlot({
    histogram_discrete(x = "work", title = "Working Classes", df = filtered_data(), levels=work_levels)
  })
  
  # Bar chart for Age (continuous)
  output$bar_age <- renderPlot({
    histogram_continuous(x = "age_ranges", title = "Age ranges", df = filtered_data())
  })
  
  # Bar chart for Year (continuous)

  output$bar_year <- renderPlot({
    current_click <- clicked_info()
    
    if (!is.null(current_click$variable) && current_click$variable == "year_ranges"){
      histogram_continuous(x = "year_ranges", title = "Year ranges", 
                           df = df_clean, selected_category = current_click$category)
    } else{
      histogram_continuous(x = "year_ranges", title = "Year ranges", 
                           df = filtered_data())
    }
   
    })
  
  
  # Map plot
  # Render map grid dynamically
  output$map_grid <- renderUI({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Marital Status" = "marital")
    
    levels_selected <- level_list[[variable]]
    n_rows <- length(levels_selected)
    n_cols <- length(years)
    n_maps <- n_rows * n_cols
    
    grid_css <- sprintf("
      display:grid;
      grid-template-columns: 60px repeat(%d, 1fr);
      grid-template-rows: 40px repeat(%d, 180px); #40 px is the labels row
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
        tags$span("-20%"),
        tags$span("0%"),
        tags$span("20%")
      )
    )
  })
  
  # Reactive rendering of all maps
  observe({
    variable <- switch(input$var,
                       "Happiness" = "happiness",
                       "Education" = "educ",
                       "Marital Status" = "marital")
    
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