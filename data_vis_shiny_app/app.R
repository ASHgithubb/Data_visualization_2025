library(shiny)
library(bslib)
library(ggiraph)
library(dplyr)

source('functions.R')

# ---- Define UI ----
ui <- fluidPage(
  titlePanel("Exploring Gender Differences"),
  theme = bs_theme(version = 5),
  selectInput(
    "var",
    "Choose a variable to display:",
    choices = c(
      "Happiness",
      "Education",
      "Marital Status",
      "Work"
    )
  ),
  
  accordion(
    open = TRUE,
    accordion_panel(
      title = "Distribution of variables",
      wellPanel(
        style = "overflow-x:scroll",
        splitLayout(
          card(
            card_body(
              plotOutput("bar_year")
            )
          ),
          card(
            card_body(
              plotOutput("bar_age")
            )
          ),
          card(
            card_body(
              plotOutput("bar_race")
            )
          ),
          card(
            card_body(
              plotOutput("bar_happy")
            )
          ),
          card(
            card_body(
                plotOutput("bar_educ")
            )
          ),
          card(
            card_body(
              plotOutput("bar_marital")
            )
          ),
          card(
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
    histogram_discrete(x = "work", title = "Working Classes", df = df_clean, levels=work_levels, flip = TRUE)
  })
  
  # Bar chart for Age (continuous)
  output$bar_age <- renderPlot({
    histogram_continuous(x = "age_ranges", title = "Age ranges", df = df_clean, flip = TRUE)
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
          style = paste0("grid-column:", j+1, "; grid-row:1; text-align:center; font-size:15px; font-weight:bold; justify-content:center; align-items:center;"),
          years[j]
        )
      }),
      
      # Row labels
      lapply(seq_along(levels_selected), function(i){
        div(
          style = paste0(
            "grid-column:1; grid-row:", i+1, ";",
            "writing-mode:vertical-rl; transform:rotate(180deg); font-size:15px; font-weight:bold; justify-content:center; align-items:center;"
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
      style="
      width:400px; 
      background:white; 
      padding:10px;  
      border-radius:5px;
      text-align:center;
    ",
      
      tags$h4("Gender difference in percent"),
      
      # ROW 1: left label, color bar, right label
      tags$div(
        style="
        display:grid;
        grid-template-columns: auto 1fr auto;
        column-gap:8px;
        margin-bottom:2px;   /* reduce vertical gap between rows */
        align-items: center    /* align top of cells */
      ",
        
        # left top label
        tags$div(
          "50% more males",
          style="font-size:12px; font-weight:bold; text-align:center; word-break: break-word; overflow-wrap: break-word; max-width: 10ch;"
        ),
        
        # color bar
        tags$div(
          style = paste0(
            "height:20px; 
           background:linear-gradient(to right, ",
            paste(custom_colors, collapse = ","),
            "); 
           border:1px solid #000;
            "
          )
        ),
        
        # right top label
        tags$div(
          "50% more females",
          style="font-size:12px; font-weight:bold; text-align:center; word-break: break-word; overflow-wrap: break-word; max-width: 10ch;"
        )
      ),
      
      # ROW 2: left bottom label, middle label, right bottom label
      tags$div(
        style="
        display:grid;
        grid-template-columns: auto 1fr auto;
        column-gap:8px;
        align-items:start;    /* align top of cells so close to row 1 */
      ",
        
        # left bottom label
        tags$div(
          "",
          style="font-size:12px; font-weight:bold; text-align:center;"
        ),
        
        # middle bottom label
        tags$div(
          "0% difference",
          style="text-align:center; font-size:12px; font-weight:bold;"
        ),
        
        # right bottom label
        tags$div(
          "",
          style="font-size:12px; font-weight:bold; text-align:center;"
        )
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