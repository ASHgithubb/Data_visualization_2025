library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(maps)
library(sf)
library(tidyr)
library(purrr)
library(rlang)
library(tools)
library(scales)

################## COLORS ################################
custom_colors <- c(
  '#0a3139', '#0b333b', '#0b353d', '#0c373f', '#0d3941', '#0e3b44', '#0f3d46', '#103e48', '#11404a', '#12424c', '#13444e', '#154650', '#164852', '#174a55', '#194c57', '#1a4e59', '#1c505b', '#1d525d', '#1f545f', '#215661', '#225863', '#245a65', '#265c68', '#285e6a', '#29606c', '#2b626e', '#2d6470', '#2f6672', '#316974', '#336b76', '#356d78', '#376f7b', '#39717d', '#3b737f', '#3d7581', '#3f7783', '#427985', '#447b87', '#467d89', '#487f8b', '#4b818d', '#4d838f', '#4f8591', '#528893', '#548a95', '#568c98', '#598e9a', '#5b909c', '#5e929e', '#6094a0', '#6396a2', '#6598a4', '#689aa6', '#6a9da8', '#6d9faa', '#70a1ac', '#72a3ae', '#75a5b0', '#78a7b2', '#7aa9b4', '#7dabb6', '#80adb8', '#83b0ba', '#85b2bc', '#88b4be', '#8bb6c0', '#8eb8c2', '#91bac3', '#94bcc5', '#97bec7', '#9ac1c9', '#9dc3cb', '#a0c5cd', '#a3c7cf', '#a6c9d1', '#a9cbd3', '#accdd5', '#afcfd7', '#b2d1d9', '#b5d4da', '#b8d6dc', '#bcd8de', '#bfdae0', '#c2dce2', '#c5dee4', '#c9e0e5', '#cce2e7', '#cfe4e9', '#d3e7eb', '#d6e9ed', '#daebee', '#ddedf0', '#e1eff2', '#e4f1f4', '#e8f3f5', '#ecf5f7', '#eff7f9', '#f3f9fa', '#f7fbfc', '#fbfdfd', '#ffffff', '#fefcfc', '#fdf9f9', '#fdf7f6', '#fcf4f3', '#fcf1f0', '#fbeeec', '#faebe9', '#fae9e6', '#f9e6e3', '#f8e3e0', '#f7e0dd', '#f7deda', '#f6dbd7', '#f5d8d4', '#f4d5d1', '#f3d3ce', '#f2d0cb', '#f1cdc8', '#f0cac5', '#efc8c2', '#eec5bf', '#edc2bc', '#ecc0b9', '#ebbdb6', '#eabab3', '#e9b8b1', '#e8b5ae', '#e6b2ab', '#e5b0a8', '#e4ada5', '#e2aba3', '#e1a8a0', '#e0a59d', '#dea39a', '#dda098', '#db9e95', '#da9b92', '#d99990', '#d7968d', '#d5948a', '#d49188', '#d28f85', '#d18c83', '#cf8a80', '#cd877d', '#cc857b', '#ca8378', '#c88076', '#c77e74', '#c57b71', '#c3796f', '#c1776c', '#bf746a', '#be7268', '#bc7065', '#ba6d63', '#b86b61', '#b6695e', '#b4675c', '#b2645a', '#b06257', '#ae6055', '#ac5e53', '#aa5b51', '#a8594f', '#a6574d', '#a4554a', '#a25348', '#a05046', '#9d4e44', '#9b4c42', '#994a40', '#97483e', '#95463c', '#93443a', '#904238', '#8e4036', '#8c3e34', '#8a3c32', '#873a31', '#85382f', '#83362d', '#80342b', '#7e3229', '#7c3028', '#792e26', '#772d24', '#742b22', '#722921', '#70271f', '#6d251d', '#6b231c', '#68221a', '#662019', '#631e17', '#611d15', '#5e1b14', '#5c1912', '#5a1811', '#57160f'
)

# Create color palette 
region_palette <- scale_fill_gradientn(
  colours = custom_colors,
  limits = c(-52, 52),
  name = "Percent"
)

################## LOADING DATA ################################
df_happy <- read.csv("data_happy.csv", stringsAsFactors = FALSE)
df_educ <- read.csv("data_educ.csv", stringsAsFactors = FALSE)
df_marital <- read.csv("data_marital.csv", stringsAsFactors = FALSE)
df_work <- read.csv("data_work.csv", stringsAsFactors = FALSE)
df_race <- read.csv("data_race.csv", stringsAsFactors = FALSE)
df_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)

years <- c("1970's","1980's","1990's","2000's","2010's","2020's")

decades <- list(
  "1970's" = 1970:1979,
  "1980's" = 1980:1989,
  "1990's" = 1990:1999,
  "2000's" = 2000:2009,
  "2010's" = 2010:2019,
  "2020's" = 2020:2029
)

us_regions <- list(
  "New England"        = c("CT","ME","MA","NH","RI","VT"),
  "Middle Atlantic"    = c("NJ","NY","PA"),
  "East North Central" = c("IL","IN","MI","OH","WI"),
  "West North Central" = c("IA","KS","MN","MO","NE","ND","SD")
)

regions <- names(us_regions)

happiness_levels <- c("Not too happy", "Pretty happy", "Very happy")
educ_levels <- c("Elementary School", "High School", "College Degree", "Bachelor's Degree", "Master's Degree", "Advanced Professional Degree")
marital_levels <- c("Never married", "Married", "Widowed", "Divorced", "Seperated")
race_levels <- c("White", "Black", "Other")
work_levels <- c("In school", "Working full time", "Working part time", "With a job, but home", "Keeping house", "Unemployed", "Retired", "Other")

unique(df_clean$educ)

level_list <- list(
  happiness = happiness_levels,
  educ      = educ_levels,
  marital   = marital_levels,
  race   = race_levels,
  work   = work_levels
)

################## HISTOGRAM PLOTS ################################

# Discrete values
histogram_discrete <- function(x, title, df, adjust_label=TRUE, flip=FALSE, levels=NULL) {
  if(!any(is.na(levels))){
    df_processed <- df %>%
      group_by(sex, !!sym(x)) %>% 
      summarise(count = n(), .groups = "drop") %>% 
      group_by(sex) %>% 
      mutate(perc = count / sum(count) * 100) %>% 
      mutate(!!sym(x):= factor(!!(sym(x)), levels = levels))
  }
  
  else{
    df_processed <- df %>%
      group_by(sex, !!sym(x)) %>% 
      summarise(count = n(), .groups = "drop") %>% 
      group_by(sex) %>% 
      mutate(perc = count / sum(count) * 100)
  }
  
  
  p <- ggplot(df_processed,
              aes(x = !!sym(x), y = perc, fill = sex)) + 
    geom_col(position = position_dodge(width = 0.9)) +
    labs(x = x, y = "Percentage", title = title, fill = "Gender")+
    scale_fill_manual(values= alpha(c("#d6665c","#2a94a7")))+
    theme_minimal()
  if (flip) {
    p <- p + coord_flip(ylim = c(0, 100)) 
    + theme(axis.text.y = element_text(size = 10)) 
  } else {
    p <- p + coord_cartesian(ylim = c(0, 100))
    if (adjust_label) {
      p <- p  + scale_x_discrete(labels=scales::label_wrap(8))+
        theme(axis.text.x = element_text(size=9),
              plot.title= element_text(size=20),
              text = element_text(size=15))
        #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = .95))
    }
  }
    
  return(p)
}



histogram_continuous <- function(x, title, df) {
  # Determine breaks based on x variable
  if (x == "age_ranges") {
    breaks <- unique(df$age_ranges)
  } else if (x == "year_ranges") {
    breaks <- unique(df$year_ranges)
  } else {
    # Default case for other continuous variables
    breaks <- waiver()  # Let ggplot choose breaks
  }
  
  df_processed <- df %>%
    group_by(sex, !!sym(x)) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    group_by(sex) %>%
    mutate(perc = count / sum(count) * 100)
  
  p <- ggplot(df_processed,
              aes(x = !!sym(x), y = perc, fill = sex)) + 
    geom_col(position = position_dodge(width = 0.9)) +
    labs(x = title, y = "Percentage", title = title, fill = "Gender") +
    theme_minimal() +
    scale_fill_manual(values= alpha(c("#d6665c","#2a94a7")))
  if (x %in% c("age_ranges", "year_ranges")) {
    p <- p + scale_x_discrete(breaks = breaks, labels=scales::label_wrap(8))+
      theme(axis.text.x = element_text(size=9),
        plot.title= element_text(size=20),
        text = element_text(size=15))
 
  }
  
  return(p)
}
################## MAP ################################

data_list <- list(
  happiness = df_happy,
  educ      = df_educ,
  marital   = df_marital,
  race   = df_race,
  work   = df_work
)

################## lOADING MAP ################################
# Load states as sf
states_map <- maps::map("state", plot = FALSE, fill = TRUE)
states_map_sf <- st_as_sf(states_map) %>%
  rename(geometry = geom) %>%  # may be redundant, but safe
  mutate(
    state_full = tools::toTitleCase(ID),
    state = state.abb[match(state_full, state.name)]
  )

# Define region mapping
region_states <- tibble(
  region = rep(names(us_regions), lengths(us_regions)),
  state  = unlist(us_regions)
)

# Disable s2 to avoid geometry errors
sf::sf_use_s2(FALSE)

# Merge states into regions
map_data_gg <- states_map_sf %>%
  inner_join(region_states, by = "state") %>%
  mutate(geometry = st_make_valid(geometry)) %>%   # important: fix invalid polygons
  group_by(region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

# Re-enable s2
sf::sf_use_s2(TRUE)


################## FILTER FUNCTION ################################
function_filter <- function(df_var, df_data) {
  
  df_sym <- sym(df_var)
  
  # Aggregate data for each decade and region
  df_simple <- map_dfr(
    names(decades),
    \(decade) {
      map_dfr(
        regions,
        \(reg) {
          df_clean %>%
            filter(
              year %in% decades[[decade]], region == reg) %>%
            group_by(sex, !!df_sym) %>%
            summarise(count = n(), .groups = "drop") %>%
            group_by(sex) %>%
            mutate(
              perc   = count / sum(count) * 100,
              region = reg,
              year   = decade
            )
        }
      )
    }
  )
  
  # Merge with external data and compute difference
  df_all <- df_data %>%
    left_join(df_simple, by = c("sex", df_var, "region", "year")) %>%
    mutate(
      count = replace_na(count, 0),
      perc  = replace_na(perc, 0)
    ) %>%
    arrange(year, region, sex, !!df_sym)
  
  df_all %>%
    select(-count) %>%
    pivot_wider(names_from = sex, values_from = perc) %>%
    mutate(percent = F - M)
}


### MAP PLOT FUNCTION --> DESIGN MAPS HERE ################################

#creates an interactive choropleth map using ggplot2 and ggiraph
plot_map_ggiraph <- function(map_df) {
  map_df <- map_df %>%
    #The textbox
    mutate(
      text = ifelse(percent < 0, "more males", "more females"),
      tooltip = sprintf("<div 
                   style='
                   font-size:15px; 
                   color: black;
                   background-color: rgba(211,211,211,0.8); 
                   border-radius:0px;
                   '>   
                             <b>%s</b>          
                             <br>
                             Difference: %.1f%% %s
       </div>",
                             region, abs(percent), text) #what fills in the % values
    ) 
  
  gg <- ggplot() +
    geom_sf_interactive( #from the ggiraph package
      data = map_df,
      aes(
        geometry = geometry, 
        fill = percent, #Colors the perc values from the dataframe
        tooltip = tooltip, 
        data_id = region), #identifyier for interactivity
      color = "black", #outline color
      size  = 1 #line thickness of borders
    ) +
    region_palette + #applies defined color scale
    theme_void() + #a very clean theme
    theme(legend.position = "none") #hides the default legend, as we're using a custom legend elsewhere
  
  #Convert ggplot to interactive plot
  girafe(
    ggobj = gg,
    options = list(
      opts_hover(css = #when hovering
                   "
                   #fill:rgba(211,211,211,0.8);  #region color when hovering
                    stroke:black;
                    stroke-width:5px;
                    cursor:pointer;") #cursor changes to pointer (like a button)
    )
  )
}