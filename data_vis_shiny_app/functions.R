library(dplyr)
library(readr)

################## COLORS ################################
custom_colors <- c(
  '#0a3139', '#0b333b', '#0b353d', '#0c373f', '#0d3941', '#0e3b44', '#0f3d46', '#103e48', '#11404a', '#12424c', '#13444e', '#154650', '#164852', '#174a55', '#194c57', '#1a4e59', '#1c505b', '#1d525d', '#1f545f', '#215661', '#225863', '#245a65', '#265c68', '#285e6a', '#29606c', '#2b626e', '#2d6470', '#2f6672', '#316974', '#336b76', '#356d78', '#376f7b', '#39717d', '#3b737f', '#3d7581', '#3f7783', '#427985', '#447b87', '#467d89', '#487f8b', '#4b818d', '#4d838f', '#4f8591', '#528893', '#548a95', '#568c98', '#598e9a', '#5b909c', '#5e929e', '#6094a0', '#6396a2', '#6598a4', '#689aa6', '#6a9da8', '#6d9faa', '#70a1ac', '#72a3ae', '#75a5b0', '#78a7b2', '#7aa9b4', '#7dabb6', '#80adb8', '#83b0ba', '#85b2bc', '#88b4be', '#8bb6c0', '#8eb8c2', '#91bac3', '#94bcc5', '#97bec7', '#9ac1c9', '#9dc3cb', '#a0c5cd', '#a3c7cf', '#a6c9d1', '#a9cbd3', '#accdd5', '#afcfd7', '#b2d1d9', '#b5d4da', '#b8d6dc', '#bcd8de', '#bfdae0', '#c2dce2', '#c5dee4', '#c9e0e5', '#cce2e7', '#cfe4e9', '#d3e7eb', '#d6e9ed', '#daebee', '#ddedf0', '#e1eff2', '#e4f1f4', '#e8f3f5', '#ecf5f7', '#eff7f9', '#f3f9fa', '#f7fbfc', '#fbfdfd', '#ffffff', '#fefcfc', '#fdf9f9', '#fdf7f6', '#fcf4f3', '#fcf1f0', '#fbeeec', '#faebe9', '#fae9e6', '#f9e6e3', '#f8e3e0', '#f7e0dd', '#f7deda', '#f6dbd7', '#f5d8d4', '#f4d5d1', '#f3d3ce', '#f2d0cb', '#f1cdc8', '#f0cac5', '#efc8c2', '#eec5bf', '#edc2bc', '#ecc0b9', '#ebbdb6', '#eabab3', '#e9b8b1', '#e8b5ae', '#e6b2ab', '#e5b0a8', '#e4ada5', '#e2aba3', '#e1a8a0', '#e0a59d', '#dea39a', '#dda098', '#db9e95', '#da9b92', '#d99990', '#d7968d', '#d5948a', '#d49188', '#d28f85', '#d18c83', '#cf8a80', '#cd877d', '#cc857b', '#ca8378', '#c88076', '#c77e74', '#c57b71', '#c3796f', '#c1776c', '#bf746a', '#be7268', '#bc7065', '#ba6d63', '#b86b61', '#b6695e', '#b4675c', '#b2645a', '#b06257', '#ae6055', '#ac5e53', '#aa5b51', '#a8594f', '#a6574d', '#a4554a', '#a25348', '#a05046', '#9d4e44', '#9b4c42', '#994a40', '#97483e', '#95463c', '#93443a', '#904238', '#8e4036', '#8c3e34', '#8a3c32', '#873a31', '#85382f', '#83362d', '#80342b', '#7e3229', '#7c3028', '#792e26', '#772d24', '#742b22', '#722921', '#70271f', '#6d251d', '#6b231c', '#68221a', '#662019', '#631e17', '#611d15', '#5e1b14', '#5c1912', '#5a1811', '#57160f'
  #'#67001f', '#b2182b', '#d6604d', '#f4a582', '#fddbc7','#f7f7f7', '#d1e5f0',  '#92c5de', '#4393c3', '#2166ac', '#053061'
  )

# Create numeric palette for -100 to 100
region_palette <- colorNumeric(
  palette = custom_colors,
  domain = c(-20,0,20),   # important: fixed domain
  na.color = "transparent"
)

# Loading data
df_happy <- read.csv("data_happy.csv", stringsAsFactors = FALSE)
df_educ <- read.csv("data_educ.csv", stringsAsFactors = FALSE)
df_marital <- read.csv("data_marital.csv", stringsAsFactors = FALSE)
df_clean <- read.csv("df_clean.csv", stringsAsFactors = FALSE)
map_data <- sf::read_sf("data_map.shp")

################## HISTOGRAM PLOTS ################################

# Discrete values
histogram_discrete <- function(x, title, df) {
  p <- ggplot(df %>%
                group_by(sex, !!sym(x)) %>% 
                summarise(count = n(), .groups = "drop") %>% 
                group_by(sex) %>%  # Fixed grouping variable
                mutate(perc = count / sum(count) * 100),
              aes(x = !!sym(x), y = perc, fill = sex)) + 
    geom_col(position = position_dodge(width = 0.9)) +
    labs(x = x, title = title, fill = "Gender") +
    coord_cartesian(ylim = c(0, 100))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.95, size = rel(0.80)))
  return(p)
}



# continuous values
histogram_continuous <- function(x, title, df) {
  #all_unique <- unique(!!sym(x))
  if (x=="age")
    breaks <- unique(df$age)
  else if (x=="year")
    breaks <- min(df$year):max(df$year)
  p <- ggplot(df %>%
                group_by(sex, !!sym(x)) %>% 
                summarise(count = n(), .groups = "drop") %>% 
                group_by(sex) %>%  # Fixed grouping variable
                mutate(perc = count / sum(count) * 100)%>%
                mutate(perc_diverging = ifelse(sex == "F", -perc, perc)),
              aes(x = !!sym(x), y = perc_diverging, fill = sex)) + 
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = rel(.80))) +
    labs(x = x, y = "Percentage", title = title, fill = "Gender") +
    coord_cartesian(ylim = c(-100, 100))+
    theme_minimal()+
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(labels = function(x) abs(x))+
    coord_flip()
  return(p)
}

################## MAP ################################

# Define levels
years <- c("1970's", "1980's", "1990's", "2000's", "2010's", "2020's")
decades <- list(
  "1970's" = 1970:1979,
  "1980's" = 1980:1989,
  "1990's" = 1990:1999,
  "2000's" = 2000:2009,
  "2010's" = 2010:2019,
  "2020's" = 2020:2029
)
regions <- c("New England", "Middle Atlantic", "East North Central", "West North Central"
             #, "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"
             )
happiness_levels <- c("Not too happy", "Pretty happy", "Very happy")
educ_levels <- c("4 years of college","10th grade","12th grade", "5 years of college","2 years of college","1 year of college","6th grade","9th grade","11th grade","7th grade","3 years of collage","8 or more years of college", "6 years of college","3rd grade" , "2nd grade","4th grade","5th grade","7 years of college","1st grade","No formal schooling" )
marital_levels <- c("Never married", "Married", "Divorced", "Widowed", "Sepreated")

#Function to create data
function_filter <- function (df_var, df_data) {
  
  df_sym <- rlang::sym(df_var)  # convert string → symbol
  
  df_simple <- purrr::map_dfr(
    names(decades),
    \(decade) {
      purrr::map_dfr(
        regions,
        \(reg) {
          df_clean %>%
            filter(year %in% decades[[decade]], region == reg) %>%
            group_by(sex, !!df_sym) %>%
            summarise(count = n(), .groups = "drop") %>%
            group_by(sex) %>%
            mutate(
              perc = count / sum(count) * 100,
              region = reg,
              year = decade
            ) 
        }
      )
    }
  )
  
  # Calculating percentage differences
  df_all <- df_data %>%
    left_join(df_simple, by = c("sex", df_var, "region", "year")) %>%
    mutate(
      count = tidyr::replace_na(count, 0),
      perc  = tidyr::replace_na(perc, 0)
    ) %>%
    arrange(year, region, sex, !!df_sym)
  
  df_dif <- df_all %>%
    select(-"count")%>%
    tidyr::pivot_wider(
      names_from = sex,
      values_from = perc
    ) %>%
    mutate(percent = `F` - `M`)  # difference between male and female percentages
  
  df_dif
} 


# Function to create shapefiles for all year × happiness combinations
create_shapefiles <- function(df_var, df_data, map_data, years, levels, out_dir = "data_vis_shiny_app") {
  
  df_sym <- rlang::sym(df_var)  # convert string → symbol
  
  # Ensure output directory exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  file_counter <- 1
  
  for (h in levels) {
    for (y in years) {
      
      # Filter data for this combination
      df_filtered <- df_data %>%
        filter(year == y, !!df_sym == h)
      
      map_joined <- map_data %>%
        left_join(df_filtered, by = "region") %>%
        st_as_sf() %>%
        st_transform(4326)
      
      # # Join with map geometry
      # map_joined <- df_filtered %>%
      #   select(region, percent, year, !!df_sym) %>%
      #   left_join(st_drop_geometry(map_data), by = "region") %>%
      #   left_join(map_data, by = "region") %>%
      #   st_as_sf()  # Ensure it is an sf object
      # 
      # # Transform CRS to WGS84 for Leaflet
      # map_joined <- st_transform(map_joined, crs = 4326)
      
      # Define filename
      #file_name <- file.path(out_dir, paste0("map_happy_", file_counter, ".shp"))
      
      file_name <- file.path(out_dir, sprintf("map_%s_%d.shp", df_var, file_counter))
      
      # Write shapefile
      st_write(map_joined, file_name, delete_dsn = TRUE, quiet = TRUE)
      
      cat("Written:", file_name, "\n")
      
      file_counter <- file_counter + 1
    }
  }
  
  cat("All shapefiles created.\n")
}
