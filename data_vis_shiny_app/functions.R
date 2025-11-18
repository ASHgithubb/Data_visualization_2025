
################## HISTOGRAM PLOTS ################################

# Discrete values
histogram_discrete <- function(x, title, df, angle=45, hjust=.95, vjust=1, txt_size=0.8) {
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
    theme(axis.text.x = element_text(angle = angle, vjust = vjust, hjust = hjust, size = rel(txt_size)))
  return(p)
}



# continuous values
histogram_continuous <- function(x, title, df) {
  if (x == "age_ranges") {
    p <- ggplot(df %>%
                  group_by(sex, !!sym(x)) %>% 
                  summarise(count = n(), .groups = "drop") %>% 
                  group_by(sex) %>% 
                  mutate(perc = count / sum(count) * 100) %>%
                  mutate(perc_diverging = ifelse(sex == "F", -perc, perc)),
                aes(x = !!sym(x), y = perc_diverging, fill = sex)) + 
      geom_col(position = position_dodge(width = 0.9)) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      theme_minimal() +
      labs(x = x, y = "Percentage", title = title, fill = "Gender") +
      coord_cartesian(ylim = c(-100, 100)) +
      scale_y_continuous(labels = function(x) abs(x)) +
      coord_flip()
    
  } else if (x == "year") {
    breaks <- min(df$year):max(df$year)
    p <- ggplot(df %>%
                  group_by(sex, !!sym(x)) %>% 
                  summarise(count = n(), .groups = "drop") %>% 
                  group_by(sex) %>% 
                  mutate(perc = count / sum(count) * 100) %>%
                  mutate(perc_diverging = ifelse(sex == "F", -perc, perc)),
                aes(x = !!sym(x), y = perc_diverging, fill = sex)) + 
      geom_col(position = position_dodge(width = 0.9)) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = rel(.80))) +
      labs(x = x, y = "Percentage", title = title, fill = "Gender") +
      coord_cartesian(ylim = c(-100, 100)) +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = function(x) abs(x)) +
      coord_flip()
  }
  return(p)
}



# temporary function to show interaction
temp_plot_year <- function(x, df, title){
  ggplot(df %>%
           group_by(year, !!sym(x)) %>%
           summarise(count = n(), .groups = "drop") %>%
           group_by(year) %>%
           mutate(perc = count / sum(count) * 100),
         aes(x = year, y = perc, color = !!sym(x), group = !!sym(x))
  ) +
    geom_point(size = 2) +
    geom_line() +
    theme_minimal() +
    labs(
      x = "Year",
      y = "Percentage of Responses",
      color = x,
      title = paste(title ,"across Time")
    )
  
}

