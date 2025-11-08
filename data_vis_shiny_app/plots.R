histogram_discrete <- function(x, title, df, minimal_theme = TRUE) {
  p <- ggplot(df %>%
                group_by(sex, !!sym(x)) %>% 
                summarise(count = n(), .groups = "drop") %>% 
                group_by(!!sym(x)) %>%  # Fixed grouping variable
                mutate(perc = count / sum(count) * 100),
              aes(x = !!sym(x), y = perc, fill = sex)) + 
    geom_col(position = position_dodge(width = 0.9)) +
    labs(x = x, title = title, fill = "Gender") +
    coord_cartesian(ylim = c(0, 100))
  
  # Add theme conditionally
  if (minimal_theme) {
    p <- p + theme_minimal()
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.95, size = rel(0.80)))
  }
  return(p)
}


histogram_age <- function(df){
  all_ages <- unique(df$age)
  
  diverging_age <- df %>%
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
    scale_y_continuous(labels = function(x) abs(x))+
    coord_flip()
}


histogram_year <- function(df){
  complete_years <- min(df$year):max(df$year)
  
  
  diverging_year <- df %>%
    group_by(sex, year) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    group_by(year) %>%
    mutate(perc = count / sum(count) * 100) %>%
    mutate(perc_diverging = ifelse(sex == "F", -perc, perc))
  
  ggplot(diverging_year,
                   aes(x = year, y = perc_diverging, fill = sex)) + 
    geom_col() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = rel(.80))) +
    labs(x = "Year", y = "Percentage", title = "Sex responses per Year") +
    coord_cartesian(ylim = c(-100, 100))+
    scale_x_continuous(breaks = complete_years) +
    scale_y_continuous(labels = function(x) abs(x))+
    coord_flip()
  
}


temp_plot_year <- function(x, df){
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
      title = paste(x ,"Distribution across Time")
    )
  
}