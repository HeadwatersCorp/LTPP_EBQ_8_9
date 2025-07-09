################################################################################
################################################################################
###################### Analysis 1: Visualization Functions #####################
################################################################################
################################################################################

### Basic BACI-style boxplots ##################################################

plot_baci_boxplot <- function(data, yvar, title, subtitle = NULL, ylabel = NULL) {
  
  yvar_sym <- ensym(yvar)
  
  ggplot(data = data %>% mutate(Time = factor(Time, levels = c("Before", "After"))),
         aes(x = Type, y = !!yvar_sym, fill = Time)) +
    
    geom_boxplot(width = 0.6, alpha = 0.75,
                 outlier.shape = 21, outlier.size = 2,
                 outlier.stroke = 0.5, outlier.fill = "white") +
    
    geom_jitter(aes(color = Time),
                width = 0.2, alpha = 0.3, size = 1.5, show.legend = FALSE) +
    
    scale_fill_manual(values = c("Before" = "#E76F51", "After" = "#2A9D8F")) +
    scale_color_manual(values = c("Before" = "#E76F51", "After" = "#2A9D8F")) +
    
    theme_classic(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13)
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Site Type",
      y = ylabel,
      fill = "Time Period"
    )
}




### Fledge Ratio - Trend over time plot PREP ###################################

trend_plot_prep <- function(data, yvar) {
  
  yvar_sym <- ensym(yvar)
  
  bind_rows(
    # Unified summary before 2021
    data %>%
      filter(Year < 2021) %>%
      group_by(Year) %>%
      summarise(
        mean_ratio = mean(!!yvar_sym, na.rm = TRUE),
        sd_ratio = sd(!!yvar_sym, na.rm = TRUE),
        grouping = "All",
        .groups = "drop"
      ),
    
    # Split summary by Type after 2021
    data %>%
      filter(Year >= 2021) %>%
      group_by(Year, Type) %>%
      summarise(
        mean_ratio = mean(!!yvar_sym, na.rm = TRUE),
        sd_ratio = sd(!!yvar_sym, na.rm = TRUE),
        grouping = Type,
        .groups = "drop"
      )
  )
}



### Fledge Ratio Trend over time PLOT ##########################################

plot_baci_trend <- function(summary_data,
                            title = "Fledge Ratio Trend",
                            subtitle = "Unified trend: 2010–2020 | Split by treatment: 2021–2024",
                            ylabel = "Mean Fledge Ratio") {
  
  ggplot(summary_data, aes(x = Year, y = mean_ratio, color = grouping)) +
    annotate("rect", xmin = 2021, xmax = 2024.5, ymin = -Inf, ymax = Inf,
             fill = "gray90", alpha = 0.5) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_ratio - sd_ratio,
                      ymax = mean_ratio + sd_ratio),
                  width = 0.2, alpha = 0.4) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = ylabel,
      color = "Group"
    )
}
