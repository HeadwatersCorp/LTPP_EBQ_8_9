################################################################################
################################################################################
################# Basic BACI Design: Fledge Ratios ~ Treatment #################
################################################################################
################################################################################

# load libraries
source("scripts/libraries.R")

# load data
data <- read.csv("data/analysis_1/fledge_ratios.csv")
head(data,5); str(data); names(data) # examine

################################################################################

# Summarizing means
data %>%
  mutate(Time = factor(Time, levels = c("Before", "After"))) %>% # Order before, after
  group_by(Type, Time) %>% # group by Control/Treatment, Before/After
  summarise(
    mean = mean(PP_Fledge_Ratio_Nest, na.rm = TRUE), # mean
    sd = sd(PP_Fledge_Ratio_Nest, na.rm = TRUE), # std. dev.
    .groups = "drop"
  )

### Create trends ##############################################################

### Piping Plover Nest Ratio
pp_nest_summary <- bind_rows( # Unified summary before 2021
  data %>%
    filter(Year < 2021) %>% # before 2021
    group_by(Year) %>%      # grouping
    summarise(
      mean_ratio = mean(PP_Fledge_Ratio_Nest, na.rm = TRUE), # mean ratio
      sd_ratio = sd(PP_Fledge_Ratio_Nest, na.rm = TRUE), # sd ratio
      grouping = "All",
      .groups = "drop"
    ),
  
  # Split summary by Type after 2021
  data %>%
    filter(Year >= 2021) %>% # after 2021
    group_by(Year, Type) %>% # group by year and treatment to show split
    summarise(
      mean_ratio = mean(PP_Fledge_Ratio_Nest, na.rm = TRUE), # mean ratio for each treatment
      sd_ratio = sd(PP_Fledge_Ratio_Nest, na.rm = TRUE), # sd of ratio for each treatment
      grouping = Type, # group by before
      .groups = "drop"
    )
)



### Piping Plover BPE Ratio
pp_bpe_summary <- bind_rows(
  data %>%
    filter(Year < 2021) %>%
    group_by(Year) %>%
    summarise(
      mean_ratio = mean(PP_Fledge_Ratio_BPE, na.rm = TRUE),
      sd_ratio = sd(PP_Fledge_Ratio_BPE, na.rm = TRUE),
      grouping = "All",
      .groups = "drop"
    ),
  
  data %>%
    filter(Year >= 2021) %>%
    group_by(Year, Type) %>%
    summarise(
      mean_ratio = mean(PP_Fledge_Ratio_BPE, na.rm = TRUE),
      sd_ratio = sd(PP_Fledge_Ratio_BPE, na.rm = TRUE),
      grouping = Type,
      .groups = "drop"
    )
)


### Least Tern Nest Ratio
lt_nest_summary <- bind_rows(
  data %>%
    filter(Year < 2021) %>%
    group_by(Year) %>%
    summarise(
      mean_ratio = mean(LT_Fledge_Ratio_Nest, na.rm = TRUE),
      sd_ratio = sd(LT_Fledge_Ratio_Nest, na.rm = TRUE),
      grouping = "All",
      .groups = "drop"
    ),
  
  data %>%
    filter(Year >= 2021) %>%
    group_by(Year, Type) %>%
    summarise(
      mean_ratio = mean(LT_Fledge_Ratio_Nest, na.rm = TRUE),
      sd_ratio = sd(LT_Fledge_Ratio_Nest, na.rm = TRUE),
      grouping = Type,
      .groups = "drop"
    )
)


### Least Tern BPE Ratio
lt_bpe_summary <- bind_rows(
  data %>%
    filter(Year < 2021) %>%
    group_by(Year) %>%
    summarise(
      mean_ratio = mean(LT_Fledge_Ratio_BPE, na.rm = TRUE),
      sd_ratio = sd(LT_Fledge_Ratio_BPE, na.rm = TRUE),
      grouping = "All",
      .groups = "drop"
    ),

  data %>%
    filter(Year >= 2021) %>%
    group_by(Year, Type) %>%
    summarise(
      mean_ratio = mean(LT_Fledge_Ratio_BPE, na.rm = TRUE),
      sd_ratio = sd(LT_Fledge_Ratio_BPE, na.rm = TRUE),
      grouping = Type,
      .groups = "drop"
    )
)



### Plot Trends ################################################################

### PP Nest
p1 <- ggplot(pp_nest_summary, aes(x = Year, y = mean_ratio, color = grouping)) +
  annotate("rect", xmin = 2021, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio),
                width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Piping Plover Fledge Ratio (Per Nest)",
    subtitle = "Unified trend: 2010-2020 | Split by treatment: 2021-2024",
    x = "Year",
    y = "Mean Fledge Ratio (Nest)",
    color = "Group"
  )

### PP BPE
p2 <- ggplot(pp_bpe_summary, aes(x = Year, y = mean_ratio, color = grouping)) +
  annotate("rect", xmin = 2021, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio),
                width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Piping Plover Fledge Ratio (Per BPE)",
    subtitle = "Unified trend: 2010-2020 | Split by treatment: 2021-2024",
    x = "Year",
    y = "Mean Fledge Ratio (BPE)",
    color = "Group"
  )

### LT Nest
l1 <- ggplot(lt_nest_summary, aes(x = Year, y = mean_ratio, color = grouping)) +
  annotate("rect", xmin = 2021, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio),
                width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Least Tern Fledge Ratio (Per Nest)",
    subtitle = "Unified trend: 2010-2020 | Split by treatment: 2021-2024",
    x = "Year",
    y = "Mean Fledge Ratio (Nest)",
    color = "Group"
  )

### LT BPE
l2 <- ggplot(lt_bpe_summary, aes(x = Year, y = mean_ratio, color = grouping)) +
  annotate("rect", xmin = 2021, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio),
                width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Least Tern Fledge Ratio (Per BPE)",
    subtitle = "Unified trend: 2010-2020 | Split by treatment: 2021-2024",
    x = "Year",
    y = "Mean Fledge Ratio (BPE)",
    color = "Group"
  )

# Piping Plover Plots
pp_trends <- grid.arrange(p1, p2); pp_trends

# Least Tern Plots
lt_trends <- grid.arrange(l1, l2); lt_trends

# Save
ggsave(pp_trends, filename = "figures/analysis_1/pp_fledge_ratio_trends.png", 
       width = 8, height = 6, dpi = "retina", type = "png") # pp
ggsave(lt_trends, filename = "figures/analysis_1/lt_fledge_ratio_trends.png", 
       width = 8, height = 6, dpi = "retina", type = "png") # lt
