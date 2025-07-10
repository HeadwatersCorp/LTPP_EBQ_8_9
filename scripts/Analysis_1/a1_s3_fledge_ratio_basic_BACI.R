################################################################################
################################################################################
########################### Basic BACI Analysis ################################
################################################################################
################################################################################

# load libraries
source("scripts/libraries.R")
source("scripts/functions/a1_baci.R")

# load data
data <- read.csv("data/analysis_1/fledge_ratios.csv")
head(data,5); str(data); names(data) # examine

### The Basic BACI #############################################################

# BACI Effect: (Ŷ_TA - Ŷ_TB) - (Ŷ_CA - Ŷ_CB)

baci_summary <- data %>%
  group_by(Type, Time) %>%
  summarise(
    mean_ratio = mean(PP_Fledge_Ratio_Nest, na.rm = TRUE),
    .groups = "drop"
  )

# Pull mean ratio values from treatments and time periods
m_tb <- baci_summary %>% 
  filter(Type == "Treatment", 
         Time == "Before") %>% 
  pull(mean_ratio)

m_ta <- baci_summary %>% 
  filter(Type == "Treatment", 
         Time == "After")  %>% 
  pull(mean_ratio)

m_cb <- baci_summary %>% 
  filter(Type == "Control",   
         Time == "Before") %>% 
  pull(mean_ratio)

m_ca <- baci_summary %>% 
  filter(Type == "Control",   
         Time == "After")  %>% 
  pull(mean_ratio)

# Compute BACI effect
baci_effect <- (m_ta - m_tb) - (m_ca - m_cb) # (-,0,+) = (negative, zero, postive effect)

# Output
cat("Treatment Before:", round(m_tb, 3), "\n")
cat("Treatment After:", round(m_ta, 3), "\n")
cat("Control Before:", round(m_cb, 3), "\n")
cat("Control After:", round(m_ca, 3), "\n")
cat("BACI Effect:", round(baci_effect, 3), "\n") 


### Wilcoxon Rank test #########################################################

# Run tests and bind results
wilcox_table <- bind_rows(
  get_wilcox_results(filter(data, Type == "Control"), PP_Fledge_Ratio_Nest ~ Time, "Control Before vs After"),
  get_wilcox_results(filter(data, Type == "Treatment"), PP_Fledge_Ratio_Nest ~ Time, "Treatment Before vs After"),
  get_wilcox_results(filter(data, Time == "Before"), PP_Fledge_Ratio_Nest ~ Type, "Control vs Treatment (Before)"),
  get_wilcox_results(filter(data, Time == "After"), PP_Fledge_Ratio_Nest ~ Type, "Control vs Treatment (After)")
)

# View table
pander(wilcox_table)

### Bootstrapping BACI #########################################################


# Bootstrap BACI function
bootstrap_baci <- function(data, n_iter = 1000) {
  baci_values <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    # Resample with replacement
    boot_data <- data %>% 
      group_by(Type, Time) %>%
      sample_frac(replace = TRUE) %>%
      ungroup()
    
    # Compute group means
    means <- boot_data %>%
      group_by(Type, Time) %>%
      summarise(
        mean_ratio = mean(PP_Fledge_Ratio_Nest, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Extract group means
    m_tb <- means %>% filter(Type == "Treatment", Time == "Before") %>% pull(mean_ratio)
    m_ta <- means %>% filter(Type == "Treatment", Time == "After")  %>% pull(mean_ratio)
    m_cb <- means %>% filter(Type == "Control",   Time == "Before") %>% pull(mean_ratio)
    m_ca <- means %>% filter(Type == "Control",   Time == "After")  %>% pull(mean_ratio)
    
    # Compute BACI
    baci_values[i] <- (m_ta - m_tb) - (m_ca - m_cb)
  }
  
  return(baci_values)
}

# Run bootstrap
set.seed(123)  # for reproducibility
baci_boot <- bootstrap_baci(data, n_iter = 1000)

# Estimate 95% CI
ci <- quantile(baci_boot, probs = c(0.025, 0.975))
mean_baci <- mean(baci_boot)

# Print results
cat("Bootstrap BACI Mean: ", round(mean_baci, 3), "\n")
cat("95% Confidence Interval: [", round(ci[1], 3), ", ", round(ci[2], 3), "]\n")



# Turn bootstrap samples into a data frame
baci_df <- data.frame(BACI = baci_boot)

# Calculate 95% CI again for plotting reference lines
ci <- quantile(baci_boot, probs = c(0.025, 0.975))
mean_baci <- mean(baci_boot)

# Plot
ggplot(baci_df, aes(x = BACI)) +
  geom_histogram(color = "white", fill = "#2A9D8F", bins = 40, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = mean_baci, color = "#E76F51", size = 1.2) +
  geom_vline(xintercept = ci, linetype = "dotted", color = "gray40", size = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Bootstrap Distribution of BACI Effect",
    subtitle = paste0("Mean = ", round(mean_baci, 3), 
                      " | 95% CI = [", round(ci[1], 3), ", ", round(ci[2], 3), "]"),
    x = "BACI Effect",
    y = "Frequency"
  )
