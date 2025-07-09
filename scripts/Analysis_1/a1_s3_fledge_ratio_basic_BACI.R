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

# Assign values
m_tb <- baci_summary %>% filter(Type == "Treatment", Time == "Before") %>% pull(mean_ratio)
m_ta <- baci_summary %>% filter(Type == "Treatment", Time == "After")  %>% pull(mean_ratio)
m_cb <- baci_summary %>% filter(Type == "Control",   Time == "Before") %>% pull(mean_ratio)
m_ca <- baci_summary %>% filter(Type == "Control",   Time == "After")  %>% pull(mean_ratio)

# Compute BACI effect
baci_effect <- (m_ta - m_tb) - (m_ca - m_cb)

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
