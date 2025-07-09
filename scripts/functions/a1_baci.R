################################################################################
################################################################################
########################## Analysis 1: BACI Functions ##########################
################################################################################
################################################################################

# Wilcoxon Rank Results
get_wilcox_results <- function(data, formula, group_label) {
  test <- wilcox.test(formula, data = data)
  tibble(
    Comparison = group_label,
    W_statistic = round(test$statistic, 2),
    p_value = round(test$p.value, 4),
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  )
}