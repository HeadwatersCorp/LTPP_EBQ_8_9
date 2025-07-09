################################################################################
################################################################################
####################### Basic BACI Design: Visualizations ######################
################################################################################
################################################################################

# load libraries
source("scripts/libraries.R")
source("scripts/functions/a1_visualization.R")

# load data
data <- read.csv("data/analysis_1/fledge_ratios.csv")
head(data,5); str(data); names(data) # examine


### Boxplots ###################################################################

### Fledge Ratios ~ Control/Treatment & Time Period (Before, After)

pp_b1 <- plot_baci_boxplot(
  data = data,
  yvar = PP_Fledge_Ratio_Nest,
  title = "Piping Plover Fledge Ratios (Per Nest)",
  subtitle = "Comparison of control and treatment sites",
  ylabel = "Fledge Ratios (Nest)"
); pp_b1


pp_b2 <- plot_baci_boxplot(
  data = data,
  yvar = PP_Fledge_Ratio_BPE,
  title = "Piping Plover Fledge Ratios (BPE)",
  subtitle = "Comparison of control and treatment sites",
  ylabel = "Fledge Ratios (BPE)"
); pp_b2

lt_b1 <- plot_baci_boxplot(
  data = data,
  yvar = LT_Fledge_Ratio_Nest,
  title = "Least Tern Fledge Ratios (Per Nest)",
  subtitle = "Comparison of control and treatment sites",
  ylabel = "Fledge Ratios (Nest)"
); lt_b1


lt_b2 <- plot_baci_boxplot(
  data = data,
  yvar = LT_Fledge_Ratio_BPE,
  title = "Least Tern Fledge Ratios (BPE)",
  subtitle = "Comparison of control and treatment sites",
  ylabel = "Fledge Ratios (BPE)"
); lt_b2

all_box <- grid.arrange(pp_b1, pp_b2, lt_b1, lt_b2)

# Save plots
ggsave(pp_b1, filename = "figures/analysis_1/pp_fledge_ratios_by_Treatment_Time_NEST.png", 
       width = 5, height = 5, dpi = "retina", type = "png")
ggsave(pp_b2, filename = "figures/analysis_1/pp_fledge_ratios_by_Treatment_Time_BPE.png", 
       width = 5, height = 5, dpi = "retina", type = "png")
ggsave(lt_b1, filename = "figures/analysis_1/lt_fledge_ratios_by_Treatment_Time_NEST.png", 
       width = 5, height = 5, dpi = "retina", type = "png")
ggsave(lt_b2, filename = "figures/analysis_1/lt_fledge_ratios_by_Treatment_Time_BPE.png", 
       width = 5, height = 5, dpi = "retina", type = "png")
ggsave(all_box, filename = "figures/analysis_1/all_fledge_ratios_by_Treatment_Time.png", 
       width = 10, height = 8, dpi = "retina", type = "png")

### Create trends ##############################################################

# Prepare trend data for PP_Fledge_Ratio_Nest
pp_nest_summary <- trend_plot_prep(data, PP_Fledge_Ratio_Nest)
# PP BPE
pp_bpe_summary <- trend_plot_prep(data, PP_Fledge_Ratio_BPE)
# LT_Fledge_Ratio_Nest
lt_nest_summary <- trend_plot_prep(data, LT_Fledge_Ratio_Nest)
# LT BPE
lt_bpe_summary <- trend_plot_prep(data, LT_Fledge_Ratio_BPE)


### Plot Trends ################################################################

### PP Nest
p1 <- plot_baci_trend(pp_nest_summary,
                title = "Piping Plover Fledge Ratio (Per Nest)",
                ylabel = "Mean Fledge Ratio (Nest)")

### PP BPE
p2 <- plot_baci_trend(pp_bpe_summary,
                title = "Piping Plover Fledge Ratio (BPE)",
                ylabel = "Mean Fledge Ratio (BPE)")

### LT Nest
l1 <- plot_baci_trend(lt_nest_summary,
                title = "Least Tern Fledge Ratio (Per Nest)",
                ylabel = "Mean Fledge Ratio (Nest)")

### LT BPE
l2 <- plot_baci_trend(lt_bpe_summary,
                title = "Least Tern Fledge Ratio (BPE)",
                ylabel = "Mean Fledge Ratio (BPE)")


# Piping Plover Plots
pp_trends <- grid.arrange(p1, p2); pp_trends

# Least Tern Plots
lt_trends <- grid.arrange(l1, l2); lt_trends

# Save
ggsave(pp_trends, filename = "figures/analysis_1/pp_fledge_ratio_trends.png", 
       width = 8, height = 6, dpi = "retina", type = "png") # pp
ggsave(lt_trends, filename = "figures/analysis_1/lt_fledge_ratio_trends.png", 
       width = 8, height = 6, dpi = "retina", type = "png") # lt


