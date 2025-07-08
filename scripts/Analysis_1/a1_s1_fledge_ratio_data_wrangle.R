################################################################################
################################################################################
####################### Initial Fledge Ratio Data Wrangling ####################
################################################################################
################################################################################

# last_update: 07/08/2025

# load libraries
source("scripts/libraries.R")

# data
data <- read.csv("data/raw/SiteSuccess.csv", header = T)
head(data); str(data)

################################################################################

# Select sites
data <- data %>% 
  subset(Site %in% c("OSG Lexington", "NPPD Lexington", "Dyer",
                     "Cottonwood Ranch", "Blue Hole", "Kearney Broadfoot South",
                     "Newark East", "Newark West", "Leaman", "Follmer")) %>%    # Subset to site
  mutate(LT_Fledge_Ratio_Nest = round(LT.Fledglings / LT.Nests, 4)) %>%         # Fledge Ratio / Nest
  mutate(LT_Fledge_Ratio_BPE = round(LT.Fledglings / LT.Max.Adults, 4)) %>%     # Round
  mutate(PP_Fledge_Ratio_Nest = round(PP.Fledglings / PP.Nests, 4)) %>%         # Fledge Ratio / Breeding Pair Estimate
  mutate(PP_Fledge_Ratio_BPE = round(PP.Fledglings / PP.Max.Adults, 4)) %>%     # Round
  filter(str_detect(Year, "^\\d{4}$") | str_detect(Year, "\\(O\\)")) %>%        # Years, outside monitoring only
  mutate(Year = str_extract(Year, "^\\d{4}") %>% as.integer()) %>%              # select
  subset(Year < 2025 & Year > 2009) %>%                                         # define years for study 2010-2024
  mutate(Time = ifelse(Year <= 2020, "Before", "After")) %>%                    # Before vs. After
  mutate(Type = ifelse(Site %in% c("Kearney Broadfoot South",                   # Treatment vs control
                                   "Leaman",
                                   "Newark West"), 
                                   "Treatment", "Control")) %>% 
  filter(!is.na(PP_Fledge_Ratio_Nest), !is.infinite(PP_Fledge_Ratio_Nest)) %>%  # No Ratio to calculate
  filter(!is.na(LT_Fledge_Ratio_Nest), !is.infinite(LT_Fledge_Ratio_Nest)) %>%
  .[c(1, 2, 3, 23, 19:22)]


# save
write.csv(data, file = "data/analysis_1/flede_ratios.csv")
save(data, file = "data/analysis_1/flede_ratios.RData")
