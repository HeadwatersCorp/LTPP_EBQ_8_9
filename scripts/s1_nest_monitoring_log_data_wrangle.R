################################################################################
################################################################################
################### LTPP Nest Monitoring Data: Initital Wrangling ##############
################################################################################
################################################################################

# last_update: 07/08/2025

# load libraries
source("scripts/libraries.R")

# Load LTPP Nest monitoring log data
data <- read.csv("data/ltppNestMonitoringLog.csv", header = T) # Monitoring log
head(data, 5); str(data) # view raw data



################################################################################
### Dataset without interval (Only first date - last data of observations) #####
################################################################################

# Initial Structure Needed: (Non-interval dataset) 
  # Site (Subset to 10 sites of interest; Factor)
  # Year (Factor)
  # Nest (ID; Factor)
  # Fledge Ratio (Double)
  # Proportion Lost (Numeric)
  # Duration (Date final - Date first; Integer)
  # Camera Observations (Factor)
  # Time Monitored (Per nest; Integer)
  

data_without_interval <- data %>% 
  subset(Site %in% c("OSG Lexington", "NPPD Lexington", "Dyer",
                     "Cottonwood Ranch", "Blue Hole", "Kearney Broadfoot South",
                     "Newark East", "Newark West", "Leaman",
                     "Follmer")) %>%                                            # Select sites
  mutate(Visit.Date = as.Date(Visit.Date)) %>%                                  # Date to Date class
  mutate(camera.obs = ifelse(Camera.Observation == "true", 1, 0)) %>%           # Change false/true to 0,1
  group_by(Nest, Site) %>%                                                      # Group by Nest and Site to retain information
  summarise(first = min(Visit.Date),                                            # First date that nest is obs.
            last = max(Visit.Date),                                             # last dat that nest is obs.
            duration_n_days = as.numeric(last - first),                         # Difference in number of days
            camera_present = as.factor(camera.obs))                             # camera 0,1 to factor
