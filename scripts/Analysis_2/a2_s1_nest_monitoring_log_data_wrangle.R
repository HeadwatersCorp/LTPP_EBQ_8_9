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


# Dataset creation
data_without_interval <- data %>% 
  filter(Site %in% c("OSG Lexington", "NPPD Lexington", "Dyer",
                     "Cottonwood Ranch", "Blue Hole", "Kearney Broadfoot South",
                     "Newark East", "Newark West", "Leaman", "Follmer")) %>%    # Select Sites
  mutate(Visit.Date = as.Date(Visit.Date),                                      # Visit Date as Date
         camera.obs = ifelse(Camera.Observation == "true", 1, 0)) %>%           # Make Camera 0,1
  group_by(Nest, Site) %>%                                                      # Group by nest and site
  arrange(Visit.Date, .by_group = TRUE) %>%                                     # ensure chronological order
  mutate(
    first = min(Visit.Date),
    last = max(Visit.Date),
    duration_n_days = as.numeric(last - first),                                 # Calculate duration of nest observation
    first_nonzero_X15 = X..15.Day[which(X..15.Day != 0)[1]],                    # First non-zero value in first 15 days
    first_X15 = first(X..15.Day),                                               # First non-zero value in 15-21 days
    used_X15 = ifelse(first_X15 == 0, first_nonzero_X15, first_X15)) %>%        # between the 2 columns, select first non-zero value
  slice_max(Visit.Date, n = 1, with_ties = FALSE) %>%                       
  mutate(
    Chick.Status.Last = Chick.Status,                                           # Selecting Chick status from last date
    LT21_PP28_Last = `LT.21....PP.28.`,                                         # Last Fledge Count
    camera_present = as.factor(camera.obs),                                     # Factor camera observation
    proportion_lost = ifelse(
      used_X15 > 0,
      (used_X15 - LT21_PP28_Last) / used_X15,
      NA)) %>%                                                                  # Calculate Proportion lost, NA if no chicks ever
  select(
    Nest, Site, first, last, duration_n_days,
    Chick.Status.Last, LT21_PP28_Last, used_X15,
    proportion_lost, camera_present, Species, Survey.year) %>%
  ungroup() %>% 
  .[c(1, 12, 11, 2, 3:5, 6, 8, 7, 9, 10)]                                       # Subset columns



# Rename
names(data_without_interval) <- c("Nest", "Year", "Species", "Site",            # Rename
                                  "First_Date", "Last_Date", "Duration", 
                                  "Fate", "Chicks_First",
                                  "Chicks_Last", "Proporation_Lost",
                                  "Camera")
