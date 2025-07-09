# List of libraries to load
libraries <- c(
  "tidyverse",   # Includes ggplot2, dplyr, tidyr, readr, etc.
  "lubridate",   # For date-time manipulation
  "rjags",       # For JAGS
  "jagsUI",      # Interface for JAGS
  "brms",        # Bayesian regression models using Stan
  "MCMCglmm",    # Generalized linear mixed models
  "knitr",       # For dynamic report generation
  "data.table",  # For data manipulation efficiency
  "coda",        # For diagnostics of MCMC chains
  "sf",          # For spatial handling and analysis
  "terra",       # For spatial raster handling and analysis
  "gridExtra",   # Viewing multiple plots from ggplot on same graph
  "Cairo",      # High Quality Images
  "pander"
)

# Function to load libraries
load_libraries <- function(lib_list) {
  # Loop through each library in the list and load it
  for (lib in lib_list) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      message(paste("Installing missing package:", lib))
      install.packages(lib)
    }
    library(lib, character.only = TRUE)
  }
}

# Call the function to load libraries
load_libraries(libraries)
