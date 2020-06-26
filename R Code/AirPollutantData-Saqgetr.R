# Set Working Direction
setwd("C:\\Users\\csy\\OneDrive - Imperial College London\\MSc Tansport\\Dissertation\\Environment\\Coronavirus\\R\\Weather Normalisation\\06-25Data(PM,O3)")

# Install saqgetr package
install.packages("saqgetr")

# Install development version of saqgetr
remotes::install_github("skgrange/saqgetr")

# Load packages
library(dplyr)
library(saqgetr)

# Import site information, so that you can check the location of monitor sites
data_sites <- get_saq_sites()


# Glimpse tibble
glimpse(data_sites)

# Get  hourly observations, verbose is used to give an indication on
# what is occuring
data_large_ish <- get_saq_observations(
  site = c( "gb0566a"), 
  start = 2015,
  verbose = TRUE
)

# Glimpse tibble, check the air pollutant and monitoring date
glimpse(data_large_ish)

# Save as csv
#write.csv(data_large_ish, "LondonBloomsbury.csv")

# Get only valid hourly data and reshape (spread)
data_city_spread <- data_large_ish %>% 
  saq_clean_observations(summary = "hour", valid_only = TRUE, spread = TRUE)

# Glimpse tibble
glimpse(data_city_spread)
write.csv(data_city_spread, "LondonBloomsbury.csv")

