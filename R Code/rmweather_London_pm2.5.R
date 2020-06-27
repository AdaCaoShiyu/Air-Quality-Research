library(remotes)
getwd()
setwd('C:\\Users\\csy\\OneDrive - Imperial College London\\MSc Tansport\\Dissertation\\Environment\\Coronavirus\\R\\Weather Normalisation\\06-26(Meteo)\\pm2.5')

# Install rmweather
devtools::install_github('skgrange/rmweather')
install_github("skgrange/rmweather")
install.packages('devtools')

# Load packages
require(devtools)
library(rlang)
library(dplyr)
library(rmweather)
library(ranger)

# Load data in R, from NOAA and ARUN database 
data_london <- read.csv('LondonCity.csv',head=TRUE)
pm2.5 <- read.csv('LondonBloomsbury-pm2.5-0626.csv',head = TRUE)

# Prepare data for modelling
data_london$date <- as.POSIXct(strptime(data_london$date,"%d/%m/%Y %H:%M"))
data_london$day <- as.POSIXct(strptime(data_london$day,"%d/%m/%Y "))
str(data_london)

pm2.5$date <- as.POSIXct(strptime(pm2.5$date,"%d/%m/%Y %H:%M"))
pm2.5$date_end <- as.POSIXct(strptime(pm2.5$date_end,"%d/%m/%Y %H:%M"))
str(pm2.5)

#Acquire Date_Julian
data_london$day_julian <- as.numeric(format(data_london$date, "%j"))

#Save the dataset with Julian
write.csv(data_london, 'data_london0627.csv')

#Join together air quality and meteorology
mydata <- left_join(data_london, pm2.5, by = "date")
write.csv(mydata,'meteo+pm2.5_0627.csv')


#data_london_prepared <- data_london 
# Only use data with valid wind speeds, nox will become the dependent variable
data_london_prepared <- mydata %>% 
  filter(!is.na(date)) %>%
  filter(!is.na(pm2.5)) %>%
  rename(value = pm2.5) %>% 
  rmw_prepare_data(na.rm = TRUE)


#Insert Set
set.seed(2)
data_london_prepared$set = sample(2,nrow(data_london_prepared),replace = TRUE, prob = c(0.7,0.3))
data_london_prepared$set <- as.character(data_london_prepared$set)

a = nrow(data_london_prepared)

# Replacement
for (i in 1:a)
{ 
  if(data_london_prepared[i,1] == "1") {
    data_london_prepared[i,1] = "training"}
  else {data_london_prepared[i,1] = "testing"
  }
}
data_london_prepared$set <- as.factor(data_london_prepared$set)

# Save the total set of data
write.csv(data_london_prepared, 'data_london_prepared_pm2.5_0627.csv')

# Check the Structure
str(data_london_prepared)

# Grow/train a random forest model and then create a meteorological normalised trend 
list_normalised <- rmw_do_all(
data_london_prepared,
variables = c(
"date_unix", "day_julian", "hour", "weekday", "air_temp", "RH", "wd", "ws"
),
n_trees = 300,
n_samples = 300,
verbose = TRUE
)

# What units are in the list? 
names(list_normalised)

# Check model object's performance
print(list_normalised[["normalised"]])

# Save normalised time series
write.csv(list_normalised[["normalised"]],'normalised-pm2.5-0627.csv')
rmw_model_statistics(list_normalised$model)

# Plot variable importances
list_normalised$model %>% 
  rmw_model_importance() %>% 
  rmw_plot_importance()


# Check if model has suffered from overfitting
rmw_predict_the_test_set(
  model = list_normalised$model,
  df = list_normalised$observations
) %>% 
  rmw_plot_test_prediction()

# How long did the process take? 
list_normalised$elapsed_times

# Plot normalised trend
rmw_plot_normalised(list_normalised$normalised)
write.csv(list_normalised$normalised,'normalised-pm2.5-0627.csv')

# Investigate partial dependencies, if variable is NA, predict all
data_pd <- rmw_partial_dependencies(
  model = list_normalised$model, 
  df = list_normalised$observations,
  variable = NA
)

# Plot partial dependencies
data_pd %>% 
  filter(variable != "date_unix") %>% 
  rmw_plot_partial_dependencies()

# Compare the prediction with obserservation
comparison <- left_join(list_normalised$normalised, pm2.5, by = "date")
write.csv(comparison, 'Comparison0627.csv')
