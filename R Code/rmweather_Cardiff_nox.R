library(remotes)
setwd('C:\\Users\\csy\\OneDrive - Imperial College London\\MSc Tansport\\Dissertation\\Environment\\Coronavirus\\R\\Weather Normalisation\\Dissertation\\Cardiff\\Cardiff_NOx')

# Install rmweather
devtools::install_github('skgrange/rmweather')
install.packages('devtools')
devtools::install_github('davidcarslaw/openair')


# Load packages
require(devtools)
library(rlang)
library(dplyr)
library(rmweather)
library(ranger)
library(openair)

# Load data in R 
data_Cardi <- read.csv('Cardi_Meteo.csv',head=TRUE)
Cardi_meteo <- na.omit(data_Cardi)
write.csv(Cardi_meteo,"Cardiff_Meteo_DeNA.csv")
nox <- read.csv('Cardiff_Center-nox.csv',head = TRUE)

# Prepare data for modelling
Cardi_meteo$date <- as.POSIXct(strptime(Cardi_meteo$date,"%d/%m/%Y %H:%M"))
Cardi_meteo$day <- as.POSIXct(strptime(Cardi_meteo$day,"%d/%m/%Y "))

# Check the structure
str(Cardi_meteo)

# Same process for air quality data
nox$date <- as.POSIXct(strptime(nox$date,"%d/%m/%Y %H:%M"))
nox$date_end <- as.POSIXct(strptime(nox$date_end,"%d/%m/%Y %H:%M"))
str(nox)

#Acquire Date_Julian
Cardi_meteo$day_julian <- as.numeric(format(Cardi_meteo$date, "%j"))

#Save the dataset with Julian
write.csv(Cardi_meteo, 'Cardiff-Meteo-WithJulian.csv')

#Join together air quality and meteorology
mydata <- left_join(Cardi_meteo, nox, by = "date")

# Save the already prepared dataset
write.csv(mydata,'Cardi_meteo+nox.csv')


# Only use data with valid , nox will become the dependent variable
data_Cardi_prepared <- mydata %>% 
  filter(!is.na(date)) %>%
  filter(!is.na(nox)) %>%
  rename(value = nox) %>% 
  rmw_prepare_data(na.rm = TRUE)
write.csv(data_Cardi_prepared,'Cardiff_meteo+nox_DeNA.csv')

#Insert Set
set.seed(2)
data_Cardi_prepared$set = sample(2,nrow(data_Cardi_prepared),replace = TRUE, prob = c(0.7,0.3))
data_Cardi_prepared$set <- as.character(data_Cardi_prepared$set)

a = nrow(data_Cardi_prepared)

# Replacement "1" "2" into "training" "testing"
for (i in 1:a) {
 
  if(data_Cardi_prepared[i,1] == "1") {
    data_Cardi_prepared[i,1] = "training"}
  else {data_Cardi_prepared[i,1] = "testing"
  }
}
data_Cardi_prepared$set <- as.factor(data_Cardi_prepared$set)

# Check the Structure
str(data_Cardi_prepared)

# Save the total set of data
write.csv(data_Cardi_prepared, 'data_Cardiff_prepared.csv')

# Observation Trend
b <- data_Cardi_prepared[,c( "date", "value")]
smoothTrend(b, pollutant = "value")
TheilSen(b, pollutant = "value")

# Daily averaged observation trend
daily <- timeAverage(b, avg.time = "day")
smoothTrend(daily, pollutant = "value")
TheilSen(daily, pollutant = "value")

# Grow/train a random forest model and then create a meteorological normalised trend 
list_normalised <- rmw_do_all(
data_Cardi_prepared,
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
normalised <- list_normalised[["normalised"]]
write.csv(normalised, "normalised.csv")
TheilSen(normalised, pollutant = "value_predict")

# Daily averaged normalised data
Normalised_daily <- timeAverage(normalised, avg.time = "day")
write.csv(Normalised_daily,"Normalised_daily.csv")

# Smooth the daily normalised trend
smoothTrend(Normalised_daily, pollutant = "value_predict")
TheilSen(Normalised_daily, pollutant = "value_predict")
plot(Normalised_daily$date,Normalised_daily$value_predict,type = 'l')


# Save normalised time series
write.csv(list_normalised[["normalised"]],'Cardiff_normalised.csv')

# The the RF model statistics
rmw_model_statistics(list_normalised$model)


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
smoothTrend(list_normalised[["normalised"]], pollutant = "value_predict")
write.csv(list_normalised$normalised,'normalised.csv')

# Plot variable importances
list_normalised$model %>% 
  rmw_model_importance() %>% 
  rmw_plot_importance()

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

# Finding normalised trend breakpoint 
data_breakpoints <- rmw_find_breakpoints(Normalised_daily)
write.csv(data_breakpoints,"Daily Normalised_breakpoints.csv")

# Compare the prediction with obserservation
comparison <- left_join(list_normalised$normalised, nox, by = "date")
plot( Normalised_daily$date,Normalised_daily$value_predict,type = 'l', col="red" )
par(new=TRUE)
plot( daily$date, daily$value, type="l", col="green" )

write.csv(comparison, 'Comparison.csv')

# Get predict data
c <- rmw_predict_the_test_set(
  model = list_normalised$model,
  df = list_normalised$observations
) 

e <- d[,c("date", "value_predict")]

# Smooth predict
smoothTrend(c, pollutant = "value_predict")
data_breakpoints <- rmw_find_breakpoints(e)
plot(d$date,d$value_predict,type = 'l')







