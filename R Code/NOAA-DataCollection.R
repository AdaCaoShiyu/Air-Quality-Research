# Set working Directory
setwd("C:\\Users\\csy\\OneDrive - Imperial College London\\MSc Tansport\\Dissertation\\Environment\\Coronavirus\\R\\Weather Normalisation\\07-03Data")

# Install Packages
install.packages('devtools')
install.packages('backports')
install.packages('raster')
install.packages('jsonlite')
install.packages('tibble')
install.packages('Rcpp')
install.packages('processx')
require(devtools)
devtools::install_github('davidcarslaw/worldmet')


# Load the package
library(worldmet)

# Search the data of London
getMeta(site = "city")

## user getMeta function to search for sites
info <- getMeta(lat = 51.505, lon = 0.055)

## note code to be used in importNOAA
dat6 <- importNOAA(code = "037683-99999", year = 2015:2020)
head(dat6)
write.csv(dat6,"London2015-2020.csv")

# Same method to get data of Manchester
getMeta(site = "manchester")
info <- getMeta(lat = 53.354, lon = -2.275)

info <- getMeta(lat = 53.48, lon = -2.237)
dat4 <- importNOAA(code = "033340-99999", year = 2015)
write.csv(dat4,"Manche2015.csv")
dat4.1 <- importNOAA(code = "033340-99999", year = 2016)
write.csv(dat4.1,"Manche2016.csv")
dat4.2 <- importNOAA(code = "033340-99999", year = 2017)
write.csv(dat4.2,"Manche2017.csv")
dat4.2 <- read.csv("Manche2017.csv", header = TRUE)
dat4.3 <- importNOAA(code = "033340-99999", year = 2018)
write.csv(dat4.3,"Manche2018.csv")
dat4.4 <- importNOAA(code = "033340-99999", year = 2019)
write.csv(dat4.4,"Manche2019.csv")
dat4.5 <- importNOAA(code = "033340-99999", year = 2020)
dat4.0 <- rbind(dat4,dat4.1,dat4.2,dat4.3,dat4.4,dat4.5)
write.csv(dat4.0,"Manche2015-2020.csv")

#Get Edinburgh Data
getMeta(site = "edinburgh")
info <- getMeta(lat = 53.354, lon = -2.275)
dat7 <- importNOAA(code = "031660-99999", year = 2015:2020)
write.csv(dat7,"Edin2015-2020.csv")
dat7.1 <- importNOAA(code = "031660-99999", year = 2016)

#Get Cardiff Data
getMeta(site = "cardiff")
dat8 <- importNOAA(code = "037150-99999", year = 2015:2020)
write.csv(dat8,"Cardi2015-2020.csv")

#Get Birmingham Data
getMeta(site = "birmingham")
dat9 <- importNOAA(code = "035340-99999", year = 2015:2020)
write.csv(dat9,"Birimin2015-2020.csv")
