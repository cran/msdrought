## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(msdrought)

## -----------------------------------------------------------------------------
library(terra)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(xts)
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") # This loads the data included in the package, but you would attach your own
infile <- terra::rast(data)

# Make a SpatRast for one point
lon <- -86.2621555581 # El Bramadero lon (from NicaAgua)
lat <- 13.3816217871 # El Bramadero lat (from NicaAgua)
lonLat <- data.frame(lon = lon, lat = lat)

# Set up precipitation data
location <- terra::vect(lonLat, crs = "+proj=longlat +datum=WGS84")
precip <- terra::extract(infile, location, method = "bilinear") %>%
  subset(select = -ID) %>%
  t()
precip[precip < 0] <- 0 # replace any negative (errant) values with zeroes
precip <- as.vector(precip)

## -----------------------------------------------------------------------------
# Set up dates (time) data for ONE year (365 days)
allTimes <- terra::time(infile) %>%
  as.Date() %>%
  data.frame()
timeFrame <- as.Date(c(allTimes[1, 1]:allTimes[365, 1])) %>%
  data.frame()
# Make a Dataframe for each year
chunkLength <- 365
divYears <- (split(precip, ceiling(seq_along(precip) / chunkLength)))
divYearsFrame <- data.frame(divYears)

averagePrecip <- c()
for (i in 1:nrow(divYearsFrame)) {
  daySum <- sum(divYearsFrame[i, ])
  averagePrecip <- rbind(averagePrecip, daySum)
}
nyears <- round(length(precip) / 365)
averagePrecip <- averagePrecip / nyears # (average over number of years)

## -----------------------------------------------------------------------------
# Assemble the Timeseries
timeseriesFrame <- cbind(timeFrame, averagePrecip)
colnames(timeseriesFrame) <- c("Date", "Precipitation")
x <- xts(timeseriesFrame$Precipitation, timeseriesFrame$Date) # This produces the "xts" screenshot

## -----------------------------------------------------------------------------
# Perform MSD calculations with the new xts
keyDatesTS <- msdrought::msdDates(time(x))
filterTS <- msdrought::msdFilter(x, window = 31, quantity = 2)
duration <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "duration")
intensity <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "intensity")
firstMaxValue <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "firstMaxValue")
secondMaxValue <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "secondMaxValue")
min <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "min")
allStats <- msdrought::msdMain(x)
graph1981 <- suppressWarnings(msdrought::msdGraph(x, 1981))
suppressWarnings(plot(graph1981))
# Note: suppressWarnings hides irrelevant messages that result from the msdGraph output

