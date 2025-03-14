## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)



## ----setup--------------------------------------------------------------------
library(msdrought)

## ----setup-1a, message=FALSE, warning=FALSE-----------------------------------
library(terra)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(xts)

## ----setup-1b-----------------------------------------------------------------
# This loads the data included in the package, but you would attach your own
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") 

# Extract 1 spatial point from the raster data
infile <- terra::rast(data)
lon <- -86.2621555581 # Longitude of the point we're interested in analyzing
lat <- 13.3816217871  # Latitude of the point we're interested in analyzing
lonLat <- data.frame(lon = lon, lat = lat)

# Set up precipitation data by extracting the data located at our longitude and lattitude
location <- terra::vect(lonLat, crs = "+proj=longlat +datum=WGS84")
precip <- terra::extract(infile, location, method = "bilinear") %>%
  subset(select = -ID) %>%
  t()
precip[precip < 0] <- 0 # replace any negative (errant) values with zeroes
precipFrame <- data.frame(precip)

# Set up dates (time) data which will be used in creating our time series
timeFrame <- as.Date(terra::time(infile)) |> data.frame()
startDate <- timeFrame[1, 1]
endDate <- timeFrame[nrow(timeFrame), 1]
datesSequence <- seq(from = startDate, to = endDate, by = 1)
timeseriesFrame <- cbind(timeFrame, precipFrame)
colnames(timeseriesFrame) <- c("Date", "Precipitation")

# Make the data into an xtimeseries that the package recognizes
x <- xts::xts(timeseriesFrame$Precipitation, timeseriesFrame$Date)

## ----load-1-------------------------------------------------------------------
data("timeseries") # This loads the data included in the package, but you would attach your own
x <- timeseries

## ----filter-1-----------------------------------------------------------------
# Use the msdDates function to extract the key dates related to the window of the MSD
# msdDates = msdDates(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
keyDatesTS <- msdrought::msdDates(time(x))
# Note: The windows (between the start and end dates) can be user-defined, but leaving the field blank uses the parameters determined by the founders of this package

# Filter the data using a bartlett noise filter by using the msdFilter function
# msdFilter = msdFilter(x, window)
filterTS <- msdrought::msdFilter(x, window = 31, quantity = 2)
# Note: The founders of this package recommend filtering the data twice, but this is left up to the user to decide

## ----data-1-------------------------------------------------------------------
# Use msdStats and r's built-in apply function to apply the intensity calculation across the filtered timeseries data
# msdStats = msdStats(x, dates, fcn)
durationValues <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "duration")
intensityValues <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "intensity")
firstMaxValues <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "firstMaxValue")
secondMaxValues <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "secondMaxValue")
minValues <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "min")

## ----stats-1------------------------------------------------------------------
# msdMain takes the input parameters from their basic form and calculates the relevant statistics
# msdMain = msdMain(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate, window, quantity)
allStats <- msdrought::msdMain(x)

## ----fig-1, fig.align="center", fig.width=5, fig.height=5, out.width="70%"----
# Use msdGraph to calculate the statistics and analyze them to determine if an MSD was present or not during the course of one year
# msdGraph = msdGraph(x, year, firstStartDate, firstEndDate, secondStartDate, secondEndDate, window, quantity)
#graph1982 <- msdrought::msdGraph(x, 1982)
#plot(graph1982)
msdrought::msdGraph(x, 1982)

