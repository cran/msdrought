## ----set knitr options, echo = FALSE------------------------------------------
# set some knitr options
knitr::opts_chunk$set(echo = FALSE
                      , message = FALSE
                      , warning = FALSE)

## ----setup--------------------------------------------------------------------
library(msdrought)

## -----------------------------------------------------------------------------
# Load the necessary packages and the included data, for the purpose of demonstration
library(terra)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(xts)
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") # This loads the data included in the package, but you would attach your own
infile <- terra::rast(data)

## -----------------------------------------------------------------------------
# Find the key dates related to the MSD
# msdDates = msdDates(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
allDates <- terra::time(infile)
formattedDates <- as.Date(allDates)
critDates <- msdrought::msdDates(formattedDates)

# Use the terra::app function to apply the bartlett noise filter (msdFilter) to the raster
# msdFilter = msdFilter(x, window)
filtered <- terra::app(infile, msdFilter, window = 31, quantity = 2)

# Use the terra::app function to apply the bartlett noise filter (msdFilter) to the raster
# msdStats = msdStats(x, dates, fcn)
intensityPlots <- suppressWarnings(terra::app(filtered, msdStats, critDates, fcn = "intensity"))
durationPlots <- suppressWarnings(terra::app(filtered, msdStats, critDates, fcn = "duration"))
# Note: suppressWarnings hides irrelevant messages that result from the msdFilter output

## -----------------------------------------------------------------------------
# Set up the desired location's longitude and latitude values
lon <- -86.2621555581 # Longitude of the spatial point we're interested in analyzing
lat <- 13.3816217871 # Lattitude of the spatial point we're interested in analyzing
lonLat <- data.frame(lon = lon, lat = lat)

# Set up precipitation data by extracting the data located at our longitude and lattitude
location <- vect(lonLat, crs = "+proj=longlat +datum=WGS84")
locationIntensity <- terra::extract(intensityPlots, location, method = "bilinear") %>%
  subset(select = -ID) %>%
  t()

locationDuration <- terra::extract(durationPlots, location, method = "bilinear") %>%
  subset(select = -ID) %>%
  t()

## -----------------------------------------------------------------------------
# Pull the years from the data, for the purpose of organizing the data
allYears <- terra::time(infile)
firstYear <- lubridate::year(allYears[1])
lastYear <- lubridate::year(allYears[length(allYears)])
yearsSeq <- firstYear:lastYear

# Combine the years, intensity and duration objects, for ease of comparison
combined <- cbind(yearsSeq, locationDuration, locationIntensity) %>%
  as.data.frame()
colnames(combined) <- c("years", "durationValue", "intensityValue")

