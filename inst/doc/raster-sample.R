## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(msdrought)

## ----warning=FALSE------------------------------------------------------------
library(terra)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(xts)
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") # This loads the data included in the package, but you would attach your own
infile <- terra::rast(data)

## ----warning=FALSE------------------------------------------------------------
# Find the key dates related to the MSD
# msdDates = msdDates(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
allDates <- terra::time(infile)
formattedDates <- as.Date(allDates)
dates <- msdrought::msdDates(formattedDates)

# Use the terra::app function to apply the bartlett noise filter (msdFilter) to the raster
# msdFilter = msdFilter(x, window)
filtered <- terra::app(infile, msdFilter, window = 31, quantity = 2)
terra::time(filtered) <- terra::time(infile)
plot(filtered)

## ----warning=FALSE------------------------------------------------------------
# Use the terra::app function to apply the bartlett noise filter (msdFilter) to the raster
# msdStats = msdStats(x, dates, fcn)
intensity <- terra::app(filtered, msdStats, dates, fcn = "intensity")
year1 <- as.numeric(format.Date(formattedDates[1], "%Y"))
year2 <- as.numeric(format.Date(formattedDates[length(formattedDates)], "%Y"))
terra::time(intensity, tstep = "years") <- year1:year2

## ----fig.height=5, fig.width=5------------------------------------------------
# Plot our results
terra::plot(intensity)

