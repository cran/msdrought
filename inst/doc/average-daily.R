## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-2b-----------------------------------------------------------------
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") # This loads the data included in the package, but you would attach your own
infile <- terra::rast(data)

# Make a SpatRast for one point
lon <- -86.2621555581 # El Bramadero lon (from NicaAgua)
lat <- 13.3816217871 # El Bramadero lat (from NicaAgua)
lonLat <- data.frame(lon = lon, lat = lat)

# Set up precipitation data
location <- terra::vect(lonLat, crs = "+proj=longlat +datum=WGS84")
precip <- terra::extract(infile, location, method = "bilinear") |>
  subset(select = -ID) |>
  t()
precip[precip < 0] <- 0 # replace any negative (errant) values with zeroes
precip <- as.vector(precip)

## ----loaddata-2---------------------------------------------------------------
# Find the average for each day of year
day_of_year <- lubridate::yday(terra::time(infile))
df <- data.frame(doy = day_of_year, precip = precip) |>
  dplyr::group_by(doy) |>
  dplyr::summarize(averagePrecip = mean(precip))

## ----ts-2---------------------------------------------------------------------
# Assemble the Timeseries, creating a dummy date column for the averaged year
ddates <- as.Date("1980-12-31") + unique(day_of_year)
x <- xts::xts(df$averagePrecip, order.by = ddates) 

## ----msd-2a-------------------------------------------------------------------
# Perform MSD calculations with the new xts
keyDatesTS <- msdrought::msdDates(time(x))
filterTS <- msdrought::msdFilter(x, window = 31, quantity = 2)

## ----msd-2b-------------------------------------------------------------------
duration <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "duration")

## ----msd-2c-------------------------------------------------------------------
intensity <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "intensity")
firstMaxValue <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "firstMaxValue")
secondMaxValue <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "secondMaxValue")
min <- msdrought::msdStats(filterTS, keyDatesTS, fcn = "min")
allStats <- msdrought::msdMain(x)

## ----fig-2, fig.align="center", fig.width=5, fig.height=5, out.width="70%"----
suppressWarnings(msdrought::msdGraph(x, 1981))

