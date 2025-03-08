## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loaddata-3---------------------------------------------------------------
# This loads the data included in the package, but you would attach your own
data <- system.file("extdata", "prcp_cropped.tif", package = "msdrought") 
infile <- terra::rast(data)

## ----dates-3, warning=FALSE---------------------------------------------------
# Find the key dates related to the MSD
# msdDates = msdDates(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
formattedDates <- as.Date(terra::time(infile))
keyDatesTS <- msdrought::msdDates(formattedDates)

# Use the terra::app function to apply the Bartlett noise filter (msdFilter) to the raster
# msdFilter = msdFilter(x, window)
filtered <- terra::app(infile, msdrought::msdFilter, window = 31, quantity = 2)
terra::time(filtered) <- formattedDates
terra::plot(filtered[[1:2]])

## ----msd-3a, warning=FALSE----------------------------------------------------
intensity <- terra::app(filtered, fun=msdrought::msdStats, dates=keyDatesTS, fcn="intensity")
year1 <- as.numeric(format.Date(formattedDates[1], "%Y"))
year2 <- as.numeric(format.Date(formattedDates[length(formattedDates)], "%Y"))
terra::time(intensity, tstep = "years") <- year1:year2

## ----fig-3, fig.align="center", fig.width=5, fig.height=5, out.width="70%"----
terra::plot(intensity)

