#' Change behavior of which.max to work as if it had an na.rm=TRUE option
#'
#' Change behavior of which.max to work as if it had an na.rm=TRUE option using
#' https://gis.stackexchange.com/questions/392666/app-function-returns-a-wrong-output-format
#' answer recommended by R Hijmans.
#'
#' @param i A vector
#'
#' @examples
#' \dontrun{
#' f.which.max(1:5)
#' f.which.max(NA)
#' }
f.which.max <- function(i) c(which.max(i), NA)[1]
