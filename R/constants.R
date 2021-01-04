#' constants
#'
#' This function holds the various constants used in the rdataviz package
#'
#' List of Constants
#' \itemize{
#' \item "chosenMix"}
#' @keywords constants
#' @return A list of constants
#' @export
#' @examples \dontrun{
#' library(rdataviz)
#' constants()$chosenMix
#' }

constants <- function() {
  #---------------------------
  # Params
  #---------------------------

  chosenMix <- c(
    "Population (Millions)",
    "GDP (Billion 1990 USD)",
    "Ag Production (Mt)",
    "Land Allocation (1000 km2)",
    "Water Withdrawal by Sector (km3)",
    "Electricity Generation by Fuel (TWh)",
    "Final Energy by Fuel (EJ)",
    "CO2 Emissions by Sector (MTCO2eq)"
  )

  constants <- list(chosenMix = chosenMix)
  return(constants)
}
