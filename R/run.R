#' run
#'
#' Function to launch Shiny Application.
#'
#' @export run
#'
#' @return Shiny application object
#'
#' @examples \dontrun{
#' library(argus)
#' argus::run()
#' }
#'
#' @import shiny

run <- function() {
  shiny::runApp(system.file('app', package='argus'))
}
