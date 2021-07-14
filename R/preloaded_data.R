
#' mappings
#'
#' preloaded_data that allows Argus to load up data from a github or zenodo link.
#' Argus data is maintained on the https://github.com/JGCRI/argusdata repo
#' If new data is added to the repo users can directly change this file and
#' commit on github at : https://github.com/JGCRI/argus/blob/main/R/preloaded_data.R
#'
#' @keywords preloaded data
#' @return A dataframe with preloaded data
#' @export
#' @examples
#' library(rmap)
#' preloaded_data()

preloaded_data <- function(){

  preloaded_data <- tibble::tribble(
    ~"group", ~"name", ~"link",
    "examples", "exampleData", "link_to_rds",
    "GCAM", "GCAMv4.1", "link_to_rds")

invisible(preloaded_data)

}

