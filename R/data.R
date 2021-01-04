#' data

#' exampleData
#'
#' @source An example GCAM run
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::exampleData
#' }
"exampleData"

#-----------------
# Internal Data
#-----------------

#' xmlQueries xml file
#'
#' @source rdataviz
#' @format .xml
#' @examples
#' \dontrun{
#'  library(rdataviz); library(XML)
#'  rdataviz::xmlQueries
#'  # Can save xml
#'  XML::saveXML(rdataviz::xmlQueries, file=paste(getwd(), "/xmlQueries.xml", sep = ""))
#' }
"xmlQueries"

# readgcam internal files

#' data_capac_fac
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalTechCapFac_elec.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_capac_fac
#' }
"data_capac_fac"

# elecInvest internal files

#' data_tech_mapping
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/agg_tech_mapping.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::tech_mapping
#' }
"data_tech_mapping"

#' data_capac_fac_int
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L223.GlobalIntTechCapFac_elec.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_capac_fac_int
#' }
"data_capac_fac_int"


#' data_A23.globaltech_retirement
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/A23.globaltech_retirement.csv",sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_A23.globaltech_retirement
#' }
"data_A23.globaltech_retirement"

#' data_cap_cost_int_cool
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec_cool.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_cap_cost_int_cool
#' }
"data_cap_cost_int_cool"

#' data_cap_cost_int_tech
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalIntTechCapital_elec.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_cap_cost_int_tech
#' }
"data_cap_cost_int_tech"

#' data_cap_cost_cool
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elec_cool.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_cap_cost_cool
#' }
"data_cap_cost_cool"

#' data_capfactors
#'
#' @source paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_capfactors
#' }
"data_capfactors"

#' data_cap_cost_tech
#'
#' @source paste(getwd(),"/dataFiles/gcam/investCalcs/L2233.GlobalTechCapital_elecPassthru.csv", sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(rdataviz);
#'  rdataviz::data_cap_cost_tech
#' }
"data_cap_cost_tech"

