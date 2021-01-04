#' saveDataFiles.R

#---------------------------
# Libraries Needed
#---------------------------
library(data.table)
library(usethis)

#---------------------------
# Example gcam data file from metis.readgcam()
#---------------------------
exampleData <- data.table::fread(paste(getwd(),"/inst/extdata/exampleData.csv",sep=""),skip=0,encoding="Latin-1")
usethis::use_data(exampleData, overwrite=T)

#-------------------
# XMl Query Files
#-------------------

xmlFilePath = paste(getwd(),"/inst/extdata/queries.xml",sep="")
xmlfile <- XML::xmlTreeParse(xmlFilePath)
xmltop <- XML::xmlRoot(xmlfile)
top <- XML::xmlNode(XML::xmlName(xmltop))
for(i in 1:length(xmltop)){
  top <- XML::addChildren(top, xmltop[[i]])
}
xmlQueries <- top
use_data(xmlQueries, overwrite=T)

#-------------------
# Capacity Factors
#-------------------

data_capfactors <- data.table::fread(file=paste(getwd(),"/inst/extdata/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=5,encoding="Latin-1")
data_cap_cost_tech <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L2233.GlobalTechCapital_elecPassthru.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_cool <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L2233.GlobalTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_tech <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L2233.GlobalIntTechCapital_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_cool <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L2233.GlobalIntTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_A23.globaltech_retirement <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/A23.globaltech_retirement.csv",sep=""), skip=1)
data_capac_fac <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L223.GlobalTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_capac_fac_int <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/L223.GlobalIntTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_tech_mapping <- data.table::fread(paste(getwd(),"/inst/extdata/investCalcs/agg_tech_mapping.csv", sep=""), skip=1)
use_data(data_capfactors, overwrite=T)
use_data(data_cap_cost_tech, overwrite=T)
use_data(data_cap_cost_cool, overwrite=T)
use_data(data_cap_cost_int_tech, overwrite=T)
use_data(data_cap_cost_int_cool, overwrite=T)
use_data(data_A23.globaltech_retirement, overwrite=T)
use_data(data_capac_fac, overwrite=T)
use_data(data_capac_fac_int, overwrite=T)
use_data(data_tech_mapping, overwrite=T)

