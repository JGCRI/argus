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
