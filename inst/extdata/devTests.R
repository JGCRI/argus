

dataDefault <- rdataviz::exampleData

  # Aggregate across classes
  tblAggsums <- dataDefault %>%
    dplyr::mutate(scenario = as.character(scenario)) %>%
    dplyr::filter(aggregate == "sum") %>%
    dplyr::select(scenario, param, subRegion, x, value) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarize_at(c("value"), list( ~ sum(.)))
  tblAggmeans <- dataDefault %>%
    dplyr::select(-class) %>%
    dplyr::mutate(scenario = as.character(scenario)) %>%
    dplyr::filter(aggregate == "mean") %>%
    dplyr::select(scenario, param, subRegion, x, value) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarize_at(c("value"), list( ~ mean(.)))

  dataMap <- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup() %>%
    filter(x=="2010");  dataMap


# Filter Data after Reactive Choices -------------------
dataMapxi <- dataMap %>% filter(param==(dataMap$param%>%unique())[1])%>%
    unique();
dataMapxi

mapx <- (rmap::mapFind(dataMapxi))$subRegShapeFound;
mapx@data <- mapx@data %>%
  dplyr::left_join(dataMapxi)%>%
  dplyr::select("subRegion","value")%>%
  unique(); mapx@data
mapx_1 <- tmap::tm_shape(mapx) +
  tm_polygons(col = "value") +
  tm_layout(legend.outside = T,
            legend.show = F)

m1<-tmap_leaflet(mapx_1)
m2<-tmap_leaflet(mapx_1) %>% clearControls()
sync(m1,m2,ncol=2)

for(x_i in 1:length(unique(dataMap$subRegion))){
  for(param_i in 1:length(unique(dataMap$subRegion))){
    for(subRegion_i in 1:length(unique(dataMap$subRegion))){
      for(year_i in 1:length(unique(dataMap$subRegion))){
        dfx <- data.frame()
      }
    }
  }
}

# All regions
dataMap %>% tidyr::complete(scenario,param,subRegion,x) %>%
  dplyr::mutate(value=case_when(is.na(value)~0,
                                TRUE~value))-> dataMap1; dataMap1

# Add regions for sumReg compare
dataMap1 %>% filter(subRegion != "Southeast Asia",
                   param == "GDP (Billion 1990 USD)")

#......................
# Readgcam
#......................

#----------------------------
# Load Libraries
#---------------------------
#install.packages("devtools")
library(devtools)
#devtools::install_github("JGCRI/rgcam")
#devtools::install_github("JGCRI/metis")
library(rgcam)
library(metis)
library(dplyr)

#----------------------------
# Choose GCAM data
#---------------------------

dirOutputs_i = "C:/Z/projects/current/00_SMART/modeling"

dbPaths <- c(
  "C:/Z/models/GCAMVersions/gcam-core_v5p3_sha_feature_southeast-asia/output/database_basexdb",
  "C:/Z/models/GCAMVersions/gcam-core_v5p3_sha_feature_southeast-asia-floorspace/output/database_basexdb_floorspace"
)

projFiles <- c("C:/Z/projects/current/00_SMART/modeling/projFile_orginal.proj",
               "C:/Z/projects/current/00_SMART/modeling/projFile_floorspace.proj")

# Scenario names
#scenOrigNames_i = c("Original","Floorspace") # make sure these exist (See outputs of the rgcam::localDBConn)
#scenNewNames_i = c("Original","Floorspace")  # Names to replace the original names for final figures.

#----------------------------
# Read GCAM Data (metis.readgcam.R)
#---------------------------

#...............
# Get names of dir and database
gcamdatabasePath <- "C:/Z/models/GCAMVersions/gcam-core_v5p3_sha_feature_southeast-asia/output/database_basexdb"
gcamdatabasePath_dir <- gsub("/$","",gsub("[^/]+$","",gcamdatabasePath)); gcamdatabasePath_dir
gcamdatabasePath_file <- gsub('.*/ ?(\\w+)', '\\1', gcamdatabasePath); gcamdatabasePath_file

#...............
# Get names of scenarios in database
# Save Message from rgcam::localDBConn to a text file and then extract names
zz <- file(paste(getwd(),"/test.txt",sep=""), open = "wt")
sink(zz,type="message")
rgcam::localDBConn(gcamdatabasePath_dir,gcamdatabasePath_file)
sink()
closeAllConnections()
# Read temp file
con <- file(paste(getwd(),"/test.txt",sep=""),open = "r")
first_line <- readLines(con,n=1); first_line
closeAllConnections()
if(grepl("error",first_line,ignore.case = T)){stop(paste(first_line))}
print(first_line)
if(file.exists(paste(getwd(),"/test.txt",sep=""))){unlink(paste(getwd(),"/test.txt",sep=""))}
# Extract scenario names from saved line
s1 <- gsub(".*:","",first_line);s1
s2 <- gsub(" ","",s1);s2
scenarios <- as.vector(unlist(strsplit(s2,",")))
print(paste("All scenarios in data available: ", paste(scenarios,collapse=", "), sep=""))


#...................................
# Create data table from database
dir.create(paste(getwd(),"/tempdir",sep=""))
tempdir <- paste(getwd(),"/tempdir",sep="")
gcamdatabasePath
scenOrigNames_i <- "Original"
scenNewNames_i <- "Original New"
regionsSelect_i <- "Southeast Asia"
paramsSelect_i <- c("gdp","pop","agProdByCrop")

dataGCAM_x <- metis::metis.readgcam(reReadData = T,
                             dirOutputs = tempdir,
                             gcamdatabase = gcamdatabasePath,
                             scenOrigNames = scenOrigNames_i,
                             scenNewNames = scenNewNames_i,
                             dataProj = "projFile",
                             #dataProjPath = dataProjPath_i,
                             regionsSelect = regionsSelect_i,
                             paramsSelect= paramsSelect_i)

unlink(tempdir, recursive = T)

dataGCAM_x$data %>% as_tibble() %>%
  dplyr::select(scenario, region, subRegion, param,
                class1, class2, x, vintage, aggregate, units,
                value) -> dataGCAM

dataGCAM
unique(dataGCAM$region)
unique(dataGCAM$subRegion)
