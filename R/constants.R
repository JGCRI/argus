#' constants
#'
#' This function holds the various constants used in the argus package
#'
#' List of Constants
#' \itemize{
#' \item "chosenMix"}
#' @param name Default=NULL. Name of constant to call.
#' @keywords constants
#' @return A list of constants
#' @export
#' @examples \dontrun{
#' library(argus)
#' constants()$chosenMix
#' }

constants <- function(name=NULL) {

  #------------------
  # Conversions
  #------------------
  GCAMbaseYear <- 2010
  convEJ2MTOE<-23.8845897  #https://www.iea.org/statistics/resources/unitconverter/
  convEJ2TWh<-277.77777777778
  convEJ2GW<-convEJ2TWh*1000/8760
  convEJ2GWh <- 277777.778
  convGW_kW <- 1e6
  conv1975USDperGJ22017USDperMWh<- (103.015/28.485)/0.2777778 # (13.02) Deflators 1975 (28.485) to 2017 (103.015) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  conv1975USDperGJ22017USDperMBTU<- (103.015/28.485)/0.947 # (3.82) Deflators 1975 (28.485) to 2017 (103.015) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  convUSD_1975_2010	<- 91.718/28.485 # (3.22) Deflators 1975 (28.485) to 2010 (91.718) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  convUSD_1975_2015	<- 100/28.485 # (3.51) Deflators 1975 (28.485) to 2015 (100) from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
  conv_C_CO2 <- 44/12
  conv_MT_GT <- 1e-3

  #--------------------------------------------------------------------------------------------------
  # Emissions Conversion to CO2eq
  # GWP conversions - uses 100-yr GWPs from IPPC AR4 and AR5
  # https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
  # Does not include all covnersions. Add them if they are tracked in GCAM
  # Mean HFC numbers GWP %>% dplyr::filter(grepl("HFC",ghg)) %>% summarize_at(vars(names(GWP)[!grepl("ghg",names(GWP))]),funs(mean))
  # GTP AR5 Box 3.2 Table 1 https://www.ipcc.ch/site/assets/uploads/2018/02/SYR_AR5_FINAL_full.pdf
  #--------------------------------------------------------------------------------------------------

  GWP<- tibble::tribble(
    ~ghg, ~GWPSAR, ~GWPAR4,~GWPAR5,~GTPAR5,
    "CO2",44/12,44/12,44/12,44/12,
    "CH4",21,25,28,4,
    "CH4_AGR", 21,	25, 28, 28,
    "CH4_AWB", 21,	25, 28, 28,
    "N2O",310,298,265,234,
    "N2O_AGR",310,298,265,234,
    "N2O_AWB",310,298,265,234,
    "C2F6",9200,12200,11100,NA,
    "CF4",6500,7390,6630,8040,
    "HFC125",2800,3500,3170,NA,
    "HFC134a",1300,1430,1300,NA,
    "HFC245fa",1030,1030,858,NA,
    "HFC143a",3800,4470,4800,NA,
    "HFC152a",140,124,138,19,
    "HFC227ea",2900,3220,3350,NA,
    "HFC23",11700,14800,12400,NA,
    "HFC236fa",6300,9810,8060,NA,
    "HFC32",650,675,677,NA,
    "HFC365mfc",794,794,804,NA,
    "HFCs", 3141, 3985, 3555,NA,
    "SF6",23900,22800,23500,NA
  )%>%as.data.frame;

  # https://nepis.epa.gov/Exe/ZyNET.exe/P1001YTS.txt?ZyActionD=ZyDocument&Client=EPA&Index=2000%20Thru%202005&Docs=&Query=&Time=&EndTime=&SearchMethod=1&TocRestrict=n&Toc=&TocEntry=&QField=&QFieldYear=&QFieldMonth=&QFieldDay=&UseQField=&IntQFieldOp=0&ExtQFieldOp=0&XmlQuery=&File=D%3A%5CZYFILES%5CINDEX%20DATA%5C00THRU05%5CTXT%5C00000017%5CP1001YTS.txt&User=ANONYMOUS&Password=anonymous&SortMethod=h%7C-&MaximumDocuments=1&FuzzyDegree=0&ImageQuality=r75g8/r75g8/x150y150g16/i425&Display=hpfr&DefSeekPage=x&SearchBack=ZyActionL&Back=ZyActionS&BackDesc=Results%20page&MaximumPages=1&ZyEntry=3
  # https://www.firescience.gov/projects/09-2-01-9/supdocs/09-2-01-9_Appendix_C_-_Unit_Conversion_and_Other_Tables.pdf
  # MTC is megatonnes (10^6 tonnes) of Carbon.
  # Tg one terragram = 1 Megatonne = 10^6 tonnes
  # Convert to Mega tonnes of CO2 eq.

  convertGgTgMTC<- tibble::tribble(
    ~Units,~Convert,
    "Gg",0.001*1,
    "Tg",1,
    "MTC",1,
    "MtC/yr",1
  )%>%as.data.frame;


  #--------------------------------------------------------------------------------------------------
  # Data source for capacity factor:
  # https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/52-capacity-factors-hydropower
  # Data source for hydropower capital cost
  # https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/51-hydropower-capital-costs
  #--------------------------------------------------------------------------------------------------

  hydro_cap_fact <- 0.38
  hydro_cost_GW <- 1.5

  #--------------------------------------------------------------------------------------------------
  # GCAM USA Regions
  #--------------------------------------------------------------------------------------------------
  US52 <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
            "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
            "MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM",
            "NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN",
            "TX","UT","VA","VT","WA","WI","WV","WY")

  # GCAM USA 49. Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
  US49 <- c("AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
            "IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
            "MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM",
            "NV","NY","OH","OK","OR","PA","RI","SC","SD","TN",
            "TX","UT","VA","VT","WA","WI","WV","WY")

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

  #--------------------------------------------------------------------------------------------------
  # Return Data
  #--------------------------------------------------------------------------------------------------

  listx <- list(
    GCAMbaseYear=GCAMbaseYear,
    convEJ2MTOE=convEJ2MTOE,
    convEJ2TWh=convEJ2TWh,
    convEJ2GW=convEJ2GW,
    convEJ2GWh=convEJ2GWh,
    convGW_kW=convGW_kW,
    conv_C_CO2=conv_C_CO2,
    conv_MT_GT=conv_MT_GT,
    hydro_cap_fact=hydro_cap_fact,
    hydro_cost_GW=hydro_cost_GW,
    convUSD_1975_2010=convUSD_1975_2010,
    convUSD_1975_2015=convUSD_1975_2015,
    conv1975USDperGJ22017USDperMWh=conv1975USDperGJ22017USDperMWh,
    conv1975USDperGJ22017USDperMBTU=conv1975USDperGJ22017USDperMBTU,
    convertGgTgMTC=convertGgTgMTC,
    GWP=GWP,
    US52=US52,
    US49=US49,
    chosenMix=chosenMix)

  if(!is.null(name)){returnx <- listx[[name]]} else {returnx <- listx }


  return(returnx)

}
