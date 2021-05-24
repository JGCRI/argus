#' data

#' exampleData
#'
#' @source Example run from GCAM v5.3
#' @format Tibble
#' @examples
#' \dontrun{
#'  library(argus);
#'  argus::example_GCAMv5p3_SSP235
#' }
"example_GCAMv5p3_SSP235"

#' exampleData
#'
#' @source An example GCAM run
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(argus);
#'  argus::exampleData
#' }
"exampleData"

# Maps prepared for ggplot dataframes

#-----------------
# SubRegions list in pre-loaded maps
#-----------------

#' List of subRegions in Pre-loaded maps
#'
#' @source Compiled from each map
#' @format List
#' @examples
#' \dontrun{
#'  library(argus);
#'  names(mapsSubRegions)
#' }
"mapsSubRegions"

#-----------------
# World Maps (Countries, States)
#-----------------

#' World Map of Countries
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapCountriesdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapCountriesdf)
#' }
"mapCountriesdf"

#' World Map of States
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapStatesdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapStatesdf)
#' }
"mapStatesdf"

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

#' GCAM 32 Regions
#'
#' @source From JGCRI confluence page. \url{https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMReg32df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMReg32df)
#' }
"mapGCAMReg32df"


#' GCAM Basins
#'
#' @source From JGCRI confluence page. \url{https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsdf)
#' }
"mapGCAMBasinsdf"

#' GCAM Land
#'
#' @source From the moirai project.
#' Shapefile received directly from Alan V. Di Vittorio.
#' Component files available at:
#' \url{https://github.com/JGCRI/moirai/blob/master/ancillary/moirai_valid_boundaries.zip}
#' \url{https://github.com/JGCRI/moirai/blob/build-r-package/tests/testthat/test_compare_raster_w_outputs.R}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMLanddf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMLanddf)
#' }
"mapGCAMLanddf"

#-----------------
# Hydrology Maps (HydroShed, HUC)
#-----------------

# Hydro sheds
# https://www.hydrosheds.org/page/hydrobasins
# Lehner, B., Grill G. (2013): Global river hydrography and network routing:
# baseline data and new approaches to study the world’s large river systems.
# Hydrological Processes, 27(15): 2171–2186. Data is available at www.hydrosheds.org

#' HydroSHEDS level 1
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed1df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed1df)
#' }
"mapHydroShed1df"

#' HydroSHEDS level 2
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed2df)
#' }
"mapHydroShed2df"

#' HydroSHEDS level 3
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed3df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed3df)
#' }
"mapHydroShed3df"


# USGS HUC 2 (52 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52HUC2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52HUC2df)
#' }
"mapUS52HUC2df"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49HUC2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49HUC2df)
#' }
"mapUS49HUC2df"

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52HUC4df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52HUC4df)
#' }
"mapUS52HUC4df"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49HUC4df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49HUC4df)
#' }
"mapUS49HUC4df"


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

#' US 52 States
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52df)
#' }
"mapUS52df"

#' US 52 States Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Re-positioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52Compactdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52Compactdf)
#' }
"mapUS52Compactdf"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49df)
#' }
"mapUS49df"

#' US 52 Counties
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52Countydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52Countydf)
#' }
"mapUS52Countydf"

#' US 52 Counties Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Repositioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52CountyCompactdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52CountyCompactdf)
#' }
"mapUS52CountyCompactdf"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49Countydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49Countydf)
#' }
"mapUS49Countydf"


#--------------------
# Cropped
#--------------------

#' Cropped of GCAM Basins and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsUS52df)
#' }
"mapGCAMBasinsUS52df"

#' Cropped of GCAM Basins and US49
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsUS49df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsUS49df)
#' }
"mapGCAMBasinsUS49df"

#' GCAM Land cropped to US52
#'
#' @source Created from argus maps
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMLandUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMLandUS52df)
#' }
"mapGCAMLandUS52df"

#' GCAM Land cropped to US49
#'
#' @source Created from argus maps.
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMLandUS49df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMLandUS49df)
#' }
"mapGCAMLandUS49df"


#--------------------
# Mergers
#--------------------

#' Merge of GCAM 32 and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMReg32US52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMReg32US52df)
#' }
"mapGCAMReg32US52df"


#' Merge of Countries and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(argus); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapCountriesUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapCountriesUS52df)
#' }
"mapCountriesUS52df"

#--------------------
# Intersections
#--------------------

#' #' Intersection of GCAM Basins and countries.
#' #' @source JGCRI
#' #' @format R tibble
#' #' @examples
#' #' \dontrun{
#' #'  library(sp); library(argus); library(ggplot2)
#' #'  ggplot() +
#' #'  geom_polygon(data = mapIntersectGCAMBasinCountrydf, aes(x = long, y = lat, group = group),
#' #'              colour = "black", fill = NA)
#' #'  head(mapIntersectGCAMBasinCountrydf)
#' #' }
#' "mapIntersectGCAMBasinCountrydf"

#' #' Intersection of GCAM Basins and GCAM 32 Regions.
#' #' @source JGCRI
#' #' @format R tibble
#' #' @examples
#' #' \dontrun{
#' #'  library(sp); library(argus); library(ggplot2)
#' #'  ggplot() +
#' #'  geom_polygon(data = mapIntersectGCAMBasin32Regdf, aes(x = long, y = lat, group = group),
#' #'              colour = "black", fill = NA)
#' #'  head(mapIntersectGCAMBasin32Regdf)
#' #' }
#' "mapIntersectGCAMBasin32Regdf"

#' #' Intersection of GCAM Basins and US 52 Regions.
#' #' @source JGCRI
#' #' @format R tibble
#' #' @examples
#' #' \dontrun{
#' #'  library(sp); library(argus); library(ggplot2)
#' #'  ggplot() +
#' #'  geom_polygon(data = mapIntersectGCAMBasinUS52df, aes(x = long, y = lat, group = group),
#' #'              colour = "black", fill = NA)
#' #'  head(mapIntersectGCAMBasinUS52df)
#' #' }
#' "mapIntersectGCAMBasinUS52df"
#'
#'
#' #' Intersection of GCAM Basins and US 52 County Regions.
#' #' @source JGCRI
#' #' @format R tibble
#' #' @examples
#' #' \dontrun{
#' #'  library(sp); library(argus); library(ggplot2)
#' #'  ggplot() +
#' #'  geom_polygon(data = mapIntersectGCAMBasinUS52Countydf, aes(x = long, y = lat, group = group),
#' #'              colour = "black", fill = NA)
#' #'  head(mapIntersectGCAMBasinUS52Countydf)
#' #' }
#' "mapIntersectGCAMBasinUS52Countydf"

