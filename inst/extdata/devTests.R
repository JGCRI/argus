library(rmap)
library(broom)
library(ggplot2)
library(dplyr)
library(magrittr)
library(profvis)
library(tictoc)
library(lineprof)

# Raw Map Data
#dataMapx
#regionsx = argus::constants("US52")
#regionsx = unique(mapCountriesdf$subRegion)
regionsx = c("USA","China")
dataMap_raw <- tibble::tibble(
  subRegion=c(regionsx,regionsx),
  scenario=c(rep("scen1",length(regionsx)),(rep("scen2",length(regionsx)))),
  x=c(rep(2010,length(regionsx)),rep(2010,length(regionsx))),
  param=c(rep("ag",length(regionsx)),rep("ag",length(regionsx))),
  value=runif(length(regionsx)*2,0,1000)); dataMap_raw

diffText = "Abs Diff"
  scenRef_i = unique(dataMap_raw$scenario)[1]; scenRef_i

  # Calculate Diff Values
  tbl_pd <-dataMap_raw %>%
    dplyr::filter(scenario == scenRef_i)
  for (k in unique(dataMap_raw$scenario)[unique(dataMap_raw$scenario) !=
                                          scenRef_i]) {
    tbl_temp <- dataMap_raw %>%
      dplyr::filter(scenario %in% c(scenRef_i, k))
    # print("tbl_temp")
    # print(tbl_temp)
    # print("tbl_temp$value")
    # print(tbl_temp$value)
    tbl_temp <- tbl_temp %>%
      tidyr::spread(scenario, value)
    # print("tbl_temp post spread")
    # print(tbl_temp)

    tbl_temp[is.na(tbl_temp)] <- 0

    tbl_temp <- tbl_temp %>%
      dplyr::mutate(!!paste(k, diffText, sep = "") := get(k) - get(scenRef_i)) %>%
      dplyr::select(-dplyr::one_of(c(k, scenRef_i)))
    # print("tbl temp post mute")
    # print(tbl_temp)
    tbl_temp <- tbl_temp %>%
      tidyr::gather(key = scenario, value = value, -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k, diffText, sep = "")]))
    # print("tidyr")
    # print(tbl_temp)
    tbl_pd <- dplyr::bind_rows(tbl_pd, tbl_temp)
    # print("bind_rows")
    # print(tbl_pd)


  tbl_pd <- tbl_pd %>%
    dplyr::mutate(scenario = factor(scenario,
                                    levels = c(scenRef_i,
                                               unique(
                                                 tbl_pd$scenario
                                               )[unique(tbl_pd$scenario) != scenRef_i])))
  # print(tbl_pd)
  tbl_pd}

# shpdf <-mapdfFind(dataMap_raw)
# a <- shpdf %>%
#   dplyr::inner_join(dataMap_raw, by="subRegion") %>%
#   #dplyr::filter(subRegion!="South_Pacific_Islands")%>%
#   dplyr::group_by(subRegion) %>%
#   dplyr::mutate(minLong = min(long),
#                 negLongSum = sum(long[which(long<=0)], na.rm=T),
#                 maxLong = max(long),
#                 posLongSum = sum(long[which(long>=0)], na.rm=T),
#                 flip = case_when(minLong<-160 & maxLong>160 ~ 1,
#                                  TRUE~0),
#                 long = case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
#                                  (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
#                                  TRUE~long))%>%
#   dplyr::ungroup()
#
# if(all(unique(a$flipType) %in% c(1,-1))){print("yes!")}
#
# ggplot()+ geom_polygon(data = a,
#                        aes(x = long, y = lat, group = group, fill = value),
#                        colour = "gray10", lwd=0.5)


tm1 <- system.time(
  {
    for(i in 1:10){x <-rmap::mapFind(dataMap_raw)}
  })

tm2 <- system.time(
  {
    for(i in 1:10){x1 <-argus::mapdfFind(dataMap_raw)}
  })

tm1
tm2


dataMap_raw <- read.csv("C:/Z/models/argus/inst/extdata/exampleData.csv",header=T)%>%
  tibble::as_tibble(); dataMap_raw; dataMap_raw$param%>%unique()

tblAggsums <- dataMap_raw %>%
  dplyr::mutate(scenario = as.character(scenario)) %>%
  dplyr::filter(aggregate == "sum") %>%
  dplyr::select(scenario, param, subRegion, x, value) %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list( ~ sum(.)))
tblAggmeans <- dataMap_raw %>%
  dplyr::select(-class) %>%
  dplyr::mutate(scenario = as.character(scenario)) %>%
  dplyr::filter(aggregate == "mean") %>%
  dplyr::select(scenario, param, subRegion, x, value) %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list( ~ mean(.)))

dataMap_raw<- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup() %>%
  dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
  dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                    TRUE~subRegion)) %>%
  dplyr::select(-subRegionMap); dataMap_raw%>%head()

dataMap_raw$subRegion%>%unique()
dataMap_raw %>%
  dplyr::filter(subRegion %in% c("Bay_of_Bengal_North_East_Coast"))

#...................................
# For each Param create a plot for cowplot
#......................................

tm1 <- system.time(
  {
if(T){
# Set Breaks
US52Compact=F
naColor = "white"
breaks_n = 6
legendType = "kmean"
palAbsChosen <- "pal_hot"
yearsSelect <- 2010
paramsSelect <- unique(dataMap_raw$param)[1:3]

plist <- list()
for(i in paramsSelect[!is.na(paramsSelect)]){

  dataMap_raw_param <- dataMap_raw %>%
    dplyr::filter(x==yearsSelect,
                  param == i); dataMap_raw_param

# Set Breaks
breaks_pretty <- scales::pretty_breaks(n=breaks_n)(dataMap_raw_param$value); breaks_pretty
breaks_kmean <- sort(as.vector((stats::kmeans(dataMap_raw_param$value,
                                              centers=max(1,
                                                          min(length(unique(dataMap_raw_param$value))-1,
                                                              (breaks_n-1)))))$centers[,1]));breaks_kmean
if((max(range(dataMap_raw_param$value))-min(range(dataMap_raw_param$value)))<1E-10 &
   (max(range(dataMap_raw_param$value))-min(range(dataMap_raw_param$value)))>-1E-10){valueRange=floor(min(dataMap_raw_param$value))}else{
     valueRange=range(dataMap_raw_param$value)
   }
breaks_kmean

if(abs(min(valueRange,na.rm = T))==abs(max(valueRange,na.rm = T))){valueRange=abs(min(valueRange,na.rm = T))}
if(mean(valueRange,na.rm = T)<0.01 & mean(valueRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
  if(mean(valueRange,na.rm = T)<0.1 & mean(valueRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
    if(mean(valueRange,na.rm = T)<1 & mean(valueRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
      if(mean(valueRange,na.rm = T)<10 & mean(valueRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-2}}}}
animLegendDigits
breaks_kmean <- signif(breaks_kmean,animLegendDigits); breaks_kmean

if(!min(dataMap_raw_param$value) %in% breaks_kmean){
  breaks_kmean[breaks_kmean==min(breaks_kmean,na.rm=T)] <- signif(floor(min(dataMap_raw_param$value)),animLegendDigits)};breaks_kmean
if(!max(dataMap_raw_param$value) %in% breaks_kmean){
  breaks_kmean[breaks_kmean==max(breaks_kmean,na.rm=T)] <- signif(ceiling(max(dataMap_raw_param$value)),animLegendDigits)};breaks_kmean


if(legendType=="kmean"){breaks_map = breaks_kmean}else if(
  legendType=="pretty"){breaks_map = breaks_pretty}

# breaks_map <- breaks_map %>%
#   format(big.mark=",", scientific=F);
breaks_map <- breaks_map%>%unique()
if(length(breaks_map)==1){
  data_map <- dataMap_raw_param %>%
    dplyr::mutate(brks = format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ","))

  paletteAbs = "red"

  } else {
    breaks_map_levels <- gsub(","," to ",
                              gsub("\\(|\\]","",
                                   levels(cut(breaks_map,breaks=breaks_map)))); breaks_map_levels

    data_map <- dataMap_raw_param %>%
      dplyr::mutate(brks = cut(value,breaks=breaks_map),
                    brks = gsub("\\(|\\]","",brks),
                    brks = gsub(","," to ",brks),
                    brks = factor(brks,levels=breaks_map_levels))

    # Select Palettes
    paletteAbs <- grDevices::colorRampPalette(rmap::colors()[[palAbsChosen]])(length(breaks_map_levels)); paletteAbs
    paletteDiff <- "BrBG"
  }

data_map%>%head()


# Create ggplot path from shapefile

# Choose relevant shapefile and subset gridfile
mapFindx <- rmap::mapFind(data_map)
shp <- mapFindx$subRegShapeFound
subRegionCol <- mapFindx$subRegShapeTypeFound

if(subRegionCol=="US52" & US52Compact==T){
  shp <- rmap::mapUS52Compact
  subRegionCol <- "US52Compact"
}

# https://rpubs.com/huanfaChen/ggplotShapefile
shp_df <- broom::tidy(shp,region="subRegion") %>%
  dplyr::rename(subRegion=id);shp_df %>% head()

dataMapPlot <- shp_df %>%
  dplyr::inner_join(data_map, by="subRegion") %>%
  dplyr::filter(subRegion!="South_Pacific_Islands")%>%
  dplyr::group_by(subRegion) %>%
  dplyr::mutate(minLong = min(long),
                negLongSum = sum(long[which(long<=0)], na.rm=T),
                maxLong = max(long),
                posLongSum = sum(long[which(long>=0)], na.rm=T),
                flip = case_when(minLong<-160 & maxLong>160 ~ 1,
                                 TRUE~0),
                long = case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
                                 (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
                                 TRUE~long))%>%
  dplyr::ungroup(); dataMapPlot %>% head()
(dataMapPlot%>%filter(flip==1))$subRegion%>%unique()
dataMapPlot$long%>%min();
dataMapPlot$long%>%max();

cnames <- aggregate(cbind(long, lat) ~ subRegion, data=dataMapPlot, FUN=mean)
ggplot() +
  geom_polygon(data = dataMapPlot, aes(x = long, y = lat, group = group),
               colour = "black", fill = NA) +
  geom_text(data = cnames, aes(x = long, y = lat, label = subRegion),color="gray20", size = 4) + theme_void()


prcntZoom <- 1
longLimMinbg <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMinbg
longLimMaxbg <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMaxbg
latLimMinbg <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMinbg
latLimMaxbg <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMaxbg

prcntZoom <- 0.1
longLimMin <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMin
longLimMax <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMax
latLimMin <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMin
latLimMax <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMax


shp_bg <- broom::tidy(rmap::mapCountries,region="subRegion")%>%
  # dplyr::mutate(minLong = min(long),
  #               negLongLen = sum(long<1),
  #               maxLong = max(long),
  #               posLongLen = sum(long>1),
  #               long = case_when((posLongLen > negLongLen) & (long < 0) ~ long+360,
  #                                (posLongLen < negLongLen) & (long > 0) ~ long-360,
  #                                TRUE~long))%>%
  dplyr::ungroup()%>%
  dplyr::filter(long > longLimMinbg,
                long < longLimMaxbg,
                lat > latLimMinbg,
                lat < latLimMaxbg);
shp_bg %>% head()
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_bg, FUN=mean)
ggplot() +
  geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),
               colour = "black", fill = NA) +
  geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray20", size = 4) + theme_void()

if(T){
map <- ggplot()
  if(!US52Compact){
  map <- map + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)}
  #map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray50", size = 1) +
  map <- map + geom_polygon(data = dataMapPlot,
                 aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                 colour = "gray10", lwd=0.5) +
    scale_fill_manual(values=paletteAbs, na.value  = naColor) + theme_bw() +
    coord_fixed(ratio = 1.0,ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )+
    scale_y_continuous(position = "right")+
    facet_grid(param~scenario, switch="y",
               labeller = labeller(param = label_wrap_gen(15))
    ) +
    theme(legend.position="right",
          legend.title = element_blank(),
          plot.margin=margin(20,20,20,20,"pt"),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
  if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
}; map

plist[[i]] <- map
}

cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
}
  }
)

tm2 <- system.time(
  {
    if(T){
      # Set Breaks
      US52Compact=F
      naColor = "white"
      breaks_n = 6
      legendType = "pretty"
      palAbsChosen <- "pal_hot"
      yearsSelect <- 2010
      regionsSelect <- unique(dataMap_raw$subRegion)
      #regionsSelect <- c("Bay_of_Bengal_North_East_Coast","Southeast Asia")
      paramsSelect <- unique((dataMap_raw%>%
                                dplyr::filter(subRegion %in% regionsSelect))$param)[1:3]
      paramsSelect <- paramsSelect[!is.na(paramsSelect)]; paramsSelect


      plist <- list()
      for(i in paramsSelect){

        dataMap_raw_param <- dataMap_raw %>%
          dplyr::filter(x==yearsSelect,
                        param == i,
                        subRegion %in% regionsSelect); dataMap_raw_param

        # Set Breaks
        breaks_pretty <- scales::pretty_breaks(n=breaks_n)(dataMap_raw_param$value); breaks_pretty
        breaks_kmean <- sort(as.vector((stats::kmeans(dataMap_raw_param$value,
                                                      centers=max(1,
                                                                  min(length(unique(dataMap_raw_param$value))-1,
                                                                      (breaks_n-1)))))$centers[,1]));breaks_kmean
        if((max(range(dataMap_raw_param$value))-min(range(dataMap_raw_param$value)))<1E-10 &
           (max(range(dataMap_raw_param$value))-min(range(dataMap_raw_param$value)))>-1E-10){valueRange=floor(min(dataMap_raw_param$value))}else{
             valueRange=range(dataMap_raw_param$value)
           }
        breaks_kmean

        if(abs(min(valueRange,na.rm = T))==abs(max(valueRange,na.rm = T))){valueRange=abs(min(valueRange,na.rm = T))}
        if(mean(valueRange,na.rm = T)<0.01 & mean(valueRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
          if(mean(valueRange,na.rm = T)<0.1 & mean(valueRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
            if(mean(valueRange,na.rm = T)<1 & mean(valueRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
              if(mean(valueRange,na.rm = T)<10 & mean(valueRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-2}}}}
        animLegendDigits
        breaks_kmean <- signif(breaks_kmean,animLegendDigits); breaks_kmean

        if(!min(dataMap_raw_param$value) %in% breaks_kmean){
          breaks_kmean[breaks_kmean==min(breaks_kmean,na.rm=T)] <- signif(floor(min(dataMap_raw_param$value)),animLegendDigits)};breaks_kmean
        if(!max(dataMap_raw_param$value) %in% breaks_kmean){
          breaks_kmean[breaks_kmean==max(breaks_kmean,na.rm=T)] <- signif(ceiling(max(dataMap_raw_param$value)),animLegendDigits)};breaks_kmean


        if(legendType=="kmean"){breaks_map = breaks_kmean}else if(
          legendType=="pretty"){breaks_map = breaks_pretty}

        # breaks_map <- breaks_map %>%
        #   format(big.mark=",", scientific=F);
        breaks_map <- breaks_map%>%unique(); breaks_map
        if(length(breaks_map)==1){
          data_map <- dataMap_raw_param %>%
            dplyr::mutate(brks = format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ","))
          paletteAbs = "red"
          if(length(unique(format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ",")))!=1){
            breaks_map = format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ",")
            paletteAbs <- grDevices::colorRampPalette(rmap::colors()[[palAbsChosen]])(length(breaks_map)); paletteAbs
            data_map <- data_map %>%
              dplyr::mutate(brks = factor(brks,levels=breaks_map))

          }

        } else {
          breaks_map_levels <- gsub(","," to ",
                                    gsub("\\(|\\]","",
                                         levels(cut(breaks_map,breaks=breaks_map)))); breaks_map_levels

          data_map <- dataMap_raw_param %>%
            dplyr::mutate(brks = cut(value,breaks=breaks_map),
                          brks = gsub("\\(|\\]","",brks),
                          brks = gsub(","," to ",brks),
                          brks = factor(brks,levels=breaks_map_levels))

          # Select Palettes
          paletteAbs <- grDevices::colorRampPalette(rmap::colors()[[palAbsChosen]])(length(breaks_map_levels)); paletteAbs
          paletteDiff <- "BrBG"
        }

        data_map%>%head()
        breaks_map
        paletteAbs

        # Create ggplot path from shapefile

        # Choose relevant shapefile and subset gridfile
        shp <- argus::mapdfFind(data_map)
        subRegionCol <- unique(shp$subRegionType)

        if(subRegionCol=="US52" & US52Compact==T){
          shp <- argus::mapUS52Compactdf
          subRegionCol <- "US52Compact"
        }

        # https://rpubs.com/huanfaChen/ggplotShapefile
        shp_df <- shp

        dataMapPlot <- shp_df %>%
          dplyr::inner_join(data_map, by="subRegion") %>%
          #dplyr::filter(subRegion!="South_Pacific_Islands")%>%
          dplyr::group_by(subRegion) %>%
          dplyr::mutate(minLong = min(long),
                        negLongSum = sum(long[which(long<=0)], na.rm=T),
                        maxLong = max(long),
                        posLongSum = sum(long[which(long>=0)], na.rm=T),
                        flip = case_when(minLong<-160 & maxLong>160 ~ 1,
                                         TRUE~0),
                        long = case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
                                         (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
                                         TRUE~long))%>%
          dplyr::ungroup(); dataMapPlot %>% head()

        dataMapPlot%>%as.data.frame()%>%filter(long != lonOrig)%>%head()

        ggplot()+ geom_polygon(data = dataMapPlot,
                                    aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                    colour = "gray10", lwd=0.5)

        prcntZoom <- 1
        longLimMinbg <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMinbg
        longLimMaxbg <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMaxbg
        latLimMinbg <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMinbg
        latLimMaxbg <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMaxbg

        prcntZoom <- 0.1
        longLimMin <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMin
        longLimMax <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMax
        latLimMin <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMin
        latLimMax <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMax


        shp_bg <- argus::mapCountriesdf%>%
          dplyr::filter(long > longLimMinbg,
                        long < longLimMaxbg,
                        lat > latLimMinbg,
                        lat < latLimMaxbg);

        if(T){
          map <- ggplot()
          if(!US52Compact){
          map <- map + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)}
          #map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray50", size = 1) +
          map <- map + geom_polygon(data = dataMapPlot,
                                    aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                    colour = "gray10", lwd=0.5) +
            scale_fill_manual(values=paletteAbs, na.value  = naColor, drop=FALSE) + theme_bw() +
            coord_fixed(ratio = 1.0,ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()
            )+
            scale_y_continuous(position = "right")+
            facet_grid(param~scenario, switch="y",
                       labeller = labeller(param = label_wrap_gen(15))
            ) +
            theme(legend.position="right",
                  legend.title = element_blank(),
                  plot.margin=margin(20,20,20,20,"pt"),
                  axis.title=element_blank(),
                  axis.text=element_blank(),
                  axis.ticks=element_blank())
          if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
        }; map

        plist[[i]] <- map
      }

      cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
    }
  }
)

tm1
tm2


# Base Map Check

plist <- list()
pcount = 1
subRegTypelist <- c()
for(i in unique(dataMap_raw$param)[!is.na( unique(dataMap_raw$param))]){

dataMap_raw_regions <- dataMap_raw %>%
  dplyr::filter(subRegion!="South_Pacific_Islands")%>%
  dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
  dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                    TRUE~subRegion)) %>%
  dplyr::select(-subRegionMap) %>%
  dplyr::filter(param == i) %>%
  dplyr::select(subRegion) %>%
  unique(); dataMap_raw_regions

dataMapPlot <- argus::mapFind1(dataMap_raw_regions)%>%
  dplyr::filter(subRegion %in% dataMap_raw_regions$subRegion)%>%
  dplyr::group_by(subRegion) %>%
  dplyr::mutate(minLong = min(long),
                negLongSum = sum(long[which(long<=0)], na.rm=T),
                maxLong = max(long),
                posLongSum = sum(long[which(long>=0)], na.rm=T),
                flip = case_when(minLong<-160 & maxLong>160 ~ 1,
                                 TRUE~0),
                long = case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
                                 (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
                                 TRUE~long))%>%
  dplyr::ungroup()


if(!any(unique(dataMapPlot$subRegionType) %in% subRegTypelist)){

  subRegTypelist[pcount] <- unique(dataMapPlot$subRegionType)

prcntZoom <- 1
longLimMinbg <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMinbg
longLimMaxbg <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMaxbg
latLimMinbg <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMinbg
latLimMaxbg <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMaxbg

prcntZoom <- 0.1
longLimMin <- min(dataMapPlot$long)-abs(min(dataMapPlot$long))*prcntZoom;longLimMin
longLimMax <- max(dataMapPlot$long)+abs(max(dataMapPlot$long))*prcntZoom;longLimMax
latLimMin <- min(dataMapPlot$lat)-abs(min(dataMapPlot$lat))*prcntZoom;latLimMin
latLimMax <- max(dataMapPlot$lat)+abs(max(dataMapPlot$lat))*prcntZoom;latLimMax


shp_bg <- argus::mapCountriesdf%>%
  dplyr::filter(long > longLimMinbg,
                long < longLimMaxbg,
                lat > latLimMinbg,
                lat < latLimMaxbg);

cnames <- aggregate(cbind(long, lat) ~ subRegion, data=dataMapPlot, FUN=mean)

map <- ggplot() + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)
map <- map + geom_polygon(data = dataMapPlot,
                          aes(x = long, y = lat, group = group, fill=subRegion),
                          colour = "gray10", lwd=0.5, show.legend = F) +
  coord_fixed(ratio = 1.0,ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )+
  theme(plot.margin=margin(20,20,20,20,"pt"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = subRegion),color="black", size = 3)
map <- map + theme(panel.background = element_rect(fill="lightblue1")) + ggtitle(unique(dataMapPlot$subRegionType))
map

plist[[pcount]] <- map
pcount=pcount+1
}
}

cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")



# Check Compare Regions

dataChartPlot <-  read.csv("C:/Z/models/argus/inst/extdata/exampleDataEU.csv",header=T)%>%
  tibble::as_tibble(); dataChartPlot; dataChartPlot$param%>%unique()

# Aggregate across classes
tblAggsums <-dataChartPlot %>%
  #dplyr::filter(subRegion %in% regionsSelectedx()) %>%
  dplyr::mutate(scenario = as.character(scenario)) %>%
  dplyr::filter(aggregate == "sum") %>%
  dplyr::select(scenario, param, subRegion, x, value) %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list( ~ sum(.)))
tblAggmeans <- dataChartPlot %>%
  #dplyr::filter(subRegion %in% regionsSelectedx()) %>%
  dplyr::select(-class) %>%
  dplyr::mutate(scenario = as.character(scenario)) %>%
  dplyr::filter(aggregate == "mean") %>%
  dplyr::select(scenario, param, subRegion, x, value) %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list( ~ mean(.)))

dataMapx <- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup(); dataMapx%>%as.data.frame()

dataMapx <- dataMapx %>%
  tidyr::complete(scenario,param,subRegion,x) %>%
  dplyr::mutate(value=case_when(is.na(value)~0,
                                TRUE~value)); dataMapx

plist <- list()
for(i in 1:length(unique(dataChartPlot$param))){

  plist[[i]] <-  ggplot2::ggplot(dataChartPlot %>%
                                   filter(param==unique(dataChartPlot$param)[i]),
                                 aes(x=x,y=value,
                                     group=scenario,
                                     color=scenario)) +
    ggplottheme +
    ylab(NULL) + xlab(NULL) +
    geom_line() +
    scale_y_continuous(position = "right")+
    facet_grid(param~subRegion, scales="free",switch="y",
               labeller = labeller(param = label_wrap_gen(15))
    )+
    theme(legend.position="right",
          legend.text=element_text(size=titletext),
          legend.title = element_blank(),
          plot.margin=margin(20,20,20,20,"pt"))}
cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
}

