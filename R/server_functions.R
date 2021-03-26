# Project helper functions for server ------------------------------------------------
# addMising()

#' Add missing data
#'
#' Used to add missing data to input files and customize format
#' @param data dataframe to test and convert
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export
addMissing<-function(data){
  NULL -> year -> aggregate -> scenario -> subRegion -> param -> x -> value

  if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
  if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
  if(!any(grepl("\\<subRegion\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegion="subRegion")}else{
    data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegion\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~subRegion))}
  if(!any(grepl("\\<subRegions\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegions\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~subRegion))}
  if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<value\\>",names(data),ignore.case = T))){stop("Data must have 'value' column.")}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<value\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!any(grepl("\\<values\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<values\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!any(grepl("\\<unit\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<unit\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
  if(!any(grepl("\\<units\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(units="units")}else{
    data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<units\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
  if(!"x"%in%names(data)){
    if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
  if(!any(grepl("\\<aggregate\\>",names(data),ignore.case = T))){
    if(is.null(aggregate)){data<-data%>%dplyr::mutate(aggregate="sum")}else{
      data<-data%>%dplyr::mutate(aggregate="sum")}
  }else{
    data <- data %>% dplyr::rename(!!"aggregate" := (names(data)[grepl("\\<aggregate\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(aggregate=dplyr::case_when(is.na(aggregate)~"sum",
                                                          TRUE~aggregate))}
  if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
      data<-data%>%dplyr::mutate(class="class")}else{data<-data%>%dplyr::mutate(class=class)}}else{
        data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("\\<class\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(class=dplyr::case_when(is.na(class)~"class",TRUE~class))}

  data <- data %>%
    dplyr::select(scenario,subRegion,param,class,x,aggregate,value)
    return(data)
}


#' parse_zip
#'
#' parse zip files
#' @param file file to unzip
#' @importFrom magrittr %>%
#' @export
parse_zip <- function(file){
  tmpdir <- tempdir()
  setwd(tmpdir)
  zip::unzip(
    file, exdir = tmpdir
  )
  for(i in dir()){
    if (endsWith(i, ".csv")){
      return(utils::read.csv(i) %>% as.data.frame())
    }
  }
}

#' parse_remote
#'
#' parse remote files
#' @param input reactive remote file link to parse
#' @importFrom magrittr %>%
#' @export
parse_remote <- function(input){
  if (tools::file_ext(input$urlfiledata) == ""){
    if (grepl("./$",input$urlfiledata)){
      #GCAM
      utils::read.csv(input$urlfiledata) %>%
        as.data.frame()
    } else if((!grepl("./$",input$urlfiledata, "/") || !grepl(".zip$",input$urlfiledata) || !grepl(".csv$",input$urlfiledata))){
      if (grepl(".zip", input$urlfiledata, fixed=TRUE)){
        temp <- tempfile()
        utils::download.file(input$urlfiledata, temp)
        return(parse_zip(temp))
      }else if (grepl(".csv", input$urlfiledata, fixed=TRUE)){
        return(utils::read.csv(input$urlfiledata) %>%
                 as.data.frame())
      }
    }
  }else if ((tools::file_ext(input$urlfiledata) == "zip")){
    temp <- tempfile()
    utils::download.file(input$urlfiledata, temp)
    return(parse_zip(temp))
  }else if (tools::file_ext(input$urlfiledata) == "csv"){
    temp <- tempfile()
    utils::download.file(input$urlfiledata, temp)
    return(utils::read.csv(input$urlfiledata) %>%
             as.data.frame())
  }else{
    return(NULL)
  }
}


#' parse_local
#'
#' parse local files
#' @param input reactive local file to parse
#' @importFrom magrittr %>%
#' @export
parse_local <- function(input){
  if (tools::file_ext(input$filedata$datapath) == ""){
    if (grepl("./$",input$urlfiledata$datapath)){
      utils::read.csv(input$filedata$datapath) %>%
        as.data.frame()
    }
  }else if (tools::file_ext(input$filedata$datapath) == "zip"){
    return(parse_zip(input$filedata$datapath))
  }else if (tools::file_ext(input$filedata$datapath) == "csv"){
    return(utils::read.csv(input$filedata$datapath) %>%
             as.data.frame())
  }else{
    return(NULL)
  }
}

#' exportHeight
#'
#' generate height for exported images
#' @param chartsperrow number of columns
#' @param max_height_in max height
#' @param numelement number of planned elements
#' @param lenperchart height per element
#' @importFrom magrittr %>%
#' @export
exportHeight<-function(chartsperrow,
                       max_height_in,
                       numelement,
                       lenperchart){
  if (numelement%%chartsperrow==0){
    return(min(max_height_in, ((numelement%/%chartsperrow))*lenperchart))
  }else{
    return(min(max_height_in, ((numelement%/%chartsperrow)+1)*lenperchart))
  }
}

#' exportWidth
#'
#' generate width for exported images
#' @param max_width_in max width
#' @param numelement number of planned elements
#' @param lenperchart height per element
#' @importFrom magrittr %>%
#' @export
exportWidth<-function(max_width_in, numelement, lenperchart){
  return(min(max_width_in, (numelement)*lenperchart))
}



#' summaryPlotReg
#'
#' generate summary plot
#' @param titletext plot title
#' @param dataMapx Input map dataframe
#' @param ggplottheme ggplot theme to use
#' @param subsetRegionsx Subset of regions
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

summaryPlotReg <- function(titletext,
                           dataMapx,
                           ggplottheme,
                           subsetRegionsx){

  # Initialize
 NULL-> param->scenario->subRegion->value->x

  dataChartPlot <- # All regions
    dataMapx %>% tidyr::complete(scenario,param,subRegion,x) %>%
    dplyr::mutate(value= dplyr::case_when(is.na(value)~0,
                                          TRUE~value)) %>%
    dplyr::filter(subRegion %in% subsetRegionsx)

  plist <- list()
  for(i in 1:length(unique(dataChartPlot$param))){

    plist[[i]] <-  ggplot2::ggplot(dataChartPlot %>%
                                     dplyr::filter(param==unique(dataChartPlot$param)[i]),
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



#' breaks
#'
#' returns breaks generated using k-mean and pretty
#' @param dataMap_raw_param Input dataframe
#' @param breaks_n Number of breaks
#' @importFrom magrittr %>%
#' @export

breaks <- function(dataMap_raw_param, breaks_n){
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

  return(list(breaks_kmean, breaks_pretty))
}


#' map
#'
#' returns map
#' @param flag indicates which type of map plot to return; 1 for abs val, 2 for abs prcnt val, 3 for abs ref scen
#' @param mapLegend map legend text
#' @param mapYear year of map
#' @param scenarioRefSelected selected reference scenario
#' @param dataMapx dataMapx
#' @param dataMapz if flag is 2 or 3, use dataPrcntAbsMapx
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

map <- function(flag,
               mapLegend,
               mapYear,
               scenarioRefSelected,
               dataMapx,
               dataMapz){

  # Initialize
  NULL->long->lat->group->scenario->rv->brks->subRegionMap

  gas <- 2

  if (flag == 3){
    dataMap_raw <- dataMapz %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
      dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)
  }else if (flag == 2){
    dataMap_raw <- dataMapz %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
      dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)
  }else{
    dataMap_raw <- dataMapx %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
      dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)
    gas<-1
  }

  US52Compact=F
  naColor = "green"
  breaks_n = 6
  #<!!>
  legendType = mapLegend
  palAbsChosen <- c("yellow2","goldenrod","darkred")
  yearsSelect <- mapYear
  paramsSelect <- unique(dataMap_raw$param)

  #Partitions Value into breaks of ... 6?
  z <- 1
  plist <- list()
  for(i in paramsSelect[!is.na(paramsSelect)]){
    print(i)
    print("++++++++++++++++=")
    if ((flag == 3)||(flag == 2)){
      proc <- process_map(dataMapx %>% dplyr::ungroup() %>%
                            dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
                            dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                                              TRUE~subRegion)) %>%
                            dplyr::select(-subRegionMap), i, mapLegend, mapYear)
      shp_df <- proc[[1]]
      dataMapPlot <- proc[[2]]
      paletteAbs <- proc[[3]]
      paletteDiff <- proc[[3]]

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

      # data_map, paletteDiff, paletteAbs
        map <- ggplot()
        if(!US52Compact){
          map <- map + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)}
        #map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray50", size = 1) +
        map <- map + geom_polygon(data = dataMapPlot %>% dplyr::filter(scenario == scenarioRefSelected),
                                  aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                  colour = "gray10", lwd=0.5) +
          scale_fill_manual(values=paletteAbs, na.value  = naColor, drop=FALSE) + theme_bw() +
          ylab("hello") +
          coord_fixed(ratio = 1.0,
                      ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
          )+
          scale_y_continuous(position = "left")+
          facet_grid(param~scenario, switch="y",
                     labeller = labeller(param = label_wrap_gen(15))
          ) +
          ylab(i) +
          xlab(NULL) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                strip.text.y = element_blank(),
                plot.margin=margin(0,0,0,0,"pt"),
                axis.title=element_text(10),
                axis.text=element_blank(),
                axis.ticks=element_blank())
        if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}

      plist[[z]] <- map
      z = z+1

      proc <- process_map(dataMap_raw, i, mapLegend, mapYear)
      shp_df <- proc[[1]]
      dataMapPlot <- proc[[2]]
      paletteAbs <- proc[[3]]
      paletteDiff <- proc[[3]]

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

      # data_map, paletteDiff, paletteAbs
        map <- ggplot()
        if(!US52Compact){
          map <- map + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)}
        #map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray50", size = 1) +
        map <- map + geom_polygon(data = dataMapPlot %>% dplyr::filter(scenario != scenarioRefSelected),
                                  aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                  colour = "gray10", lwd=0.5) +
          scale_fill_manual(values=paletteAbs, na.value  = naColor, drop=FALSE) + theme_bw() +
          xlab(NULL) +
          ylab(NULL) +
          coord_fixed(ratio = 1.0,
                      ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
          )+
          scale_y_continuous(position = "right")+
          facet_grid(param~scenario, switch="y",
                     labeller = labeller(param = label_wrap_gen(15))
          ) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                plot.margin=margin(0,0,0,0,"pt"),
                strip.text.y = element_blank(),
                axis.title=element_blank(),
                axis.text=element_blank(),
                axis.ticks=element_blank())
        if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
      plist[[z]] <- map
      z = z+1
    }else {
      proc <- process_map(dataMapx %>% dplyr::ungroup() %>%
                            dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
                            dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                                              TRUE~subRegion)) %>%
                            dplyr::select(-subRegionMap), i, mapLegend, mapYear)
      shp_df <- proc[[1]]
      dataMapPlot <- proc[[2]]
      paletteAbs <- proc[[3]]
      paletteDiff <- proc[[3]]

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

      # data_map, paletteDiff, paletteAbs
      if(T){
        map <- ggplot()
        if(!US52Compact){
          map <- map + geom_polygon(data = shp_bg, aes(x = long, y = lat, group = group),colour = "gray40", fill = "gray90", lwd=0.5)}
        #map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = id),color="gray50", size = 1) +
        map <- map +geom_polygon(data =  dataMapPlot,
                                 aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                 colour = "gray10", lwd=0.5) +
          scale_fill_manual(values=paletteAbs, na.value  = naColor, drop=FALSE) + theme_bw() +
          coord_fixed(ratio = 1.0,
                      ylim=c(latLimMin,latLimMax),xlim=c(max(-180,longLimMin),longLimMax),expand = c(0, 0)) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
          )+
          scale_y_continuous(position = "left")+
          facet_grid(param~scenario, switch="y",
                     labeller = labeller(param = label_wrap_gen(15))
          ) +
          ylab(i) +
          xlab(NULL) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                strip.text.y = element_blank(),
                plot.margin=margin(0,0,0,0,"pt"),
                axis.title=element_text(10),
                axis.text=element_blank(),
                axis.ticks=element_blank())
        if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
      }; map
      plist[[i]] <- map
    }
  }
  # temp <- cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
  temp <- cowplot::plot_grid(plotlist=plist,ncol=gas,align = "v", rel_widths = c(1, length(unique(dataMapx$scenario))-1))
  # ggsave("~/Desktop/mapz.png",temp)
  return(temp)
}

#' process map
#'
#' returns map
#' @param dataMap_raw input dataframe
#' @param i parameter/category for dataframe
#' @param i the
#' @param mapYear the year of the map
#' @param mapLegend map legend text
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

process_map <- function(dataMap_raw,
                        i,
                        mapLegend,
                        mapYear){

  # Initialize
  NULL->long->lat->group->scenario->brks->x->param->subRegion->value

  US52Compact=F
  naColor = "green"
  breaks_n = 6
  legendType = mapLegend
  palAbsChosen <- c("yellow2","goldenrod","darkred")
  yearsSelect <- mapYear
  paramsSelect <- unique(dataMap_raw$param)

  dataMap_raw_param <- dataMap_raw %>%
    dplyr::filter(x==yearsSelect,
                  param == i); dataMap_raw_param

  breaks<- argus::breaks(dataMap_raw_param, breaks_n)

  if(legendType=="kmean"){breaks_map = breaks[[1]]}else if(
    legendType=="pretty"){breaks_map = breaks[[2]]}

  # breaks_map <- breaks_map %>%
  #   format(big.mark=",", scientific=F);
  paletteDiff <- ""
  breaks_map <- breaks_map%>%unique()
  if(length(breaks_map)==1){
    data_map <- dataMap_raw_param %>%
      dplyr::mutate(brks = format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ","))
    paletteAbs = "red"
    if(length(unique(format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ",")))!=1){
      breaks_map = format(unique(dataMap_raw_param$value), nsmall=2, digits=2, big.mark = ",")
      paletteAbs <- grDevices::colorRampPalette(palAbsChosen)(length(breaks_map)); paletteAbs
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
    paletteAbs <- grDevices::colorRampPalette(palAbsChosen)(length(breaks_map_levels)); paletteAbs
    paletteDiff <- "BrBG"
  }


  data_map%>%utils::head()

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
    dplyr::filter(subRegion!="South_Pacific_Islands")%>%
    dplyr::group_by(subRegion) %>%
    dplyr::mutate(minLong = min(long),
                  negLongSum = sum(long[which(long<=0)], na.rm=T),
                  maxLong = max(long),
                  posLongSum = sum(long[which(long>=0)], na.rm=T),
                  flip = dplyr::case_when(minLong<-160 & maxLong>160 ~ 1,
                                   TRUE~0),
                  long = dplyr::case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
                                   (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
                                   TRUE~long))%>%
    dplyr::ungroup(); dataMapPlot %>% utils::head()
  # data_map%>%head()
  return(list(shp_df, dataMapPlot, paletteAbs, paletteDiff))
}
