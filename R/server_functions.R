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
#' @param input number of columns, max height, number of planned elements, height per element
#' @importFrom magrittr %>%
#' @export
exportHeight<-function(chartsperrow, max_height_in, numelement, lenperchart){
  if (numelement%%chartsperrow==0){
    return(min(max_height_in, ((numelement%/%chartsperrow))*lenperchart))
  }else{
    return(min(max_height_in, ((numelement%/%chartsperrow)+1)*lenperchart))
  }
}

#' exportWidth
#'
#' generate width for exported images
#' @param input max width,number of planned elements,width per element
#' @importFrom magrittr %>%
#' @export
exportWidth<-function(max_width_in, numelement, lenperchart){
  return(min(max_width_in, (numelement)*lenperchart))
}
