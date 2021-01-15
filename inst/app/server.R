#' server

#---------------------------
# Libraries Needed
#---------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(cowplot)
library(rdataviz)
library(rmap)
library(shinyWidgets)
library(tools)
library(RCurl)
library(zip)
library(tmap)
library(leaflet)
library(leafsync)
library(rgcam)
library(yaml)
library(plyr)

#---------------------------
# Overall Strtucture
#---------------------------

# Side Bar:
#   - Input csv
#   - Input gcam project
#   - Input scenarios
#   - Input ref scenario
#   - Input parameters
#   - Input regions
#   - Input years
# Main Panel:
#   - Summary (Summary plots aggregated by chosen params and regions)
#   - Charts (Bar Charts comparing scenarios aggregated by chosen params and regions)
#   - Maps (Maps comapring scenarios aggregated by params for single years)
#   - Table (Raw data table being used)

#---------------------------
# Server object
#---------------------------

server <- function(input, output, session) {
  #---------------------------
  # Load Default Datasets from rdataviz
  #---------------------------
  dataDefault <- rdataviz::exampleData
  # settingsDefault <- rdataviz::
  map <- rmap::mapGCAMReg32
  ggplottheme <- ggplot2::theme_bw()
  # session$sendCustomMessage("setsetting", c("data", unique(dataSum()$scenario)))
  #---------------------------
  # Settings
  #---------------------------


  output$downloadSettings <- downloadHandler(
    filename = "settings.yaml",
    content = function(filename) {
      write_yaml(reactiveValuesToList(input), filename)
  })


  settings <- reactive({
    print(input$settingdata)
    if (is.null(input$settingdata)){
      return(reactiveValuesToList(input))
    }else{
      return(read_yaml(input$settingdata$datapath))
    }
  })

  observeEvent(input$settingdata,{
    setValues(settings())
  })

  observeEvent(input$defaultsetting,{
    setValues(read_yaml("../extdata/settings.yaml"))
  })

  #gcamdatapath: file filemap: file paramsSelected:  scenarioSelected filedata settingdata github urlfiledata regionsselected  help tabs do

#     observeEvent(input$scenariosSelected,{
#       print(input)
#       print(input$scenariosSelected)
#     })

  # tabPanel(
  #   "All",
  #   br(),
  #   fluidRow(column(6, p(
  #     'Sum of Regions Selected'
  #   )),
  #   column(
  #     6, div(
  #       downloadButton(
  #         'downloadPlotSum',
  #         NULL,
  #         download = "summaryChart.png",
  #         class = "download_button"
  #       ),
  #       style = "float: right"
  #     )
  #   )),
  #   div(
  #     class="charts",
  #     plotOutput(outputId = "summary")
  #   ),
  #   width = "100%"
  # ),

  observeEvent(input$loadsetting, {
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        fileInput(
          inputId = "settingdata",
          label = "Upload yaml",
          accept = c(".yaml"),
          multiple = TRUE,
          width = "100%"
          ),
        fluidRow(
          column(6,
                 div(
                   tags$a(tags$i(class="fas fa-cog",
                                 style="float:center;margin-right:5px"),
                          id = "downloadSettings",
                          class = "btn btn-default shiny-download-link download_button",
                          href = "",
                          target = "_blank",
                          download = NA, "Save Settings"),
                   style = "float:center"
                 )),
          column(6,
                 div(actionLink(inputId='defaultsetting',
                                label='Default Setting',
                                class = "btn btn-default shiny-download-link download_button",
                                icon = icon("cog","fa-1x"
                                            ),
                      ),
                     )
                 # div(
                 #
                 #   tags$a(tags$i(class="fas fa-cog",
                 #                 style="float:center;margin-right:5px"),
                 #          id = "defaultsetting",
                 #          class = "btn btn-default shiny-download-link download_button",
                 #          "Default Settings")
                 # ))
            )
          )
        )
      )
  })



   setValues <- function(setting){
     for (x in names(setting)){
       print(x)
       if ((x == "regionsSelected")||(x == "scenariosSelected")||(x=="paramsSelected")||(x=="scenarioRefSelected")||(x == "subsetRegions")){
         updatePickerInput(session, x, selected = setting[[x]])
         print(x)
         print(setting[[x]])
       }
     }
   }

  #---------------------------
  # Data File (GCAM)
  #---------------------------

  gcamdatabasepathx <- reactive({
    if (dir.exists(input$gcamdatabasepath)) {
      input$gcamdatabasepath
    } else {
      "GCAM database entered does not exist."
    }
  })

  #output$text <- renderText({print(dataGCAMx())})


  # Get names of scenarios in GCAM database.....................
  gcamScenariosx <- reactive({

  if(!is.null(gcamdatabasepathx()) & gcamdatabasepathx()!="GCAM database entered does not exist."){
  gcamdatabasePath_dir <- gsub("/$","",gsub("[^/]+$","",gcamdatabasepathx())); gcamdatabasePath_dir
  gcamdatabasePath_file <- gsub('.*/ ?(\\w+)', '\\1', gcamdatabasepathx()); gcamdatabasePath_file
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
  as.vector(unlist(strsplit(s2,",")))
  }else{
    gcamdatabasepathx()
  }
  })

  output$gcamScenarios = renderUI({
    if(!is.null(gcamdatabasepathx()) & gcamdatabasepathx()!="GCAM database entered does not exist."){
    pickerInput(
      inputId = "gcamScenariosSelected",
      label = "Select Available GCAM Scenarios",
      choices = unique(gcamScenariosx()),
      selected = unique(gcamScenariosx()),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )}else{
      NULL
    }
  })

  #...................................
  # Create data table from database
  dataGCAMx <- reactive({

    if(!is.null(gcamdatabasepathx()) & gcamdatabasepathx()!="GCAM database entered does not exist."){
    tempdir <- paste(getwd(),"/tempdir",sep="")
    dir.create(tempdir)
    gcamdatabasepath_i <- gcamdatabasepathx()
    scenOrigNames_i <- input$gcamScenariosSelected
    scenNewNames_i <- paste(input$gcamScenariosSelected,"NEW",sep="")
    regionsSelect_i <- "Southeast Asia"
    paramsSelect_i <- c("gdp","pop","agProdByCrop")

    dataGCAMraw <- rdataviz::readgcam(reReadData = T,
                                      dirOutputs = tempdir,
                                      gcamdatabase = gcamdatabasepath_i,
                                      scenOrigNames = scenOrigNames_i,
                                      scenNewNames = scenNewNames_i,
                                      dataProj = "projFile",
                                      #dataProjPath = dataProjPath_i,
                                      regionsSelect = regionsSelect_i,
                                      paramsSelect= paramsSelect_i)

    unlink(tempdir, recursive = T)

    dataGCAMraw$data %>% as_tibble() %>%
      dplyr::select(scenario, region, subRegion, param,
                    class1, class2, x, vintage, aggregate, units,
                    value) %>%
      dplyr::rename(class=class1)-> dataGCAM

    dataGCAM
    } else {
      NULL
    }
  })


  #---------------------------
  # Data File (CSV)
  #---------------------------
  data_raw <- reactive({
    if (is.null(input$filedata) & is.null(dataGCAMx()) & ("" == input$urlfiledata)) {
      print("memes")
      rdataviz::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      )
    } else if(!is.null(input$filedata) & is.null(dataGCAMx()) & ("" == input$urlfiledata)) {
      rdataviz::addMissing(
        rdataviz::parse_local(input)%>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      )
    } else if(is.null(input$filedata) & !is.null(dataGCAMx()) & ("" == input$urlfiledata)){
      dataGCAMx() %>%
        dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
    }else{
      rdataviz::addMissing(
       rdataviz::parse_remote(input)%>%
         dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      )
    }
  })

  data <- reactive({
    # Aggregate across classes
    tblAggsums <- data_raw() %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, subRegion, param, aggregate, class, x, value)%>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data_raw() %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, subRegion, param, aggregate, class, x, value)%>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
  })

  #---------------------------
  # Scenarios Select
  #---------------------------
  output$selectScenarios = renderUI({
    # if(is.null(input$scenariosSelected)){
    #   print("sentz")
    #   session$sendCustomMessage("setsetting", c("scenariosSelected", unique(dataSum()$scenario)))
    #   updatePickerInput(session, "scenariosSelected", selected = unique(dataSum()$scenario))
    # }
    pickerInput(
      inputId = "scenariosSelected",
      label = "Select Scenarios",
      choices = unique(dataSum()$scenario),
      selected = unique(dataSumx()$scenario),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Ref Scenario Select
  #---------------------------
  output$selectRefScenarios = renderUI({
    pickerInput(
      inputId = "scenarioRefSelected",
      label = "Select Ref Scenario",
      choices = unique(dataSumx()$scenario),
      selected = unique(dataSumx()$scenario)[1],
      multiple = F
    )
  })

  #---------------------------
  # Parameters Select
  #---------------------------
  output$selectParams = renderUI({
    # if(is.null(input$selectParams)){
    #   print("sentzX")
    #   # session$sendCustomMessage("setsetting", c("scenariosSelected", unique(dataSum()$scenario)))
    #   updatePickerInput(session, "paramsSelected", selected = "Chosen Mix")
    # }
    pickerInput(
      inputId = "paramsSelected",
      label = "Select Params",
      choices = c("Chosen Mix", unique(dataSum()$param)),
      selected = paramsSelectedx(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Regions Select
  #---------------------------
  output$selectRegions = renderUI({
    # if(is.null(input$regionsSelected)){
    #   print("sentzX")
    #   # session$sendCustomMessage("setsetting", c("scenariosSelected", unique(dataSum()$scenario)))
    #   updatePickerInput(session, "regionsSelected", "All")
    # }
    pickerInput(
      inputId = "regionsSelected",
      label = "Select Regions",
      choices = c("All", unique(data()$subRegion)),
      selected = "All",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Reactive Regions Select based on inputs
  #---------------------------
  regionsSelectedx <- reactive({
    if (input$regionsSelected == "All" && length(input$regionsSelected) == 1) {
      return(unique(data()$subRegion))
    } else if (is.null(input$regionsSelected)){
      return(unique(data()$subRegion))
    }else{
      return(input$regionsSelected)
    }
  })

  #---------------------------
  # Reactive Params based on inputs
  #---------------------------
  paramsSelectedx <- reactive({
    if (any(input$paramsSelected == "Chosen Mix") &&
        length(input$paramsSelected) == 1) {
      paramsCheck <- unique(data()$param)[unique(data()$param) %in%
                                            rdataviz::constants()$chosenMix]
      if (length(paramsCheck) >= 1) {
        paramsCheck
      } else{
        unique(data()$param)
      }
    }else if (is.null(input$paramsSelected)){
      print("memextz")
      paramsCheck <- c("Chosen Mix", unique(data()$param)[unique(data()$param) %in%
                                            rdataviz::constants()$chosenMix])
      return(paramsCheck)
    } else{
      print("memeasd")
      input$paramsSelected
    }
  })

  scenarioSelectedx <- reactive({
    if (input$scenariosSelected == "All" && length(input$scenariosSelected) > 0) {
      print("meme1")
      return(unique(dataSum()$scenario))
    } else if (is.null(input$scenariosSelected)){
      print("meme2")
      return(unique(dataSum()$scenario))
    }else{
      print("meme3")
      print(input$scenariosSelected)
      return(input$scenariosSelected)
    }
  })

  #---------------------------
  # Data Summary
  #---------------------------
  dataSum <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::select(-class) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

  })

  # Filter Data after Reactive Choices -------------------
  dataSumx <- reactive({
    # print("----")
    print(unique(scenarioSelectedx()))
    print(paramsSelectedx())
    # print("----")
    x <- dataSum() %>%
      dplyr::filter(scenario %in% scenarioSelectedx(),
                    param %in% paramsSelectedx())
    print(x)
    return(x)
  })

  #---------------------------
  # Data Map
  #---------------------------
  dataMap <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, subRegion, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::select(-class) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, subRegion, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

  })

  # Filter Data after Reactive Choices -------------------
  dataMapx <- reactive({
    dataMap() %>%
      dplyr::filter(scenario %in% input$scenariosSelected,
                    param %in% paramsSelectedx())
  })


  #---------------------------
  # Data Chart
  #---------------------------
  dataChartx <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(
        scenario %in% input$scenariosSelected,
        param %in% paramsSelectedx(),
        subRegion %in% regionsSelectedx()
      ) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, class, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(
        scenario %in% input$scenariosSelected,
        param %in% paramsSelectedx(),
        subRegion %in% regionsSelectedx()
      ) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, class, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
  })

  #---------------------------
  # Data Chart Absolute Diff
  #---------------------------
  dataDiffAbsx <- reactive({
    diffText <- " Diff Abs"

    if (is.null(input$scenarioRefSelected)) {
      print(paste("No reference scenario provided", sep = ""))
      print(paste(
        "Using ",
        unique(dataChartx()$scenario)[1],
        " as reference",
        sep = ""
      ))
      scenRef_i = unique(dataChartx()$scenario)[1]
    } else{
      if (!input$scenarioRefSelected %in% unique(dataChartx()$scenario)) {
        print(paste(
          "scenario ",
          input$scenarioRefSelected,
          " not in scenarios",
          sep = ""
        ))
        print(paste(
          "Using ",
          unique(dataChartx()$scenario)[1],
          " as reference",
          sep = ""
        ))
        scenRef_i = unique(dataChartx()$scenario)[1]
      } else{
        scenRef_i <- input$scenarioRefSelected
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataChartx() %>%
      dplyr::filter(scenario == scenRef_i)

    for (k in unique(dataChartx()$scenario)[unique(dataChartx()$scenario) !=
                                            scenRef_i]) {
      tbl_temp <- dataChartx() %>%
        dplyr::filter(scenario %in% c(scenRef_i, k))
      tbl_temp <- tbl_temp %>%
        tidyr::spread(scenario, value)

      tbl_temp[is.na(tbl_temp)] <- 0

      tbl_temp <- tbl_temp %>%
        dplyr::mutate(!!paste(k, diffText, sep = "") := get(k) - get(scenRef_i)) %>%
        dplyr::select(-dplyr::one_of(c(k, scenRef_i)))
      tbl_temp <- tbl_temp %>%
        tidyr::gather(key = scenario, value = value, -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k, diffText, sep =
                                                                                                      "")]))
      tbl_pd <- dplyr::bind_rows(tbl_pd, tbl_temp)
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    tbl_pd
  })


  #---------------------------
  # Summary Plot
  #---------------------------
  summaryPlot <- function(aspectratio, textsize, titletext){
    ggplot2::ggplot(dataSumx(),
                    aes(x=x,y=value,
                        group=scenario,
                        color=scenario))+
      geom_line(size=1.25) +
      ggplottheme +
      geom_line() +
      ylab(NULL) +  xlab(NULL) +
      facet_wrap(.~param, scales="free", ncol = 3,
                 labeller = labeller(param = label_wrap_gen(15)))+
      theme(legend.position="top",
            legend.text=element_text(size=titletext),
            legend.title = element_blank(),
            plot.margin=margin(20,20,20,0,"pt"),
            text=element_text(size=textsize),
            aspect.ratio = aspectratio
            )
  }

  output$summary <- renderPlot({
    summaryPlot(NULL, 17.5, 20)
  },
  height=function(){
    if (length(unique(dataChartx()$param))%%3==0){
      return(((length(unique(dataChartx()$param))%/%3))*250)
    }else{
      return(((length(unique(dataChartx()$param))%/%3)+1)*250)
    }
  }
  )
  output$downloadPlotSum <- downloadHandler(
    filename = "summaryPlot.png",
    content = function(file) {
      ggsave(
        file,
        plot=summaryPlot(0.75, 10, 10),
        #max(13,min(13,1.25*length(unique(dataChartx()$param)))),
        height = rdataviz::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
        width=rdataviz::exportWidth(10, length(unique(dataChartx()$param)), 3),
        units="in"
      )
    })

    # exportHeight<-function(chartsperrow, max_height_in, numelement, lenperchart){
    #   if (numelement%%chartsperrow==0){
    #     return(min(max_height_in, ((numelement%/%chartsperrow))*lenperchart))
    #   }else{
    #     return(min(max_height_in, ((numelement%/%chartsperrow)+1)*lenperchart))
    #   }
    # }
    # exportWidth<-function(max_width_in, numelement, lenperchart){
    #   print(numelement)
    #   return(min(max_width_in, (numelement)*lenperchart))
    # }

    #---------------------------
    # Subset Regions Selected
    #---------------------------
    output$subsetRegions = renderUI({
      pickerInput(
        inputId = "subsetRegions",
        label = "Select Regions to Compare",
        choices = unique(dataMap()$subRegion),
        selected = subsetRegionsx(),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "None",
          `select-all-text` = "All",
          `none-selected-text` = "None Selected"
      ))
    })

    #---------------------------
    # Reactive Regions Select based on inputs
    #---------------------------
    subsetRegionsx <- reactive({
      if (input$subsetRegions == "All" && length(input$subsetRegions) == 1) {
        return(unique(dataMapx()$subRegion))
      } else if (is.null(input$subsetRegions)){
        return(unique(dataMapx()$subRegion)[1:4])
      } else{
        return(input$subsetRegions)
      }
    })

    #---------------------------
    # Summary Plot Compare Regions
    #---------------------------
    summaryPlotReg <- function(titletext){

      dataChartPlot <- # All regions
        dataMapx() %>% tidyr::complete(scenario,param,subRegion,x) %>%
        dplyr::mutate(value=case_when(is.na(value)~0,
                                      TRUE~value))%>%
        dplyr::filter(subRegion %in% subsetRegionsx())

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


    output$summaryReg <- renderPlot({
      summaryPlotReg(10)
    },
    height=function(){200*length(unique(dataMapx()$param))}
    # width=function(){200*length(subsetRegionsx())}
    )

    output$downloadPlotSumReg <- downloadHandler(
      filename = "summaryChartReg.png",
      content = function(file) {
        ggsave(file,plot=summaryPlotReg(10),
               # width=min(49,max(15,1*length(unique(dataMapx()$subRegion))),
               # height=min(49,max(12,1*length(unique(dataMapx()$param)))),units="in")
               height = rdataviz::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
               width = rdataviz::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
               units = "in")
      })


  #---------------------------
  # Chart Plot
  #---------------------------
  chartPlot <- function(){

    dataChartPlot <- dataDiffAbsx()

    plist <- list()
    for(i in 1:length(unique(dataChartPlot$param))){

      # Check Color Palettes
      palAdd <- rmap::colors()$pal_Basic
      missNames <- unique(dataChartPlot$class)[!unique(dataChartPlot$class) %in%
                                                     names(rmap::colors()$pal_rmap)]
      if (length(missNames) > 0) {
        palAdd <- palAdd[1:length(missNames)]
        names(palAdd) <- missNames
        palCharts <- c(rmap::colors()$pal_rmap, palAdd)
      } else{
        palCharts <- rmap::colors()$pal_rmap
      }

      plist[[i]] <-  ggplot2::ggplot(dataChartPlot %>%
                                       filter(param==unique(dataChartPlot$param)[i])%>%
                                       droplevels(),
                                     aes(x=x,y=value,
                                         group=scenario,
                                         fill=class)) +
        ggplottheme +
        ylab(NULL) + xlab(NULL) +
        scale_fill_manual(breaks=names(palCharts),values=palCharts) +
        scale_y_continuous(position = "right")+
        geom_bar(position="stack", stat="identity") +
        facet_grid(param~scenario, scales="free",switch="y")+
        theme(legend.position="bottom",
              legend.title = element_blank(),
              legend.margin=margin(0,0,0,0,"pt"),
              legend.key.height=unit(0, "cm"),
              text = element_text(size = 12.5),
              plot.margin=margin(20,20,20,0,"pt"))}
    cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
  }


  output$plot <- renderPlot({
    chartPlot()
  },
  height=function(){300*length(unique(dataChartx()$param))}
  )

  output$downloadPlotChart <- downloadHandler(
    filename = "barChart.png",
    content = function(file) {
      ggsave(file,plot=chartPlot(),
      width=rdataviz::exportWidth(49, length(unique(dataChartx()$param)), 5),
      height=rdataviz::exportHeight(1, 49, length(unique(dataChartx()$param)), 5)+2,
      unit = "in"
      )
      # exportHeight<-function(chartsperrow, max_height_in, numelement, lenperchart){
        # max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
    })

  #---------------------------
  # Maps
  #---------------------------

    output$map <- renderUI({

      dataMapxi = dataMapx() %>%
        filter(param %in% paramsSelectedx()[1],
               scenario %in% input$scenariosSelected[1],
               x %in% c("2010"))

      mapx <- (rmap::mapFind(dataMapxi))$subRegShapeFound;
      mapx@data <- mapx@data %>%
        dplyr::left_join(dataMapxi)%>%
        dplyr::select("subRegion","value")%>%
        unique(); mapx@data
      mapx_1 <- tm_shape(mapx) +
        tm_polygons(col = "value") +
        tm_layout(legend.outside = T,
                  legend.show = F)

      m1<-tmap_leaflet(mapx_1) %>% clearControls()
      m2<-tmap_leaflet(mapx_1)
      sync(m1,m2,ncol=1)
    })

  #---------------------------
  # Data Table
  #---------------------------
  output$table <- renderDT(
    # Point to reactive values
    data(),
    filter = "top"
  )

  output$downloadTable <- downloadHandler(
    file = "table.csv",
    content = function(file) {
      print(class(data()))
      write.csv(data() , file)
    })

  #---------------------------
  # Download All
  #---------------------------
  output$downloadAll <- downloadHandler(
    file = "all.zip",
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- c("table.csv", "summaryCharts.png", "barCharts.png")
      write.csv(data(), "table.csv")
      ggsave("summaryCharts.png",plot=summaryPlot(),
             height = sum_hi(3, 49),
             width=sum_wi(3, 49),
             units="in")
      ggsave("barCharts.png",plot=chartPlot(),width=13,height=max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
      print(fs)
      zip::zip(zipfile=file, files=fs)
    }
  )


}
