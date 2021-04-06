#' server

#---------------------------
# Libraries Needed
#---------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(cowplot)
library(argus)
library(shinyWidgets)
library(tools)
library(RCurl)
library(zip)
library(tmap)
library(leaflet)
library(leafsync)
library(rgcam)
#library(plyr)
library(broom)

#---------------------------
# Options
#---------------------------
options(shiny.maxRequestSize=100*1024^2)
pal_all <- argus::mappings()$pal_all

#---------------------------
# Server object
#---------------------------

server <- function(input, output, session) {

  #---------------------------
  # Load Default Datasets from argus
  #---------------------------
  dataDefault <- argus::exampleData
  ggplottheme <- ggplot2::theme_bw()
  # session$sendCustomMessage("setsetting", c("data", unique(dataSum()$scenario)))


  #---------------------------
  # Settings
  #---------------------------

  # Initialize Settings
  settings <- reactive({data.frame()})
  settingsVars <- c("urlSelect",
                    "regionsSelect",
                    "scenariosSelect",
                    "scenarioRefSelect",
                    "paramsSelect")
  # Create Modal for Settings Download and Loading
  observeEvent(input$loadsetting, {
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        fileInput(
          inputId = "settingdata",
          label = "Upload csv",
          accept = c(".csv"),
          multiple = TRUE,
          width = "100%"
        ),
        fluidRow(
          column(6,
                 div(downloadButton(
                   outputId='downloadSettings',
                   label="Save Settings",
                   download = "settings.csv",
                   class = "download_button"),
                   style = "float:center"
                 ))
          ,
          column(6,
                 div(actionLink(inputId='defaultsetting',
                                label='Default Setting',
                                class = "btn btn-default shiny-download-link download_button",
                                icon = icon("cog","fa-1x")
                 )
                 )
          )
        )
      )
    )
  })


  observeEvent(input$help,{
    session$sendCustomMessage("handler1", unique(data()$subRegion))
  })

  # Download Settings
  output$downloadSettings <- downloadHandler(
    filename = "settings.csv",
    content = function(filename) {
      write.csv(data.frame(variable=settingsVars,
                           value=c(paste(input$urlfiledata,collapse=";"),
                                   paste(regionsSelectedx(),collapse=";"),
                                   paste(scenariosSelectedx(),collapse=";"),
                                   paste(scenarioRefSelectedx(),collapse=";"),
                                   paste(paramsSelectedx(),collapse=";"))),
                file=filename,
                row.names = F)
    })




  # Load Settings Data if Selected
  observeEvent(input$settingdata,{
    settings <- reactive({
      if (!is.null(input$settingdata)){
        return(read.csv(input$settingdata$datapath,header=T)%>%as.data.frame())
      }
    })
    print("Settings file loaded.")
    print(settings())

    # Make sure all variables in settings are valid
    if(any(!settingsVars %in% unique(settings()$variable))){
     showModal(modalDialog(
        title = "Settings Variable Error.",
        print(paste("Setting variable(s) not valid: ",
                    paste(unique(settings()$variable)[
                      !unique(settings()$variable) %in% settingsVars],collapse=", "),
              sep=""))
     ))
    } else {

    # Update All Reactive Inputs Based on Settings Loaded
    if(nrow(settings())>0){

      # URL Update
      # settingsURL <- unlist(
      #   strsplit(
      #     (settings()%>%dplyr::filter(variable=="urlSelect"))$value
      #     ,split=";")
      # )
      # updateTextInput(
      #     session=session,
      #     inputId = "urlfiledata",
      #     value = settingsURL,
      #   )


      # Regions Update
      settingsRegions <- unlist(
        strsplit(
          (settings()%>%dplyr::filter(variable=="regionsSelect"))$value
          ,split=";")
      )
      if(any(unique(settingsRegions) %in% unique(data()$subRegion))){
      updatePickerInput(
        session=session,
        inputId = "regionsSelected",
        selected = unique(settingsRegions)[unique(settingsRegions) %in% unique(data()$subRegion)],
      )
      } else{
        showModal(modalDialog(
          title = "Settings Region Error.",
          "None of the regions selected in the settings file are available in the data.
          Using default selection."
        ))
      }

      # Parameters Update
      settingsParams <- unlist(
        strsplit(
          (settings()%>%dplyr::filter(variable=="paramsSelect"))$value
          ,split=";")
      )
      if(any(unique(settingsParams) %in% unique(data()$param))){
        updatePickerInput(
          session=session,
          inputId = "paramsSelected",
          selected = unique(settingsParams)[unique(settingsParams) %in% unique(data()$param)],
        )
      } else{
        showModal(modalDialog(
          title = "Settings Param Error.",
          "None of the params selected in the settings file are available in the data.
          Using default selection."
        ))
      }

      # Scenario Update
      settingsScenario <- unlist(
        strsplit(
          (settings()%>%dplyr::filter(variable=="scenariosSelect"))$value
          ,split=";")
      )
      if(any(unique(settingsScenario) %in% unique(data()$scenario))){
        updatePickerInput(
          session=session,
          inputId = "scenariosSelected",
          selected = unique(settingsScenario)[unique(settingsScenario) %in% unique(data()$scenario)],
        )
      } else{
        showModal(modalDialog(
          title = "Settings Scenario Error.",
          "None of the scenarios selected in the settings file are available in the data.
          Using default selection."
        ))
      }

      # Reference Scenario Update
      settingsRefScenario <- unlist(
        strsplit(
          (settings()%>%dplyr::filter(variable=="scenarioRefSelect"))$value
          ,split=";")
      )
      if(any(unique(settingsRefScenario) %in% unique(data()$scenario))){
        updatePickerInput(
          session=session,
          inputId = "scenarioRefSelected",
          selected = unique(settingsRefScenario)[unique(settingsRefScenario) %in% unique(data()$scenario)],
        )
      } else{
        showModal(modalDialog(
          title = "Settings Ref Scenario Error.",
          "The Ref Scenario selected in the settings file is not available in the data.
          Using default selection."
        ))
      }

    }
  }
  })



  # Attempt to read settings if selected
  observeEvent(input$settingdata,
               if(!is.null(input$settingdata)){
                 if(grepl(".csv",input$settingdata$datapath)){
                   #removeModal()
                 }else{
                   showModal(modalDialog(
                     title = "Incorrect file type loaded",
                     "Settings file must be a .csv"
                   ))
                 }
               })

  # Attempt to read default settings if selected
  observeEvent(input$defaultsetting,{

    settings <- reactive({
      return(data.frame(variable=settingsVars) %>%
                          dplyr::mutate(value="Default"))
    })

    #---------------------------
    # Update input File to Default (NULL)
    #---------------------------
    rv$filedatax <- NULL
    rv$selectedx <- NULL

    #---------------------------
    # Scenarios Select
    #---------------------------
   updatePickerInput(
        inputId = "scenariosSelected",
        session=session,
        choices = unique(data()$scenario),
        selected = unique(data()$scenario))

    #---------------------------
    # Ref Scenario Select
    #---------------------------
    updatePickerInput(
        inputId = "scenarioRefSelected",
        session=session,
        choices = unique(data()$scenario)[unique(data()$scenario)
                                             %in% scenariosSelectedx()],
        selected = (unique(data()$scenario)[unique(data()$scenario)
                                               %in% scenariosSelectedx()])[1])

    #---------------------------
    # Parameters Select
    #---------------------------
    updatePickerInput(
        inputId = "paramsSelected",
        session=session,
        choices = c("Chosen Mix", unique(data()$param)),
        selected = unique(data()$param)[1:5])

    #---------------------------
    # Regions Select
    #---------------------------
    updatePickerInput(
        inputId = "regionsSelected",
        session=session,
        choices = unique(data()$subRegion),
        selected = unique(data()$subRegion))

    #---------------------------
    # Subset Regions Selected
    #---------------------------
    updatePickerInput(
        inputId = "subsetRegions",
        session=session,
        choices = unique(data()$subRegion),
        selected = unique(data()$subRegion)[1:4])
  })

  # Close Modal After Loading Default
  observeEvent(input$defaultsetting,
               if(!is.null(input$defaultsetting)){
                 removeModal()
               })



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

      dataGCAMraw <- argus::readgcam(reReadData = T,
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

  # Create your own reactive values that you can modify because input is read only
  rv <- reactiveValues()


  rv$pcount = 1;

  # Charts initializing abs, percDiff, and absDiff
  rv$absChart = 1;
  rv$percDiffChart = 0;
  rv$absDiffChart = 0;


  # Maps initializing abs, percDiff, and absDiff
  rv$absMap = 1;
  rv$percDiffMap = 0;
  rv$absDiffMap = 0;


    # Observe File Inputs
  observeEvent(input$filedata, {
    rv$filedatax=input$filedata
    # URL Update
    updateTextInput(
      session=session,
      inputId = "urlfiledata",
      value = "",
    )
    })
  observeEvent(input$urlfiledata, {
    rv$urlfiledatax=input$urlfiledata})
  eventReactive(input$urlfiledata, {
    rv$filedatax=NULL})

  # Read in Raw Data
  data_raw <- reactive({
    if (is.null(rv$filedatax) & is.null(dataGCAMx()) & ("" == rv$urlfiledatax)) {
      return(argus::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
      rv$filedatax <- NULL
      rv$urlfiledatax <- NULL
    } else if(!is.null(rv$filedatax) & is.null(dataGCAMx()) & ("" == rv$urlfiledatax)) {
      return(argus::addMissing(
        argus::parse_local(input)%>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
      rv$filedatax <- NULL
      rv$urlfiledatax <- NULL
    } else if(is.null(rv$filedatax) & !is.null(dataGCAMx()) & ("" == rv$urlfiledatax)){
      return(dataGCAMx() %>%
        dplyr::select(scenario, subRegion, param, aggregate, class, x, value))
      rv$filedatax <- NULL
      rv$urlfiledatax <- NULL
    }else{
      return(argus::addMissing(
        argus::parse_remote(input)%>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
      rv$filedatax <- NULL
      rv$urlfiledatax <- NULL
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
    pickerInput(
      inputId = "scenariosSelected",
      label = "Select Scenarios",
      choices = unique(data()$scenario),
      selected = unique(data()$scenario),
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
      choices = unique(data()$scenario)[unique(data()$scenario)
                                           %in% scenariosSelectedx()],
      selected = (unique(data()$scenario)[unique(data()$scenario)
                                             %in% scenariosSelectedx()])[1],
      multiple = F
    )
  })

  #---------------------------
  # Parameters Select
  #---------------------------
  output$selectParams = renderUI({
    pickerInput(
      inputId = "paramsSelected",
      label = "Select Params",
      choices = c("Chosen Mix", unique(data()$param)),
      selected = unique(data()$param)[1:5],
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
    pickerInput(
      inputId = "regionsSelected",
      label = "Select Regions",
      choices = unique(data()$subRegion),
      selected = unique(data()$subRegion),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

 output$mymapBase <- renderLeaflet({


    dataMap_raw <- data() %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
      dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)

    plist <- list()
    pcount = 1
    subRegTypelist <- c()
    z <- leaflet(height = "100vh") %>% addTiles()
    print(dataMap_raw$subRegion)
    print("===============================================")
    for(i in unique(dataMap_raw$param)[!is.na( unique(dataMap_raw$param))]){
      dataMap_raw_regions <- dataMap_raw %>%
        dplyr::filter(subRegion!="South_Pacific_Islands")%>%
        dplyr::filter(param == i) %>%
        dplyr::select(subRegion) %>%
        unique(); dataMap_raw_regions

      dataMapPlot <- argus::mapdfFind(dataMap_raw_regions)%>%
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
          print(dataMapPlot)
          subRegTypelist[pcount] <- unique(dataMapPlot$subRegionType)
          pcount = pcount+1
          #a <-  dataMapPlot %>% group_by(subRegion) %>% group_split()
          a <-  dataMapPlot %>% group_by(subRegion, piece) %>% group_split()
          #z <- z %>% addPolygons(data=base, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
          if (length(a) >= 2){
            for (i in 1:length(a)){#group =unique(a[[i]]$subRegion),
              pal <- colorNumeric(
                palette = c("green", "red"),
                domain = 1:length(a))
            z <- z %>% addPolygons(data=a[[i]],  label = unique(a[[i]]$subRegion), group=~unique(subRegionType),lat=~lat, lng=~long, fillColor = ~pal(i), stroke = TRUE, weight = 0.5)
            #base <- base %>% add_row(lat=NA, long=NA) %>% bind_rows(d)
              }
            }
        }
    }
    print("]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]")
    z <- z%>%
        addLayersControl(
          overlayGroups = unique(subRegTypelist),
          options = layersControlOptions(collapsed = FALSE)
        )
    #z<-leaflet() %>% addTiles() %>% addPolygons(data=base, layerId = ~group, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    return(z)
  })


  output$mymap <- renderLeaflet({
    dataMap_raw <- data() %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion")

    dataMap_raw <- dataMap_raw %>%  dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)

    plist <- list()
    pcount = 1
    subRegTypelist <- c()
    z <- leaflet(height = "100vh") %>% addTiles()
    print("===============================================")
    for(i in unique(dataMap_raw$param)[!is.na( unique(dataMap_raw$param))]){
      dataMap_raw_regions <- dataMap_raw %>%
        dplyr::filter(subRegion!="South_Pacific_Islands")%>%
        dplyr::filter(param == i) %>%
        dplyr::select(subRegion) %>%
        unique(); dataMap_raw_regions

      dataMapPlot <- argus::mapdfFind(dataMap_raw_regions)%>%
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
          print(dataMapPlot)
          subRegTypelist[pcount] <- unique(dataMapPlot$subRegionType)
          pcount = pcount+1
          #a <-  dataMapPlot %>% group_by(subRegion) %>% group_split()
          dict <- argus::mappings("mappingGCAMBasins")
          dict$subRegion <- argus::mappings("mappingGCAMBasins")$subRegionMap
          dict$subRegionMap <- argus::mappings("mappingGCAMBasins")$subRegion
          dataMapPlot <- dataMapPlot %>% dplyr::left_join(dict,by="subRegion") %>%  dplyr::mutate(subRegionMap=case_when(is.na(subRegionMap)~subRegion, TRUE~subRegionMap))
          a <-  dataMapPlot %>% group_by(subRegion, piece) %>% group_split()
          #z <- z %>% addPolygons(data=base, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
             # palx <- colorFactor(
             #   palette = c("green", "red"),
             #   dataMapPlot$subRegionType)
          if (length(a) >= 2){
            for (i in 1:length(a)){#group =unique(a[[i]]$subRegion)
              pal <- colorNumeric(
                palette = c("green", "red"),
                domain = 1:length(a))
              z <- z %>% addPolygons(data=a[[i]],  group=~unique(subRegionMap), label = ~unique(subRegionMap), lat=~lat, lng=~long, fillColor = ~pal(i), stroke = FALSE)
            #base <- base %>% add_row(lat=NA, long=NA) %>% bind_rows(d)
            }
            for (i in 1:(length(a))){
              tem<-a[[i]] %>% dplyr::mutate(subRegionA =paste(subRegionMap, sep="", piece))
              z <- z %>% addPolygons(data=tem, group= ~unique(subRegionType), layerId = ~unique(subRegionA), label = ~unique(subRegionMap), lat=~lat, lng=~long, color = "#4287f5", stroke=TRUE, weight = 0.5)
            }
            }

        }
    }
    print("]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]")
    z <- z%>%
       addLayersControl(
         overlayGroups = unique(subRegTypelist),
         options = layersControlOptions(collapsed = FALSE)
       )
      session$sendCustomMessage("rhm_clic", unique(data()$subRegion))
    #z<-leaflet() %>% addTiles() %>% addPolygons(data=base, layerId = ~group, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    # updatePickerInput(
    #   inputId = "regionsSelected",
    #   session=session,
    #   choices = unique(data()$subRegion),
    #   selected = unique(reactiveValuesToList(input)$regionsSelected)
    # )
    check()
    return(z)
  })

  check <- function(){
    print("oof")
    hidden_group <- data()$subRegion[which(!data()$subRegion %in% reactiveValuesToList(input)$regionsSelected)]
    for (i in unique(hidden_group)){
      print(paste("hiding", " ", i))
      leafletProxy("mymap") %>% hideGroup(i)
    }
    for (i in unique(reactiveValuesToList(input)$regionsSelected)){
      print(paste("showing", " ", i))
      leafletProxy("mymap") %>% showGroup(i)
    }
    return(0)
  }

  observeEvent(input$regionsSelected, {
    check()
  }, ignoreNULL = FALSE)

  observeEvent(input$mymap_shape_click,{
    print(input$mymap_shape_click)
    if (is.null(input$mymap_shape_click$id)){
      return(0)
    }
    l = strsplit(input$mymap_shape_click$id, "[[:digit:]]")[[1]][[1]]
    selectedx <- reactiveValuesToList(input)$regionsSelected
    print(selectedx)
    print(input$regionsSelected)
    if (l %in% selectedx){
      print("deselecting")
      leafletProxy("mymap") %>% hideGroup(l)
      selectedx =  selectedx[!(selectedx %in% l)]
    }else{
      print("selecting")
      leafletProxy("mymap") %>% showGroup(l)
      leafletProxy("mymap") %>% hideGroup(input$mymap_shape_click$group)
      leafletProxy("mymap") %>% showGroup(input$mymap_shape_click$group)
      selectedx =  append(selectedx, l)
    }
    # session$sendCustomMessage("rhm_clic", selectedx)
    updatePickerInput(
     inputId = "regionsSelected",
     session=session,
     choices = unique(data()$subRegion),
     selected = unique(selectedx)
     )
  })

  #---------------------------
  # Subset Regions Selected
  #---------------------------
  output$subsetRegions = renderUI({
    pickerInput(
      inputId = "subsetRegions",
      label = "Select Regions to Compare",
      choices = unique(dataMapx()$subRegion),
      selected = unique(dataMapx()$subRegion)[1:4],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      ))
  })


  #---------------------------
  # Select Years for Map
  #---------------------------
  output$selectMapYear = renderUI({
    sliderInput("mapYear", label = h3("Select Year"), min = min(dataMapx()$x),
                max = max(dataMapx()$x), step = 5,
                value=sort(unique(dataMapx()$x))[round(length(sort(unique(dataMapx()$x)))/2)], sep="",
                animate =F)
  })

  #---------------------------
  # Reactive Regions Select based on inputs
  #---------------------------
  subsetRegionsx <- reactive({
    if (input$subsetRegions == "All" && length(input$subsetRegions) == 1) {
      return(unique(regionsSelectedx()))
    } else if (is.null(input$subsetRegions)){
      return(unique(regionsSelectedx())[1:4])
    } else{
      return(input$subsetRegions)
    }
  })


  #---------------------------
  # Reactive Reference Scenario Select
  #---------------------------

  scenarioRefSelectedx <- reactive({
    input$scenarioRefSelected
  })


  #---------------------------
  # Reactive Regions Select based on inputs
  #---------------------------
  regionsSelectedx <- reactive({
    if (input$regionsSelected == "All" && length(input$regionsSelected) == 1) {
      return(unique(data()$subRegion))
    } else {
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
                                            argus::constants()$chosenMix]
      if (length(paramsCheck) >= 1) {
        paramsCheck
      } else{
        unique(data()$param)
      }
    }else{
      input$paramsSelected
    }
  })

  #---------------------------
  # Reactive Scenarios based on inputs
  #---------------------------
  scenariosSelectedx <- reactive({
    if (input$scenariosSelected == "All" && length(input$scenariosSelected) > 0) {
      return(unique(dataSum()$scenario))
    } else{
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
    # print(unique(scenariosSelectedx()))
    # print(paramsSelectedx())
    x <- dataSum() %>%
      dplyr::filter(scenario %in% scenariosSelectedx(),
                    param %in% paramsSelectedx())
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
        print(scenRef_i)
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataChartx() %>%
      dplyr::filter(scenario == scenRef_i)
    for (k in unique(dataChartx()$scenario)[unique(dataChartx()$scenario) !=
                                            scenRef_i]) {
      tbl_temp <- dataChartx() %>%
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
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    # print(tbl_pd)
    tbl_pd
  })

  #---------------------------
  # Data Chart Absolute Diff
  #---------------------------
  dataPrcntAbsx <- reactive({
    diffText <- " Prcent Abs"

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
        print(scenRef_i)
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataChartx() %>%
      dplyr::filter(scenario == scenRef_i)
    for (k in unique(dataChartx()$scenario)[unique(dataChartx()$scenario) !=
                                            scenRef_i]) {
      print(k)
      tbl_temp <- dataChartx() %>%
        dplyr::filter(scenario %in% c(scenRef_i, k))
      tbl_temp <- tbl_temp %>%
        tidyr::spread(scenario, value)

      tbl_temp[is.na(tbl_temp)] <- 0

      #Important Code

      tbl_temp <- tbl_temp %>%
        dplyr::mutate(!!paste(k, diffText, sep = "") := 100*((get(k)/get(scenRef_i))-1)) %>%
        dplyr::select(-dplyr::one_of(c(k, scenRef_i)))
      tbl_temp <- tbl_temp %>%
        tidyr::gather(key = scenario, value = value, -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k, diffText, sep = "")]))
      tbl_pd <- dplyr::bind_rows(tbl_pd, tbl_temp)
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    print(dplyr::filter(tbl_pd, scenario %in% c(paste(k, diffText, sep = ""))))
    tbl_pd
  })



  #---------------------------
  # Data Map Absolute Diff
  #---------------------------
  dataDiffAbsMapx <- reactive({
    diffText <- " Diff Abs"

    if (is.null(input$scenarioRefSelected)) {
      print(paste("No reference scenario provided", sep = ""))
      print(paste(
        "Using ",
        unique(dataMapx()$scenario)[1],
        " as reference",
        sep = ""
      ))
      scenRef_i = unique(dataMapx()$scenario)[1]
    } else{
      if (!input$scenarioRefSelected %in% unique(dataMapx()$scenario)) {
        print(paste(
          "scenario ",
          input$scenarioRefSelected,
          " not in scenarios",
          sep = ""
        ))
        print(paste(
          "Using ",
          unique(dataMapx()$scenario)[1],
          " as reference",
          sep = ""
        ))
        scenRef_i = unique(dataMapx()$scenario)[1]
      } else{
        scenRef_i <- input$scenarioRefSelected
        print(scenRef_i)
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataMapx() %>%
      dplyr::filter(scenario == scenRef_i)
    for (k in unique(dataMapx()$scenario)[unique(dataMapx()$scenario) !=
                                            scenRef_i]) {
      tbl_temp <- dataMapx() %>%
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
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    # print(tbl_pd)
    tbl_pd
  })

  #---------------------------
  # Data Map Absolute Diff
  #---------------------------
  dataPrcntAbsMapx <- reactive({
    diffText <- " Prcent Abs"

    if (is.null(input$scenarioRefSelected)) {
      print(paste("No reference scenario provided", sep = ""))
      print(paste(
        "Using ",
        unique(dataMapx()$scenario)[1],
        " as reference",
        sep = ""
      ))
      scenRef_i = unique(dataMapx()$scenario)[1]
    } else{
      if (!input$scenarioRefSelected %in% unique(dataMapx()$scenario)) {
        print(paste(
          "scenario ",
          input$scenarioRefSelected,
          " not in scenarios",
          sep = ""
        ))
        print(paste(
          "Using ",
          unique(dataMapx()$scenario)[1],
          " as reference",
          sep = ""
        ))
        scenRef_i = unique(dataMapx()$scenario)[1]
      } else{
        scenRef_i <- input$scenarioRefSelected
        print(scenRef_i)
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataMapx() %>%
      dplyr::filter(scenario == scenRef_i)
    for (k in unique(dataMapx()$scenario)[unique(dataMapx()$scenario) !=
                                            scenRef_i]) {
      print(k)
      tbl_temp <- dataMapx() %>%
        dplyr::filter(scenario %in% c(scenRef_i, k))
      tbl_temp <- tbl_temp %>%
        tidyr::spread(scenario, value)

      tbl_temp[is.na(tbl_temp)] <- 0

      #Important Code

      tbl_temp <- tbl_temp %>%
        dplyr::mutate(!!paste(k, diffText, sep = "") := 100*((get(k)/get(scenRef_i))-1)) %>%
        dplyr::select(-dplyr::one_of(c(k, scenRef_i)))
      tbl_temp <- tbl_temp %>%
        tidyr::gather(key = scenario, value = value, -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k, diffText, sep = "")]))
      tbl_pd <- dplyr::bind_rows(tbl_pd, tbl_temp)
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    print(dplyr::filter(tbl_pd, scenario %in% c(paste(k, diffText, sep = ""))))
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
  },
  width=function(){
    if (length(unique(dataChartx()$param))==1){
      return(300)
    }else if (length(unique(dataChartx()$param))==2){
      return(500)
    }else{
      return("auto")
    }
    }
  )
  output$downloadPlotSum <- downloadHandler(
    filename = "summaryChart.png",
    content = function(file) {
      ggsave(
        file,
        plot=summaryPlot(0.75, 10, 10),
        #max(13,min(13,1.25*length(unique(dataChartx()$param)))),
        height = argus::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
        width=argus::exportWidth(10, length(unique(dataChartx()$param)), 3),
        units="in"
      )
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
  height=function(){200*length(unique(dataMapx()$param))},
  width=function(){max(400,200*length(subsetRegionsx())+100)}
  )

  output$downloadPlotSumReg <- downloadHandler(
    filename = "summaryChartReg.png",
    content = function(file) {
      ggsave(file,plot=summaryPlotReg(10),
             height = argus::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = argus::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in")
    })


  #---------------------------
  # Pick between Absolute and Percent Diff
  #---------------------------

  observeEvent(input$absChart, {
    rv$absChart = 1;
    rv$percDiffChart = 0;
    rv$absDiffChart = 0;
  })

  observeEvent(input$percDiffChart, {
    rv$absChart = 0;
    rv$percDiffChart = 1;
    rv$absDiffChart = 0;
  })

  observeEvent(input$absDiffChart, {
    rv$absChart = 0;
    rv$percDiffChart = 0;
    rv$absDiffChart = 1;
  })

  #---------------------------
  # Chart Plot
  #---------------------------
  chartPlot <- function(){
    print(rv)
    print(rv$absChart)
    print(rv$percDiffChart)
    print(rv$absDiffChart)
    g <- 2
    if(rv$absChart == 1){
      print("abs")
      g <- 1
      dataChartPlot <- dataChartx()
    }else if(rv$percDiffChart == 1){
      print("perc diff")
      dataChartPlot <- dataPrcntAbsx()
    }else if(rv$absDiffChart == 1){
      print("abs diff")
      dataChartPlot <- dataDiffAbsx()
    }

    plist <- list()
    x = 1
    for(i in 1:length(unique(dataChartPlot$param))){
      # Check Color Palettes
      palAdd <- c("firebrick3","dodgerblue3","forestgreen","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise")

      missNames <- unique(dataChartPlot$class)[!unique(dataChartPlot$class) %in%
                                                 names(pal_all)]
      if (length(missNames) > 0) {
        palAdd <- palAdd[1:length(missNames)]
        names(palAdd) <- missNames
        palCharts <- c(pal_all, palAdd)
      } else{
        palCharts <- pal_all
      }
      print(palCharts)

      chartz <- dataChartPlot %>%
        filter(param==unique(dataChartPlot$param)[i], scenario == input$scenarioRefSelected)
      z<-x
      if(rv$percDiffChart == 1){
        plist[[z+1]] <-  ggplot2::ggplot(dataChartPlot %>%
                                           filter(param==unique(dataChartPlot$param)[i], scenario != input$scenarioRefSelected)%>%
                                           droplevels(),
                                         aes(x=x,y=value,
                                             # group=class,
                                             colour=class
                                         )) +
          ggplottheme +
          ylab(NULL) + xlab(NULL) +
          scale_color_manual(breaks=names(palCharts),values=palCharts) +
          # scale_y_continuous(position = "right")+
          # geom_bar(position="stack", stat="identity") +
          geom_line()+
          geom_point()+
          scale_color_manual(breaks=names(palCharts),values=palCharts) +
          facet_grid(param~scenario, scales="free",switch="y")+
          theme(legend.position="bottom",
                strip.text.y = element_blank(),
                legend.title = element_blank(),
                legend.margin=margin(0,0,0,0,"pt"),
                legend.key.height=unit(0, "cm"),
                text = element_text(size = 12.5),
                plot.margin=margin(20,20,20,0,"pt"))
        x = x+2
      }else if(rv$absDiffChart == 1){
        plist[[z+1]] <-  ggplot2::ggplot(dataChartPlot %>%
                                           filter(param==unique(dataChartPlot$param)[i], scenario != input$scenarioRefSelected)%>%
                                           droplevels(),
                                         aes(x=x,y=value,
                                             group=scenario,
                                             fill=class))+
          ggplottheme +
          xlab(NULL) +
          ylab(NULL) +
          scale_fill_manual(breaks=names(palCharts),values=palCharts) +
          scale_y_continuous(position = "left")+
          geom_bar(position="stack", stat="identity") +
          # geom_line()+
          # geom_point()+
          facet_grid(param~scenario, scales="free",switch="y") +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                strip.text.y = element_blank(),
                legend.margin=margin(0,0,0,0,"pt"),
                legend.key.height=unit(0, "cm"),
                text = element_text(size = 12.5),
                plot.margin=margin(20,20,20,0,"pt"))
        x = x+2
      }else{
        chartz <- dataChartPlot %>%
          filter(param==unique(dataChartPlot$param)[i])
        x=x+1
      }

      plist[[z]] <-  ggplot2::ggplot(chartz%>%
                                       droplevels(),
                                      aes(x=x,y=value,
                                          group=scenario,
                                          fill=class))+
        ggplottheme +
        xlab(NULL) +
        ylab(unique(dataChartPlot$param)[i])+
        scale_fill_manual(breaks=names(palCharts),values=palCharts) +
        scale_y_continuous(position = "left")+
        geom_bar(position="stack", stat="identity") +
        facet_grid(param~scenario, scales="free",switch="y")+
        theme(legend.position="bottom",
              strip.text.y = element_blank(),
              legend.title = element_blank(),
              legend.margin=margin(0,0,0,0,"pt"),
              legend.key.height=unit(0, "cm"),
              text = element_text(size = 12.5),
              plot.margin=margin(20,0,20,0,"pt"))
    }
    cowplot::plot_grid(plotlist = plist, ncol=g, align="v", rel_widths = c(1, length(unique(dataChartPlot$scenario))-1))
  }

  output$plot <- renderPlot({
    chartPlot()
  },
  height=function(){300*length(unique(dataChartx()$param))},
  width=function(){max(600, 400*length(unique(data()$scenario)))}
  )

  output$downloadPlotChart <- downloadHandler(
    filename = "barChart.png",
    content = function(file) {
      ggsave(file,plot=chartPlot(),
             width=argus::exportWidth(49, length(unique(dataChartx()$param)), 5),
             height=argus::exportHeight(1, 49, length(unique(dataChartx()$param)), 5)+2,
             unit = "in"
      )
      # exportHeight<-function(chartsperrow, max_height_in, numelement, lenperchart){
      # max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
    })

  #---------------------------
  # Maps
  #---------------------------

  #---------------------------
  # Map Analysis by Base Map
  #---------------------------

  output$mapBase <- renderPlot({

    dataMap_raw <- dataMapx() %>% dplyr::ungroup() %>%
      dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
      dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion)) %>%
      dplyr::select(-subRegionMap)

    plist <- list()
    pcount = 1
    subRegTypelist <- c()
    for(i in unique(dataMap_raw$param)[!is.na( unique(dataMap_raw$param))]){

      dataMap_raw_regions <- dataMap_raw %>%
        dplyr::filter(subRegion!="South_Pacific_Islands")%>%
        dplyr::filter(param == i) %>%
        dplyr::select(subRegion) %>%
        unique(); dataMap_raw_regions

      dataMapPlot <- argus::mapdfFind(dataMap_raw_regions)%>%
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
        map <- map + geom_text(data = cnames, aes(x = long, y = lat, label = subRegion),color="black", size = 4)
        map <- map + theme(panel.background = element_rect(fill="lightblue1")) + ggtitle(unique(dataMapPlot$subRegionType))
        map

        plist[[pcount]] <- map
        pcount=pcount+1
        rv$pcount <- pcount
      }
    }
    rv$pcount <- pcount
    print(rv$pcount)
    return(cowplot::plot_grid(plotlist=plist,ncol=1,align = "v"))

  },
  height=function(){300*(rv$pcount)}
  )

  #---------------------------
  # Pick between Absolute and Percent Diff
  #---------------------------

  observeEvent(input$absMap, {
    rv$absMap = 1;
    rv$percDiffMap = 0;
    rv$absDiffMap = 0;
  })

  observeEvent(input$percDiffMap, {
    rv$absMap = 0;
    rv$percDiffMap = 1;
    rv$absDiffMap = 0;
  })

  observeEvent(input$absDiffMap, {
    rv$absMap = 0;
    rv$percDiffMap = 0;
    rv$absDiffMap = 1;
  })

  #---------------------------
  # Map Analysis by Scenario x Param
  #---------------------------


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


  process_map <- function(dataMap_raw, i){
    US52Compact=F
    naColor = "green"
    breaks_n = 6
    legendType = input$mapLegend
    palAbsChosen <- c("yellow2","goldenrod","darkred")
    yearsSelect <- input$mapYear
    paramsSelect <- unique(dataMap_raw$param)

    dataMap_raw_param <- dataMap_raw %>%
      dplyr::filter(x==yearsSelect,
                    param == i); dataMap_raw_param

    breaks<- breaks(dataMap_raw_param, breaks_n)

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


    data_map%>%head()

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
                    flip = case_when(minLong<-160 & maxLong>160 ~ 1,
                                     TRUE~0),
                    long = case_when((abs(posLongSum) > abs(negLongSum)) & (long < 0) & flip ==1 ~ long+360,
                                     (abs(posLongSum) < abs(negLongSum)) & (long > 0) & flip ==1 ~ long-360,
                                     TRUE~long))%>%
      dplyr::ungroup(); dataMapPlot %>% head()
    # data_map%>%head()
    return(list(shp_df, dataMapPlot, paletteAbs, paletteDiff))
    }


  output$map <- renderPlot({
    gas <- 2
    if (rv$absDiffMap == 1){
      dataMap_raw <- dataDiffAbsMapx() %>% dplyr::ungroup() %>%
        dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
        dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                          TRUE~subRegion)) %>%
        dplyr::select(-subRegionMap)
    }else if (rv$percDiffMap == 1){
      dataMap_raw <- dataPrcntAbsMapx() %>% dplyr::ungroup() %>%
        dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
        dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                          TRUE~subRegion)) %>%
        dplyr::select(-subRegionMap)
    }else{
      dataMap_raw <- dataMapx() %>% dplyr::ungroup() %>%
        dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
        dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                          TRUE~subRegion)) %>%
        dplyr::select(-subRegionMap)
      gas<-1
    }

    # print("dataMap_raw")
    # print(dataMap_raw)

    # Map Settings
    US52Compact=F
    naColor = "green"
    breaks_n = 6
    legendType = input$mapLegend
    palAbsChosen <- c("yellow2","goldenrod","darkred")
    yearsSelect <- input$mapYear
    paramsSelect <- unique(dataMap_raw$param)

    #Partitions Value into breaks of ... 6?
    z <- 1
    plist <- list()
    for(i in paramsSelect[!is.na(paramsSelect)]){
      print(i)
      print("++++++++++++++++=")
      if ((rv$absDiffMap == 1)||(rv$percDiffMap == 1)){
        proc <- process_map(dataMapx() %>% dplyr::ungroup() %>%
                              dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
                              dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                                                TRUE~subRegion)) %>%
                              dplyr::select(-subRegionMap), i)
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
          map <- map + geom_polygon(data = dataMapPlot %>% dplyr::filter(scenario == input$scenarioRefSelected),
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
            theme(legend.position="bottom",
                  legend.title = element_blank(),
                  strip.text.y = element_blank(),
                  plot.margin=margin(0,-30,0,0,"pt"),
                  axis.title=element_text(10),
                  axis.text=element_blank(),
                  axis.ticks=element_blank())
          if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
        }; map

        plist[[z]] <- map
        z = z+1


        proc <- process_map(dataMap_raw, i)
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
          map <- map + geom_polygon(data = dataMapPlot %>% dplyr::filter(scenario != input$scenarioRefSelected),
                                    aes(x = long, y = lat, group = group, fill = as.factor(brks)),
                                    colour = "gray10", lwd=0.5) +
            scale_fill_manual(values=paletteAbs, na.value  = naColor, drop=FALSE) + theme_bw() +
            xlab(NULL) +
            ylab("hello") +
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
                  plot.margin=margin(0,0,0,-30,"pt"),
                  strip.text.y = element_blank(),
                  axis.title=element_blank(),
                  axis.text=element_blank(),
                  axis.ticks=element_blank())
          if(!US52Compact){map <- map + theme(panel.background = element_rect(fill="lightblue1"))}
        }; map
        plist[[z]] <- map
        z = z+1
      }else {
        proc <- process_map(dataMapx() %>% dplyr::ungroup() %>%
                              dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
                              dplyr::mutate(subRegion=case_when(!is.na(subRegionMap)~subRegionMap,
                                                                TRUE~subRegion)) %>%
                              dplyr::select(-subRegionMap), i)
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

    cowplot::plot_grid(plotlist=plist,ncol=gas,align = "h", rel_widths = c(1, length(unique(data()$scenario))-1))
  },
  height=function(){300*length(unique(dataMapx()$param))},
  width=function(){max(600, 400*length(unique(data()$scenario)))}
  )




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
      fs <- c("table.csv",
              "summaryChart.png",
              "barCharts.png",
              "summaryChartReg.png"
              )
      write.csv(data(), "table.csv")
      ggsave("summaryChart.png", plot=summaryPlot(0.75, 10, 10),
             #max(13,min(13,1.25*length(unique(dataChartx()$param)))),
             height = argus::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
             width=argus::exportWidth(10, length(unique(dataChartx()$param)), 3),
             units="in"
      )
      ggsave("barCharts.png",plot=chartPlot(),
              width=argus::exportWidth(49, length(unique(dataChartx()$param)), 5),
              height=argus::exportHeight(1, 49, length(unique(dataChartx()$param)), 5)+2,
              unit = "in"
             )
      ggsave("summaryChartReg.png", plot=summaryPlotReg(10),
             height = argus::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = argus::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in"
             )
      print(fs)
      zip::zip(zipfile=file, files=fs)
    }
  )
}
