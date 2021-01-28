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
library(plyr)

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

  # Create your own reactive values that you can modify because input is read only
  rv <- reactiveValues()

  #initializing abs, percDiff, and absDiff

  rv$abs = 1;
  rv$percDiff = 0;
  rv$absDiff = 0;


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
      return(rdataviz::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
      rv$filedatax <- NULL
      rv$urlfiledatax <- NULL
    } else if(!is.null(rv$filedatax) & is.null(dataGCAMx()) & ("" == rv$urlfiledatax)) {
      return(rdataviz::addMissing(
        rdataviz::parse_local(input)%>%
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
      return(rdataviz::addMissing(
        rdataviz::parse_remote(input)%>%
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
                                            rdataviz::constants()$chosenMix]
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

  observeEvent(input$test, {
    PrcntChartPlot()
    print(input)
  })

  observeEvent(input$abs, {
    rv$abs = 1;
    rv$percDiff = 0;
    rv$absDiff = 0;
  })

  observeEvent(input$percDiff, {
    rv$abs = 0;
    rv$percDiff = 1;
    rv$absDiff = 0;
  })

  observeEvent(input$absDiff, {
    rv$abs = 0;
    rv$percDiff = 0;
    rv$absDiff = 1;
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
        height = rdataviz::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
        width=rdataviz::exportWidth(10, length(unique(dataChartx()$param)), 3),
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
             height = rdataviz::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = rdataviz::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in")
    })


  #---------------------------
  # Chart Plot
  #---------------------------
  chartPlot <- function(){
    print(rv)
    print(rv$abs)
    print(rv$percDiff)
    print(rv$absDiff)
    g <- 2
    if(rv$abs == 1){
      print("abs")
      g <- 1
      dataChartPlot <- dataChartx()
    }else if(rv$percDiff == 1){
      print("perc diff")
      dataChartPlot <- dataPrcntAbsx()
    }else if(rv$absDiff == 1){
      print("abs diff")
      dataChartPlot <- dataDiffAbsx()
    }

    plist <- list()
    x = 1
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
      print(palCharts)

      chartz <- dataChartPlot %>%
        filter(param==unique(dataChartPlot$param)[i], scenario == input$scenarioRefSelected)
      z<-x
      if(rv$percDiff == 1){
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
      }else if(rv$absDiff == 1){
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
             height = rdataviz::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
             width=rdataviz::exportWidth(10, length(unique(dataChartx()$param)), 3),
             units="in"
      )
      ggsave("barCharts.png",plot=chartPlot(),
              width=rdataviz::exportWidth(49, length(unique(dataChartx()$param)), 5),
              height=rdataviz::exportHeight(1, 49, length(unique(dataChartx()$param)), 5)+2,
              unit = "in"
             )
      ggsave("summaryChartReg.png", plot=summaryPlotReg(10),
             height = rdataviz::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = rdataviz::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in"
             )
      print(fs)
      zip::zip(zipfile=file, files=fs)
    }
  )
}
