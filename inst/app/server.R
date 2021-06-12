#' server

#...........................
# Libraries Needed
#...........................

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
library(mvbutils)
library(RColorBrewer)
library(grDevices)
library(rmap)
library(plotly)

#...........................
# Options
#...........................

options(shiny.maxRequestSize=100*1024^2)
#options(shiny.trace = TRUE)
pal_all <- argus::mappings()$pal_all

#...........................
# Server object
#...........................

server <- function(input, output, session) {

  # NOTE:
  # To collapse code for easy reading place cursor here and enter: ALT+0
  # To Expand code again place cursor here and enter: ALT+SHIFT+O (O not 0)

  #...........................
  # Bookmark
  #...........................
  if(T){

    #...........................
    # Bookmark modal
    #...........................

    #Bookmark modal
    observeEvent(input$loadbookmark, {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          fileInput(
            inputId = "readbookmark",
            label = "Upload rds",
            accept = c(".rds"),
            multiple = TRUE,
            width = "100%"
          ),
          fluidRow(
            column(6,
                   div(
                     downloadButton(
                       'bookmark',
                       "RDS",
                       class = "download_button"
                     ),
                     style = "float:right;width=100%"
                   ))
            ,
            column(6,
                   div(

                     #bookmarkButton(),
                     actionLink(inputId="._bookmark_",
                                label="URL",
                                class = "btn btn-default shiny-download-link download_button",
                                icon = icon("link","fa-1x")
                     ),
                     style = "float:left;width=100%"
                   )
            )
          )
        )
      )
    })

    #...........................
    # URL Bookmark
    #...........................
    enableBookmarking(store = "server")
    setBookmarkExclude(c("urlfiledata","filedata","filedata","append", "close", "readfilebutton", "readurlbutton", "readgcambutton", "inputz"))

    #URL bookmark onbookmark
    onBookmark(function(state) {
      state$values$data <- rv$data
    })

    #URL bookmark onRestore
    onRestore(function(state) {
      print(state)
      rv$data <- state$values$data
      updatePickerInput(
        inputId = "mapLegend",
        session=session,
        selected = state$input$mapLegend
      )
    })

    #...........................
    # RDS Bookmark
    #...........................

    #rds bookmark download handler
    output$bookmark <- downloadHandler(
      filename <- "bookmark.rds",
      content = function(file){
        state <- isolate(reactiveValuesToList(input))
        state$data <- rv$data
        saveRDS(state, file)
      }
    )

    #rds bookmark upload handler
    observeEvent(input$readbookmark, {
      removeModal()
      state <- readRDS(input$readbookmark$datapath)
      rv$data <- state$data

    #focusMapScenarioSelected
    settingfocusMapScenarioSelected <- state$focusMapScenarioSelected
    if(( settingfocusMapScenarioSelected %in% unique(data()$scenario)) && !is.null(settingfocusMapScenarioSelected)){
      updatePickerInput(
        inputId = "focusMapScenarioSelected",
        session=session,
        selected = settingfocusMapScenarioSelected
      )
      session$sendCustomMessage("setsetting", c("focusMapScenarioSelected", settingfocusMapScenarioSelected))
    }

    #focusMapScenarioSelected
    settingfocusMapYearSelected <- state$focusMapYearSelected
    if((settingfocusMapYearSelected %in% dataMapx()$x) && !is.null(settingfocusMapScenarioSelected)){
      updatePickerInput(
        inputId = "focusMapYearSelected",
        session=session,
        selected = settingfocusMapYearSelected
      )
      session$sendCustomMessage("setsetting", c("focusMapYearSelected", settingfocusMapYearSelected))
    }

    #focusMapScenarioSelected
    settingfocusMapParamSelected  <- state$focusMapParamSelected
    if(( settingfocusMapParamSelected %in% unique(dataMapx()$param)) && !is.null(settingfocusMapParamSelected)){
      updatePickerInput(
        inputId = "focusMapParamSelected",
        session=session,
        selected = settingfocusMapParamSelected
      )
      session$sendCustomMessage("setsetting", c("focusMapParamSelected", settingfocusMapParamSelected))
    }


      #mapLegend
      settingsmapLegend <- state$mapLegend
      if((settingsmapLegend %in% c("kmean","pretty")) && !is.null(settingsmapLegend)){
        updatePickerInput(
          inputId = "mapLegend",
          session=session,
          selected = settingsmapLegend
        )
        session$sendCustomMessage("setsetting", c("mapLegend", settingsmapLegend))
      }

      #mapYear
      settingsmapYear <- state$mapYear
      if(!is.null(settingsmapYear) && (settingsmapYear %in% dataMapx()$x)){
        updateSliderInput(
          inputId = "mapYear",
          session=session,
          min = min(dataMapx()$x),
          max = max(dataMapx()$x),
          value=settingsmapYear
        )
        session$sendCustomMessage("setsetting", c("mapYear", settingsmapYear))
      }

      #subsetRegions
      settingsSubsetRegions <- state$subsetRegions
      if(any(unique(settingsSubsetRegions) %in% unique(data()$subRegion))){
        print("c==c")
        updatePickerInput(
          inputId = "subsetRegions",
          session=session,
          choices = unique(data()$subRegion),
          selected = state$subsetRegions)
        session$sendCustomMessage("setsetting", c("subsetRegions", settingsSubsetRegions))
        print(state$subsetRegions)
      }

      # Regions Update
      settingsRegions <- state$regionsSelect
      if(any(unique(settingsRegions) %in% unique(data()$subRegion))){
        updatePickerInput(
          session=session,
          inputId = "regionsSelected",
          selected = unique(settingsRegions)[unique(settingsRegions) %in% unique(data()$subRegion)],
        )
      }

      # Parameters Update
      settingsParams <- state$paramsSelect
      if(any(unique(settingsParams) %in% unique(data()$param))){
        updatePickerInput(
          session=session,
          inputId = "paramsSelected",
          selected = unique(settingsParams)[unique(settingsParams) %in% unique(data()$param)],
        )
      }

      # Scenario Update
      settingsScenario <- state$scenariosSelect
      if(any(unique(settingsScenario) %in% unique(data()$scenario))){
        updatePickerInput(
          session=session,
          inputId = "scenariosSelected",
          selected = unique(settingsScenario)[unique(settingsScenario) %in% unique(data()$scenario)],
        )
      }

      # Reference Scenario Update
      settingsRefScenario <- state$scenarioRefSelect
      if(any(unique(settingsRefScenario) %in% unique(data()$scenario))){
        updatePickerInput(
          session=session,
          inputId = "scenarioRefSelected",
          selected = unique(settingsRefScenario)[unique(settingsRefScenario) %in% unique(data()$scenario)],
        )
      }
    })



    } # Bookmark


  #...........................
  # Initial Setting
  #...........................

  if(T){ # Initial Settings

  # Initialize Settings
  settings <- reactive({data.frame()})
  settingsVars <- c("urlSelect",
                    "regionsSelect",
                    "scenariosSelect",
                    "scenarioRefSelect",
                    "paramsSelect")

  # Create Modal for Settings Download and Loading
  observeEvent(input$inputz, {
    if (input$inputz == "csv"){
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
          br(),
          # CSV Data ....................................-
          fileInput(
            inputId = "filedata",
            label = "Upload csv or zip file",
            accept = c(".csv", ".zip"),
            multiple = TRUE,
            width = "100%"
          ),
          br(),
          actionButton(inputId = "readfilebutton",
                       label = "Read File Data")
      ))
    }else if(input$inputz == "url"){
     showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
          br(),
          textInput(
            inputId = "urlfiledata",
            label = "Enter url to csv or zip file, seperated by ',' between url",
            placeholder =  "https://raw.githubusercontent.com/JGCRI/argus/main/inst/extdata/exampleData.csv"),
          br(),
          width = "100%",
          br(),
          actionButton(inputId = "readurlbutton",
                       label = "Read Url Data")
    ))
    }else if (input$inputz == "gcam"){
     showModal(
      modalDialog(
        size = "m",
        easyClose = TRUE,
        footer = NULL,
          br(),
          p("*Note: Only for Argus run on local computer.", style="color:#cc0000"),
          tabsetPanel(
            type = "tabs",
            id="gcamtabs",
            tabPanel(
              "gcamdatabase",
              br(),
              shinyDirButton(id = "gcamdir",
                             label = "Choose GCAM directory",
                             title = "Select"),
              br(),
              textInput(
                inputId = "gcamdirfilepath",
                label = NULL,
                placeholder =  "OR Enter path to GCAM directory"),
              br(),
              verbatimTextOutput("gcamdirtext", placeholder = FALSE),
              br(),
              uiOutput('gcamScenarios'),
            ),
            tabPanel(
              ".PROJ",
              br(),
              shinyFilesButton(id = "proj",
                             label = "Choose GCAM .proj file",
                             title = "Select",
                             multiple=F),
              br(),
              textInput(
                inputId = "gcamprojfilepath",
                label = NULL,
                placeholder =  "OR Enter path to GCAM .proj file"),
              br(),
              verbatimTextOutput("gcamprojtext", placeholder = FALSE),
              br(),
              uiOutput('gcamScenariosProj'),
            )
          ),

          br(),
          uiOutput('gcamParams'),
          br(),
          uiOutput('gcamRegions'),
          br(),
          actionButton(inputId = "readgcambutton",
                       label = "Read GCAM Data"),
          br(),
          width = "100%"
        )
      )
      }
      updateSelectInput(session, "inputz", selected = "")
    })

  observeEvent(input$readurlbutton, {
    removeModal()
    #req(input$urlfiledata)
    showModal(
      modalDialog(
        size = "s",
        easyClose = FALSE,
        footer = NULL,
        fluidRow(
          column(6,
                 div(actionLink(
                   inputId='append',
                   label="Append to Input",
                   class = "btn btn-default shiny-download-link download_button"),
                   style = "float:center"
                 ))
          ,
          column(6,
                 div(actionLink(inputId="close",
                                label='Overwrite Input',
                                class = "btn btn-default shiny-download-link download_button"),
                     style="float:right;!important"
                 )
          )
        )
      ))
  })

  observeEvent(input$readfilebutton, {
      print("oof")
      removeModal()
      #req(input$filedata)
    showModal(
      modalDialog(
        size = "s",
        easyClose = FALSE,
        footer = NULL,
        fluidRow(
            column(6,
              div(actionLink(
                  inputId='append',
                  label="Append to Input",
                  class = "btn btn-default shiny-download-link download_button"),
                  style = "float:center"
                ))
        ,
        column(6,
                div(actionLink(inputId="close",
                                label='Overwrite Input',
                                class = "btn btn-default shiny-download-link download_button"),
                                style="float:right;!important"
                 )
          )
        )
    ))
    }, ignoreInit=TRUE)

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
                   style = "float:center;"
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

    #...........................
    # Update input File to Default (NULL)
    #...........................
    rv$filedatax <- NULL
    rv$selectedx <- NULL

    #...........................
    # Scenarios Select
    #...........................
   updatePickerInput(
        inputId = "scenariosSelected",
        session=session,
        choices = unique(data()$scenario),
        selected = unique(data()$scenario))

    #...........................
    # Ref Scenario Select
    #...........................
    updatePickerInput(
        inputId = "scenarioRefSelected",
        session=session,
        choices = unique(data()$scenario)[unique(data()$scenario)
                                             %in% scenariosSelectedx()],
        selected = (unique(data()$scenario)[unique(data()$scenario)
                                               %in% scenariosSelectedx()])[1])

    #...........................
    # Parameters Select
    #...........................
    updatePickerInput(
        inputId = "paramsSelected",
        session=session,
        choices = c("Chosen Mix", unique(data()$param)),
        selected = unique(data()$param)[1:5])

    #...........................
    # Regions Select
    #...........................
    updatePickerInput(
        inputId = "regionsSelected",
        session=session,
        choices = unique(data()$subRegion),
        selected = unique(data()$subRegion))

    #...........................
    # Subset Regions Selected
    #...........................
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

  # Toggle Sidebar
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")

    if (input$toggleSidebar %% 2 == 1) {
      icon <-  icon("caret-down","fa-1x")
    } else {
      icon <-  icon("caret-up","fa-1x")
    }
    updateActionButton(session,
                       "toggleSidebar",
                       icon = icon)

  })

  # Load Default Datasets from argus
  dataDefault <- argus::example_GCAMv5p3_SSP235
  ggplottheme <- ggplot2::theme_bw()

  } # Initial Setting

  #...........................
  # Reactive Values
  #...........................

  if(T){ # Reactive Values

  # Create your own reactive values that you can modify because input is read only
  rv <- reactiveValues()

  rv$validGcam <- FALSE
  rv$pcount = 1;
  rv$mapflag = 0;
  rv$subRegTypelist = c()
  rv$selectedBase = 0;
  rv$data <- dataDefault %>% dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
  # Charts initializing abs, percDiff, and absDiff
  rv$absChart = 1;
  rv$percDiffChart = 0;
  rv$absDiffChart = 0;

  # Maps initializing abs, percDiff, and absDiff
  rv$absMap = 1;
  rv$percDiffMap = 0;
  rv$absDiffMap = 0;

  } # Reactive values

  #...........................
  # INPUTS
  #...........................

  if(T){ # Read in Inputs

  #...........................
  # GCAM INPUTS
  #...........................

  if(T){ # GCAM Inputs

  roots <- getVolumes()()

  shinyDirChoose(
    input,
    id = 'gcamdir',
    roots = roots,
    filetypes = c('')
  )

  shinyFileChoose(
    input,
    id = 'proj',
    roots = roots,
    filetypes = c('', 'proj')
  )

  #......................................................
  # Creating a reactive environment to choose gcamdatabase
  # Based on folder input or text input
  #......................................................

  rv_gcam <- reactiveValues()
  rv_gcam$gcamdatabasepathx = ""
  rv_gcam$gcamprojpathx = ""

  observeEvent(input$gcamdir,{
    rv_gcam$gcamdatabasepathx <-gsub("\\\\","/",parseDirPath(roots, input$gcamdir))
    rv_gcam$gcamprojpathx <- ""
  })

  observeEvent(input$gcamdirfilepath,{
    rv_gcam$gcamdatabasepathx <-gsub("\\\\","/",input$gcamdirfilepath)
    rv_gcam$gcamprojpathx <- ""
  })

  observeEvent(input$proj,{
    rv_gcam$gcamprojpathx <-gsub("\\\\","/",(parseFilePaths(roots, input$proj))$datapath)
    rv_gcam$gcamdatabasepathx <- ""
  })

  observeEvent(input$gcamprojfilepath,{
    rv_gcam$gcamprojpathx <-gsub("\\\\","/",input$gcamprojfilepath)
    rv_gcam$gcamdatabasepathx <- ""
  })

  output$gcamdirtext <- renderText({
    if(rv_gcam$gcamdatabasepathx != "" & rv$validGCAM){
    paste0("Reading GCAM data from: ", rv_gcam$gcamdatabasepathx)}
    else{
      "Awaiting Valid Input"
    }
  })

  output$gcamprojtext <- renderText({
    if(rv_gcam$gcamprojpathx != "" & rv$validGCAM){
    paste0("Reading GCAM Proj File from: ", rv_gcam$gcamprojpathx)}
    else{
      return("Awaiting Valid Input")
    }
  })

  #.........................................
  # Get names of scenarios in GCAM database
  #.........................................

  gcamScenariosx <- reactive({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Extracting scenarios from GCAM database", value = 0)
      progress$inc(1/3, detail = paste("Parsing Path", 1))

      gcamdatabasePath_dir <- gsub("/$","",gsub("[^/]+$","",rv_gcam$gcamdatabasepathx)); gcamdatabasePath_dir
      gcamdatabasePath_file <- gsub('.*/ ?(\\w+)', '\\1', rv_gcam$gcamdatabasepathx); gcamdatabasePath_file
      # Save Message from rgcam::localDBConn to a text file and then extract names
      zz <- file(paste(getwd(),"/test.txt",sep=""), open = "wt")
      sink(zz,type="message")
      #progress$inc(1/3, detail = paste("Connecting to Database", 2))
      rgcam::localDBConn(gcamdatabasePath_dir,gcamdatabasePath_file)
      sink()
      closeAllConnections()
      # Read temp file
      #progress$inc(1/3, detail = paste("Reading temp file", 2))
      con <- file(paste(getwd(),"/test.txt",sep=""),open = "r")
      first_line <- readLines(con,n=1); first_line
      closeAllConnections()
      if(grepl("error",first_line,ignore.case = T)){stop(paste(first_line))}
      print(first_line)
      if(file.exists(paste(getwd(),"/test.txt",sep=""))){unlink(paste(getwd(),"/test.txt",sep=""))}
      # Extract scenario names from saved line
      #progress$inc(1/3, detail = paste("Extracting Scenario names from saved line", 2))
      s1 <- gsub(".*:","",first_line);s1
      s2 <- gsub(" ","",s1);s2
      b <- as.vector(unlist(strsplit(s2,",")))
      if (!is.null(b)){
        rv$validGCAM <- TRUE
      }else{
        rv$validGCAM <- FALSE
      }
      return(b)
  })

  gcamScenariosxProj <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Extracting scenarios from GCAM database", value = 0)
    progress$inc(1/2, detail = paste("Loading project...", 1))
    scens <- (names(rgcam::loadProject(rv_gcam$gcamprojpathx)))
    if (!is.null(scens)){
      rv$validGCAM <- TRUE
    }else{
      rv$validGCAM <- FALSE
    }
    progress$inc(1/2, detail = paste("Completed", 1))
    return(scens)
  })

  output$gcamScenarios = renderUI({
    pickerInput(
        inputId = "gcamScenariosSelected",
        label = "Select Available GCAM Database Scenarios",
        choices = unique(gcamScenariosx()),
        selected = unique(gcamScenariosx()),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "None",
          `select-all-text` = "All",
          `none-selected-text` = "None Selected"
        )
      )
  })

  output$gcamScenariosProj = renderUI({
    pickerInput(
      inputId = "gcamProjScenariosSelected",
      label = "Select Available GCAM Proj File Scenarios",
      choices = unique(gcamScenariosxProj()),
      selected = unique(gcamScenariosxProj()),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #.........................................
  # GCAM parameters
  #.........................................

  gcamParamsx <- reactive({
    unique((argus::mappings()$mapParamQuery)$param)
  })

  output$gcamParams = renderUI({
    pickerInput(
      inputId = "gcamParamsSelected",
      label = "Select Available GCAM Parameters",
      choices = c(gcamParamsx(),"All"),
      selected = c("pop","gdp"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #.........................................
  # GCAM Regions
  #.........................................

  gcamRegionsx <- reactive({
   unique((argus::mappings()$countryToGCAMReg32)$region)
  })

  output$gcamRegions = renderUI({
    pickerInput(
      inputId = "gcamRegionsSelected",
      label = "Select Available GCAM Regions",
      choices = c(gcamRegionsx(),"All"),
      selected = c(gcamRegionsx(),"All"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #...................................
  # Create data table from database

  #dataGCAMx <- eventReactive(input$readgcambutton, {
  observeEvent(input$readgcambutton, {
      if(rv$validGCAM != TRUE){
        return(0)
      }
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Reading from GCAM database", value = 0)

      tempdir <- paste(getwd(),"/tempdir",sep="")
      dir.create(tempdir)
      gcamdatabasepath_i <- rv_gcam$gcamdatabasepathx
      if(rv_gcam$gcamprojpathx!=""){
        reReadData_i <- F
        dataProjFile_i <- rv_gcam$gcamprojpathx
        scenOrigNames_i <- input$gcamProjScenariosSelected
        gcamdatabasepath_i <- NULL
      } else {
        reReadData_i <- T
        dataProjFile_i <- "projFile.proj"
        scenOrigNames_i <- input$gcamScenariosSelected
      }
      if(any("All" %in% input$gcamParamsSelected)){
      paramsSelect_i <- unique(gcamParamsx())} else {
        paramsSelect_i <- input$gcamParamsSelected
      }
      if(any("All" %in% input$gcamParamsSelected)){
        regionsSelect_i <- unique(gcamRegionsx())} else {
          regionsSelect_i <- input$gcamRegionsSelected
        }
      progress$inc(1/3, detail = paste("Connecting to Database", 1))
      dataGCAMraw <- gcamextractor::readgcam(reReadData = reReadData_i,
                                        dirOutputs = tempdir,
                                        gcamdatabase = gcamdatabasepath_i,
                                        scenOrigNames = scenOrigNames_i,
                                        #scenNewNames = scenNewNames_i,
                                        dataProjFile = dataProjFile_i,
                                        regionsSelect = regionsSelect_i,
                                        paramsSelect= paramsSelect_i,
                                        saveData = F)
      progress$inc(1/3, detail = paste("Unlinking", 2))
      unlink(tempdir, recursive = T)

      dataGCAMraw$data %>% as_tibble() %>%
        dplyr::select(scenario, region, subRegion, param,
                      class1, class2, x, vintage, aggregate, units,
                      value) %>%
        dplyr::rename(class=class1)-> dataGCAM
      progress$inc(1/3, detail = paste("Load Complete", 3))
      rv$dataGCAM <- dataGCAM
      showModal(
        modalDialog(
          size = "s",
          easyClose = FALSE,
          footer = NULL,
          fluidRow(
            column(6,
                   div(actionLink(
                     inputId='append',
                     label="Append to Input",
                     class = "btn btn-default shiny-download-link download_button"),
                     style = "float:center"
                   ))
            ,
            column(6,
                   div(actionLink(inputId="close",
                                  label='Overwrite Input',
                                  class = "btn btn-default shiny-download-link download_button"),
                       style="float:right;!important"
                   )
            )
          )
        ))
  }, ignoreInit = TRUE, once = TRUE)

  } # GCAM Inputs

  #...........................
  # CSV / URL Inputs
  #...........................

  if(T){ # CSV / URL Inputs

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
    print("oof")
    if (is.null(rv$filedatax) & is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))) {
      return(argus::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
    } else if(!is.null(rv$filedatax) & is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))) {
      print(rv$filedatax)
      # res <- argus::parse_local(input$filedata$datapath[1], inpu$urlfiledata$datapath)%>%
      #   dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      res <- NULL
      for (i in 1:length(input$filedata$datapath)){
        print("lllllllllll")
        print(input$filedata$datapath[i])
        argus::parse_local(input$filedata$datapath[i], inpu$urlfiledata$datapath) %>%
            dplyr::select(scenario, subRegion, param, aggregate, class, x, value) -> a
        z<-argus::addMissing(a)
        print("oofz")
        res <- dplyr::bind_rows(res, z)
      }
      return(res)
    } else if(is.null(rv$filedatax) & !is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))){
      return(argus::addMissing(rv$dataGCAM %>%
        dplyr::select(scenario, subRegion, param, aggregate, class, x, value)))
    }else{
      z <- strsplit(rv$urlfiledatax, ",")
      res <- NULL
      for (i in 1:length(z[[1]])){
        print(z[[1]])
        print("lllllllllll")
        print(z[[1]][i])
          argus::parse_remote(gsub(" ", "", z[[1]][i], fixed = TRUE))%>%
            dplyr::select(scenario, subRegion, param, aggregate, class, x, value) -> a
        res <- dplyr::bind_rows(res, a)
      }
      return(res)
    }
  })

  data <- reactive({
    return(rv$data)
  })

  observeEvent(input$append, {
    print("ooof")
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

    tbl <- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
    rv$data <- dplyr::bind_rows(rv$data, tbl)
    print("apend")
    rv$filedatax<-NULL
    rv$dataGCAM<-NULL
    rv$urlfiledatax<-NULL
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$close, {
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

    tbl <- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
    rv$data <- dplyr::bind_rows(NULL, tbl)
    print("close")
    rv$filedatax<-NULL
    rv$dataGCAM<-NULL
    rv$urlfiledatax<-NULL
    removeModal()
  }, ignoreInit = TRUE)

  } # CSV / URL Inputs

  #...........................
  # Selection of Reactive Inputs
  #...........................

  if(T){ # Select Reactive Inputs

    # Scenario Select
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

    # Ref Scenario Select
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

    # Parameters Select
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

    # Regions Select
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

    # Subset Regions Selected
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

    # Reactive year select Select based on inputs
    mapYearx <- reactive({
      if(!is.null(input$mapYear)){
        return(input$mapYear)
      }else{
        return(sort(unique(dataMapx()$x))[round(length(sort(unique(dataMapx()$x)))/2)])
      }
    })

    # Select Years for Map
    output$selectMapYear = renderUI({
      sliderInput("mapYear", label ="Year", min = min(dataMap()$x),
                  max = max(dataMap()$x), step = 5,
                  value=mapYearx(), sep="",
                  animate =F)
    })

    # Reactive Regions Select based on inputs
    subsetRegionsx <- reactive({
      if (input$subsetRegions == "All" && length(input$subsetRegions) == 1) {
        return(unique(regionsSelectedx()))
      } else if (is.null(input$subsetRegions)){
        return(unique(regionsSelectedx())[1:4])
      } else{
        return(input$subsetRegions)
      }
    })

    # Reactive Reference Scenario Select
    scenarioRefSelectedx <- reactive({
      input$scenarioRefSelected
    })

    # Reactive Regions Select based on inputs
    regionsSelectedx <- reactive({
      if (input$regionsSelected == "All" && length(input$regionsSelected) == 1) {
        return(unique(data()$subRegion))
      } else {
        return(input$regionsSelected)
      }
    })

    # Reactive Params based on inputs
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

    # Reactive Scenarios based on inputs
    scenariosSelectedx <- reactive({
      if (input$scenariosSelected == "All" && length(input$scenariosSelected) > 0) {
        return(unique(dataSum()$scenario))
      } else{
        return(input$scenariosSelected)
      }
    })

    #focusMapParamSelected helper function
    focusMapScenariox <- reactive({
      if(!is.null(input$focusMapScenarioSelected)){
        return(input$focusMapParamSelected)
      }else{
        return(unique(dataMapx()$scenario)[1])
      }
    })

    # Focus maps Inputs
    # Scenario Select
    output$selectFocusMapScenario = renderUI({
      pickerInput(
        inputId = "focusMapScenarioSelected",
        label = "Scenario",
        choices = unique(dataMapx()$scenario),
        selected =unique(dataMapx()$scenario)[1],
        multiple = FALSE)
    })

    #sfocusMapParamSelected helper function
    focusMapParamSelectedx <- reactive({
      if(!is.null(input$focusMapParamSelected)){
        return(input$focusMapParamSelected)
      }else{
        return(unique(dataMapx()$param)[1])
      }
    })

    # Parameters Select
    output$selectFocusMapParam = renderUI({
      pickerInput(
        inputId = "focusMapParamSelected",
        label = "Parameter",
        choices =  unique(dataMapx()$param),
        selected = focusMapParamSelectedx(),
        multiple = FALSE)
    })

    selectFocusMapYearx <-  reactive({
     if (is.null(input$focusMapYearSelected)){
        return(sort(unique(dataMap()$x))[round(length(sort(unique(dataMap()$x)))/2)])
      } else{
        return(input$focusMapYearSelected)
      }
    })

    # Select Years for Map
    output$selectFocusMapYear = renderUI({
      sliderInput(inputId = "focusMapYearSelected",
                  label ="Year",
                  min = min(data()$x),
                  max = max(data()$x), step = 5,
                  value=selectFocusMapYearx(),
                  sep="",
                  animate =F)
    })

  } # Select Reactive Inputs

  #...........................
  # Subsetting Data for Outputs
  #...........................

  if(T){ # Subsetting Data for Outputs

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

    # Data for summary chart
    dataSumx <- reactive({
      # print(unique(scenariosSelectedx()))
      # print(paramsSelectedx())
      x <- dataSum() %>%
        dplyr::filter(scenario %in% scenariosSelectedx(),
                      param %in% paramsSelectedx())
      return(x)
    })

    # Data for Bar Chart
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

    # Data Bar Chart Absolute Diff
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
          dplyr::filter(scenario %in% c(scenRef_i, k))  %>%
          dplyr::filter(!(is.na(class) & value==0))%>%
          dplyr::mutate(class=if_else(is.na(class),"NA",class))

        print(tbl_temp$value)
        tbl_temp <- tbl_temp %>%
          tidyr::spread(scenario, value)

        tbl_temp[is.na(tbl_temp)] <- 0

        tbl_temp <- tbl_temp %>%
          dplyr::mutate(!!paste(k, diffText, sep = "") := get(k) - get(scenRef_i)) %>%
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
      # print(tbl_pd)
      tbl_pd
    })

    # Data Chart Percent Diff
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
          dplyr::filter(scenario %in% c(scenRef_i, k))  %>%
          dplyr::filter(!(is.na(class) & value==0))%>%
          dplyr::mutate(class=if_else(is.na(class),"NA",class))


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


    # Map Data
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

    # Filter Data After Chocie
    dataMapx <- reactive({
      dataMap() %>%
        dplyr::filter(scenario %in% input$scenariosSelected,
                      param %in% paramsSelectedx())
    })

    # Data Map Absolute Diff
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

    # Data Map Percent Diff
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

  } # Subsetting Data For Outputs

  } # Read in Inputs

  #...........................
  # Outputs
  #...........................

  if(T){ # Outputs

  #...........................
  # Plotting Outputs
  #...........................

  if(T){ # Plotting Outputs

  #...........................
  # Focus Page
  #...........................

    if(T){ # Focus Page

  output$focusMap <- renderLeaflet({

    # Read in Raw Data
    dataMapFocus_raw <- dataMapx() %>%
      dplyr::ungroup() %>%
      dplyr::select(x,param,scenario,subRegion,value) %>%
      filter(param == input$focusMapParamSelected,
             scenario == input$focusMapScenarioSelected,
             x == input$focusMapYearSelected) %>%
        dplyr::left_join(argus::mappings("mappingGCAMBasins"),by="subRegion") %>%
        dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                                 TRUE~subRegion)) %>%
        dplyr::select(-subRegionMap) %>%
      dplyr::filter(subRegion!="South_Pacific_Islands")

     # Prepare for Polygons
    mapdf <- rmap::mapFind(dataMapFocus_raw)$subRegShapeFound;
    mapdf <- mapdf[mapdf$subRegion %in% dataMapFocus_raw$subRegion,]; mapdf
    mapdf@data <- mapdf@data %>%
      left_join(dataMapFocus_raw %>%
                  dplyr::select(subRegion,value)) %>%
      filter(subRegion %in% unique(dataMapFocus_raw$subRegion)) %>%
      droplevels(); mapdf

    # Create legends and color scales
    bins <- unique(argus::breaks(dataMapFocus_raw,breaks=7)[[1]]);
    print("bins")
    print(bins)
    pal <- colorBin(grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(9,length(bins)), "YlOrRd"))(length(bins)),
                    domain = dataMapFocus_raw$value, bins = bins)

    # Plot polygons on Leaflet
      labels <- sprintf(
        "<strong>%s</strong><br/>%g",
        mapdf@data$subRegion, mapdf@data$value
      ) %>% lapply(htmltools::HTML)

      if(length(bins)>1){

        initial_lat = 0
        initial_lng = 0
        initial_zoom = 3

      mapFocus <- leaflet() %>%
        setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
        addTiles() %>%
        addPolygons(data=mapdf,
                    group=~unique(subRegionType),
                    fillColor = ~pal(value),
                    fillOpacity = 0.5,
                    opacity = 0.5,
                    stroke = TRUE,
                    weight = 0.5,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
                    ) %>%
        addLegend(pal = pal,
                values = mapdf@data$value,
                opacity = 0.6,
                title = NULL,
                position = "bottomright")}

      if(length(bins)==1){
        mapFocus <- leaflet() %>%
          addTiles() %>%
          addPolygons(data=mapdf,
                      group=~unique(subRegionType),
                      fillColor = "red",
                      fillOpacity = 0.5,
                      opacity = 0.5,
                      stroke = TRUE,
                      weight = 0.5,
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
          ) %>%
          addLegend(colors = "red",
                    labels = bins,
                    opacity = 0.6,
                    title = NULL,
                    position = "bottomright")}

    mapFocus

    })

  output$focusChartSum <- renderPlotly({

    #ggplotly()
    (ggplot2::ggplot(dataSumx() %>%
                       dplyr::select(scenario, value, param, x)%>%
                       dplyr::filter(param == input$focusMapParamSelected),
                     aes(x=x,y=value,
                         group=scenario,
                         color=scenario))+
       geom_line(size=1.25) +
       ggplottheme +
       geom_line() +
       geom_point() +
       ylab(NULL) +  xlab(NULL) + ggtitle(input$focusMapParamSelected) +
       theme(legend.position="bottom",
             legend.title = element_blank(),
             plot.margin=margin(0,0,0,0,"pt"),
             text=element_text(size=12),
             aspect.ratio = NULL,
             plot.title = element_text(hjust = 0.5)))%>%
      ggplotly(tooltip = c("x","value"))%>%
      config(displayModeBar = FALSE) %>%
      layout(legend = list(orientation = "h", x=0,y=-0.2),
             hovermode = "x")

  })

  output$focusChartBar <- renderPlotly({

    dataChartPlot <- dataChartx() %>%
      dplyr::filter(!(is.na(class) & value==0))%>%
      dplyr::mutate(class=if_else(is.na(class),"NA",class))%>%
      dplyr::select(scenario, value, param, class, x)%>%
      dplyr::filter(param == input$focusMapParamSelected,
                    scenario == input$focusMapScenarioSelected)

    # Check Color Palettes
    palAdd <- rep(c("firebrick3","dodgerblue3","forestgreen","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise"),10000)

    missNames <- unique(dataChartPlot$class)[!unique(dataChartPlot$class) %in%
                                               names(jgcricolors::jgcricol()$pal_all)]
    if (length(missNames) > 0) {
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
    } else{
      palCharts <- jgcricolors::jgcricol()$pal_all
    }
    print(palCharts)

    #ggplotly()
    (ggplot2::ggplot(dataChartPlot,
                     aes(x=x,y=value,
                         group=scenario,
                         fill=class))+
       ggplottheme +
       scale_fill_manual(breaks=names(palCharts),values=palCharts) +
       scale_y_continuous(position = "left")+
       geom_bar(position="stack", stat="identity") +
       theme(legend.position="bottom",
              strip.text.y = element_blank(),
              legend.title = element_blank(),
              legend.margin=margin(0,0,0,0,"pt"),
              legend.key.height=unit(0, "cm"),
              text = element_text(size = 15),
              plot.margin=margin(0,0,0,0,"pt")) +
      ylab(NULL) +  xlab(NULL))%>%
      ggplotly(tooltip = c("class","x","value"))%>%
      config(displayModeBar = FALSE) %>%
      layout(showlegend = TRUE, legend = list(font = list(size = 10)))%>%
      layout(legend = list(orientation = "h", x=0,y=-0.2))

  })


    } # Focus Page

  #...........................
  # Summary Plot
  #...........................

    if(T){ # Summary Plot

      output$summary <- renderPlot({
        argus::summaryPlot(NULL, 17.5, 20, dataSumx())
      },
      height=function(){
        if (length(unique(dataChartx()$param))>3){
          if(length(unique(dataChartx()$param))%%3==0){
            multiplier = length(unique(dataChartx()$param))%/%3
          } else {
            multiplier = (length(unique(dataChartx()$param))%/%3)+1
          }
          return(multiplier*400)
        }else{
          return("auto")
        }
      }
      )

      output$downloadPlotSum <- downloadHandler(
        filename = "summaryChart.png",
        content = function(filename) {
          ggsave(
            filename,
            plot=argus::summaryPlot(0.75, 10, 10, dataSumx()),
            #max(13,min(13,1.25*length(unique(dataChartx()$param)))),
            height = argus::exportHeight(3, 49, length(unique(dataChartx()$param)), 3),
            width=argus::exportWidth(10, length(unique(dataChartx()$param)), 3),
            units="in"
          )
        })

    } # Summary Plot

  #...........................
  # Summary Plot Compare Regions
  #...........................

    if(T){# Summary Plot Compare Regions

  output$summaryReg <- renderPlot({
    summaryPlotReg(15, dataMapx(),ggplottheme, subsetRegionsx())
  },
  height=function(){300*length(unique(dataMapx()$param))}#,
  #width=function(){max(400,300*length(subsetRegionsx())+100)}
  )

  output$downloadPlotSumReg <- downloadHandler(
    filename = "summaryChartReg.png",
    content = function(filename) {
      ggsave(file,plot=summaryPlotReg(10,dataMapx(),ggplottheme, subsetRegionsx()),
             height = argus::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = argus::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in")
    })

    } # Summary Plot Compare Regions

  #...........................
  # Bar Charts
  #...........................

    if(T){ # Bar Charts

    #...........................
    # Pick Type of Chart
    #...........................

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


  #...........................
  # Chart Plot Abs
  #...........................

  output$plotAbs <- renderPlot({
    argus::plotAbs(dataChartx(), input$scenarioRefSelected)
  },
  height=function(){400*length(unique(dataChartx()$param))}#,
  #width=function(){500*length(unique(data()$scenario))}
  )

  output$downloadPlotChart <- downloadHandler(
    file = "barChart.png",
    content = function(file) {
      # foodweb(where=environment())
      ggsave(file,plot=chartPlot(),
             width=argus::exportWidth(49, length(unique(dataChartx()$param)), 6),
             height=argus::exportHeight(1, 49, length(unique(dataChartx()$param)), 3)+2,
             unit = "in"
      )
      # exportHeight<-function(chartsperrow, max_height_in, numelement, lenperchart){
      # max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
    })

  #...........................
  # Chart Plot Abs Diff
  #...........................

  output$plotDiff <- renderPlot({
    argus::plotDiffAbs(dataDiffAbsx(), input$scenarioRefSelected)
  },
 height=function(){400*length(unique(dataChartx()$param))}#,
 #width=function(){500*length(unique(data()$scenario))}
  )

  output$plotPerc <- renderPlot({
    argus::plotDiffPrcnt(dataPrcntAbsx(), input$scenarioRefSelected)
  },
  height=function(){400*length(unique(dataChartx()$param))}#,
  #width=function(){500*length(unique(data()$scenario))}
  )

    } # Bar Charts

  #...........................
  # Maps
  #...........................

    if(T){ # Maps

  # Absolute Value Map
  output$mapAbs <- renderPlot({
    argus::map(1, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataMapx())
  },
  height=function(){500*length(unique(dataMapx()$param))}#,
  #width=function(){max(600, 450*length(unique(dataMapx()$scenario)))}
  )

  # Percentage Difference Map
  output$mapPerc <- renderPlot({
    argus::map(2, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataPrcntAbsMapx())
  },
  height=function(){500*length(unique(dataMapx()$param))}#,
  )

  # Absolute Difference Map
  output$mapDiff <- renderPlot({
    argus::map(3, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataDiffAbsMapx())
  },
  height=function(){500*length(unique(dataMapx()$param))}#,
  #width=function(){max(600, 450*length(unique(dataMapx()$scenario)))}
  )

  # Download
  output$downloadMap <- downloadHandler(
    file = "map.png",
    content = function(file) {
      ggsave(
        file,
        plot=map(),
        height = argus::exportHeight(3, 49, length(unique(dataMapx()$param)), 7),
        width=argus::exportWidth(10, length(unique(data()$scenario)), 10),
        units="in"
      )
    })

    } # Maps

  } # Plotting Outputs

  #...........................
  # Data Table
  #...........................

  if(T) { # Data Table

  output$table <- renderDT(
    # Point to reactive values
    dataChartx(),
    filter = "top"
  )

  output$downloadTable <- downloadHandler(
    file = "table.csv",
    content = function(file) {
      write.csv(data() , file)
    })

  } # Data Table

  #...........................
  # Download All
  #...........................
  output$downloadAll <- downloadHandler(
    file = "all.zip",
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- c("table.csv",
              "summaryChart.png",
              "barCharts.png",
              "summaryChartReg.png",
              "map.png",
              "mapBase.png"
              )
      write.csv(data(), "table.csv")
      ggsave("summaryChart.png", plot=argus::summaryPlot(0.75, 10, 10, dataSumx()),
             #max(13,min(13,1.25*length(unique(dataChartx()$param)))),
             height = argus::exportHeight(3, 49, length(unique(dataChartx()$param)), 4),
             width=argus::exportWidth(10, length(unique(dataChartx()$param)), 2),
             units="in"
      )
      ggsave("barCharts.png",plot=chartPlot(),
              width=argus::exportWidth(49, length(unique(dataChartx()$param)), 5),
              height=argus::exportHeight(1, 49, length(unique(dataChartx()$param)), 5)+2,
              unit = "in"
             )
      ggsave("summaryChartReg.png", plot=summaryPlotReg(10, dataMapx(),ggplottheme, subsetRegionsx()),
             height = argus::exportHeight(1, 49, length(unique(dataMapx()$param)), 3),
             width = argus::exportWidth(49, length(unique(subsetRegionsx())), 2)+3,
             units = "in"
             )
      ggsave("map.png",plot=map(),
              height = argus::exportHeight(3, 49, rv$pcount, 3),
              width=argus::exportWidth(10, length(unique(dataChartx()$param)), 3),
              units="in"
              )
      ggsave("mapBase.png", plot=mapBase(dataMapx()),
              height = argus::exportHeight(3, 49, rv$pcount, 3),
              width=argus::exportWidth(10, length(unique(dataChartx()$param)), 3),
              units="in"
            )

      print(fs)
      zip::zip(zipfile=file, files=fs)
    }
  )

  } # Outputs

} # Close Server

