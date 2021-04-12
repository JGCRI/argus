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
library(mvbutils)

#---------------------------
# Options
#---------------------------
options(shiny.maxRequestSize=100*1024^2)
options(shiny.trace = TRUE)
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

  observeEvent(input$inputz, {
    if (input$inputz == "csv"){
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
          br(),
          # CSV Data -------------------------------------
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
            label = "Enter url to csv or zip file",
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


  observeEvent(
    {input$readgcambutton
    input$readurlbutton
    input$readfilebutton}, {
    removeModal()
    req(data_raw())
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

  #------------------------------------------------------
  # Creating a reactive environment to choose gcamdatabase
  # Based on folder input or text input
  #------------------------------------------------------
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


      gcamdatabasePath_dir <- gsub("/$","",gsub("[^/]+$","",rv_gcam$gcamdatabasepathx)); gcamdatabasePath_dir
      gcamdatabasePath_file <- gsub('.*/ ?(\\w+)', '\\1', rv_gcam$gcamdatabasepathx); gcamdatabasePath_file
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
      b <- as.vector(unlist(strsplit(s2,",")))
      if (!is.null(b)){
        rv$validGCAM <- TRUE
      }else{
        rv$validGCAM <- FALSE
      }
      return(b)
  })

  gcamScenariosxProj <- reactive({
    scens <- (names(rgcam::loadProject(rv_gcam$gcamprojpathx)))
    if (!is.null(scens)){
      rv$validGCAM <- TRUE
    }else{
      rv$validGCAM <- FALSE
    }
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
      dataGCAMraw <- argus::readgcam(reReadData = reReadData_i,
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
  }, ignoreInit = TRUE)


  #---------------------------
  # Data File (CSV)
  #---------------------------

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

  addMissing<-function(data){
      NULL -> year -> aggregate -> scenario -> subRegion -> param -> x -> value
      print("iif")
      print(names(data))
    if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){
      data<-data%>%dplyr::mutate(scenario="scenario")
    }else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
      for (i in 1:length(names(data))){
        print(typeof(names(data)))
        }
            #browser()
      data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~as.character(scenario)))
    }
    print("xiif")
    if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~as.character(scenario)))}
    if(!any(grepl("\\<subRegion\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegion="subRegion")}else{
      data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegion\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~as.character(subRegion)))}
    if(!any(grepl("\\<subRegions\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegions\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~as.character(subRegion)))}
    if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~as.character(param)))}
    if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=dplyr::case_when(is.na(param)~"param",TRUE~as.character(param)))}
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
      data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~as.character(units)))}
    if(!any(grepl("\\<units\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(units="units")}else{
      data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<units\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(units=dplyr::case_when(is.na(units)~"units",TRUE~as.character(units)))}
    if(!"x"%in%names(data)){
      if("year"%in%names(data)){
        data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(!any(grepl("\\<aggregate\\>",names(data),ignore.case = T))){
    if(is.null(aggregate)){data<-data%>%dplyr::mutate(aggregate="sum")}else{
        data<-data%>%dplyr::mutate(aggregate="sum")}
    }else{
      data <- data %>% dplyr::rename(!!"aggregate" := (names(data)[grepl("\\<aggregate\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(aggregate=dplyr::case_when(is.na(aggregate)~"sum",
                                                          TRUE~as.character(aggregate)))}
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
      if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
        data<-data%>%dplyr::mutate(class="class")}else{data<-data%>%dplyr::mutate(class=class)}}else{
          data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("\\<class\\>",names(data),ignore.case = T)])[1])
          data<-data%>%dplyr::mutate(class=dplyr::case_when(is.na(class)~"class",TRUE~as.character(class)))}

    data <- data %>%
      dplyr::select(scenario,subRegion,param,class,x,aggregate,value)
      return(data)
  }

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
        z<-addMissing(a)
        print("oofz")
        res <- dplyr::bind_rows(res, a)
      }
      return(res)
    } else if(is.null(rv$filedatax) & !is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))){
      return(rv$dataGCAM %>%
        dplyr::select(scenario, subRegion, param, aggregate, class, x, value))
    }else{
      return(argus::addMissing(
        argus::parse_remote(input$urlfiledata)%>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      ))
    }
  })

  data <- reactive({
    return(rv$data)
  })

  observeEvent(input$append, {
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
    rv$data <- dplyr::bind_rows(rv$data[0,], tbl)
    print("close")
    removeModal()
  }, ignoreInit = TRUE)

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
      browser()
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
    z <- z%>%
       addLayersControl(
         baseGroups = unique(subRegTypelist),
         # overlayGroups = unique(subRegTypelist),
         options = layersControlOptions(collapsed = FALSE)
       )
    rv$subRegTypelist = subRegTypelist
    rv$mapflag = 1
    rv$selectedBase = subRegTypelist[1]
    return(z)
  })

  observe({
    if(rv$mapflag == 1){
      check()
      rv$mapflag = 0
    }
  })

  check <- function(){
    hidden_group <- data()$subRegion[which(!data()$subRegion %in% reactiveValuesToList(input)$regionsSelected)]
    for (i in unique(hidden_group)){
      print(paste("hiding", " ", i))
      leafletProxy("mymap") %>% hideGroup(i)
    }
    for (i in unique(reactiveValuesToList(input)$regionsSelected)){
      print(paste("showing", " ", i))
      leafletProxy("mymap") %>% showGroup(i)
    }
    for (i in unique(rv$selectedBase)){
      leafletProxy("mymap") %>% hideGroup(i)
      leafletProxy("mymap") %>% showGroup(i)
    }
    return(0)
  }


  observeEvent(input$regionsSelected, {
    check()
  }, ignoreNULL = FALSE)

  observeEvent(input$mymap_groups,{
    print(input$mymap_groups)
    rv$selectedBase = (reactiveValuesToList(input)$mymap_groups)[which((reactiveValuesToList(input)$mymap_groups) %in%   rv$subRegTypelist)]
    print(rv$selectedBase)
  })

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
      leafletProxy("mymap") %>% hideGroup(l)
      selectedx =  selectedx[!(selectedx %in% l)]
    }else{
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

  # Filter Data after Reactive Choices
  #---------------------------
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
  #---------------------------
  # Filter Data after Reactive Choices
  #---------------------------
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
  # summaryPlot <- function(aspectratio, textsize, titletext){
  #   ggplot2::ggplot(dataSumx(),
  #                   aes(x=x,y=value,
  #                       group=scenario,
  #                       color=scenario))+
  #     geom_line(size=1.25) +
  #     ggplottheme +
  #     geom_line() +
  #     ylab(NULL) +  xlab(NULL) +
  #     facet_wrap(.~param, scales="free", ncol = 3,
  #                labeller = labeller(param = label_wrap_gen(15)))+
  #     theme(legend.position="top",
  #           legend.text=element_text(size=titletext),
  #           legend.title = element_blank(),
  #           plot.margin=margin(20,20,20,0,"pt"),
  #           text=element_text(size=textsize),
  #           aspect.ratio = aspectratio
  #     )
  # }

  output$summary <- renderPlot({
    argus::summaryPlot(NULL, 17.5, 20, dataSumx())
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


  #---------------------------
  # Summary Plot Compare Regions
  #---------------------------

  output$summaryReg <- renderPlot({
    summaryPlotReg(10, dataMapx(),ggplottheme, subsetRegionsx())
  },
  height=function(){200*length(unique(dataMapx()$param))},
  width=function(){max(400,200*length(subsetRegionsx())+100)}
  )

  output$downloadPlotSumReg <- downloadHandler(
    filename = "summaryChartReg.png",
    content = function(filename) {
      ggsave(file,plot=summaryPlotReg(10,dataMapx(),ggplottheme, subsetRegionsx()),
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
  # Chart Plot Abs Diff
  #---------------------------

  output$plotDiff <- renderPlot({
    argus::plotDiff(dataDiffAbsx(), input$scenarioRefSelected)
  },
  height=function(){300*length(unique(dataChartx()$param))},
  width=function(){max(600, 400*length(unique(data()$scenario)))}
  )


  # #---------------------------
  # # Chart Plot Perc
  # #---------------------------
  # plotPerc <- function(){
  #   g <- 2
  #   print("perc diff")
  #   dataChartPlot <- dataPrcntAbsx()
  #
  #   plist <- list()
  #   x = 1
  #   for(i in 1:length(unique(dataChartPlot$param))){
  #     # Check Color Palettes
  #     palAdd <- c("firebrick3","dodgerblue3","forestgreen","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise")
  #
  #     missNames <- unique(dataChartPlot$class)[!unique(dataChartPlot$class) %in%
  #                                                names(pal_all)]
  #     if (length(missNames) > 0) {
  #       palAdd <- palAdd[1:length(missNames)]
  #       names(palAdd) <- missNames
  #       palCharts <- c(pal_all, palAdd)
  #     } else{
  #       palCharts <- pal_all
  #     }
  #     print(palCharts)
  #
  #     chartz <- dataChartPlot %>%
  #       filter(param==unique(dataChartPlot$param)[i], scenario == input$scenarioRefSelected)
  #     z<-x
  #     plist[[z+1]] <-  ggplot2::ggplot(dataChartPlot %>%
  #                                        filter(param==unique(dataChartPlot$param)[i], scenario != input$scenarioRefSelected)%>%
  #                                        droplevels(),
  #                                      aes(x=x,y=value,
  #                                          # group=class,
  #                                          colour=class
  #                                      )) +
  #       ggplottheme +
  #       ylab(NULL) + xlab(NULL) +
  #       scale_color_manual(breaks=names(palCharts),values=palCharts) +
  #       # scale_y_continuous(position = "right")+
  #       # geom_bar(position="stack", stat="identity") +
  #       geom_line()+
  #       geom_point()+
  #       scale_color_manual(breaks=names(palCharts),values=palCharts) +
  #       facet_grid(param~scenario, scales="free",switch="y")+
  #       theme(legend.position="bottom",
  #             strip.text.y = element_blank(),
  #             legend.title = element_blank(),
  #             legend.margin=margin(0,0,0,0,"pt"),
  #             legend.key.height=unit(0, "cm"),
  #             text = element_text(size = 12.5),
  #             plot.margin=margin(20,20,20,0,"pt"))
  #     x = x+2
  #
  #     plist[[z]] <-  ggplot2::ggplot(chartz%>%
  #                                      droplevels(),
  #                                    aes(x=x,y=value,
  #                                        group=scenario,
  #                                        fill=class))+
  #       ggplottheme +
  #       xlab(NULL) +
  #       ylab(unique(dataChartPlot$param)[i])+
  #       scale_fill_manual(breaks=names(palCharts),values=palCharts) +
  #       scale_y_continuous(position = "left")+
  #       geom_bar(position="stack", stat="identity") +
  #       facet_grid(param~scenario, scales="free",switch="y")+
  #       theme(legend.position="bottom",
  #             strip.text.y = element_blank(),
  #             legend.title = element_blank(),
  #             legend.margin=margin(0,0,0,0,"pt"),
  #             legend.key.height=unit(0, "cm"),
  #             text = element_text(size = 12.5),
  #             plot.margin=margin(20,0,20,0,"pt"))
  #   }
  #   cowplot::plot_grid(plotlist = plist, ncol=g, align="v", rel_widths = c(1, length(unique(dataChartPlot$scenario))-1))
  # }

  output$plotPerc <- renderPlot({
    argus::plotDiff(dataPrcntAbsx(), input$scenarioRefSelected)
  },
  height=function(){300*length(unique(dataChartx()$param))},
  width=function(){max(600, 400*length(unique(data()$scenario)))}
  )




  #---------------------------
  # Chart Plot Abs
  #---------------------------


  output$plotAbs <- renderPlot({
    argus::plotAbs(dataChartx(), input$scenarioRefSelected)
  },
  height=function(){300*length(unique(dataChartx()$param))},
  width=function(){max(600, 400*length(unique(data()$scenario)))}
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

  }#,
  #height=function(){300*(rv$pcount)}
  )

  #---------------------------
  # Map Analysis by Scenario x Param
  #---------------------------

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

  # map <- function(flag,
  #                 mapLegend,
  #                 mapYear,
  #                 scenarioRefSelected,
  #                 dataMapx,
  #                 dataMapz){
  #

  output$mapAbs <- renderPlot({
    argus::map(1, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataMapx())
  },
  height=function(){225*length(unique(dataMapx()$param))},
  width=function(){max(600, 450*length(unique(dataMapx()$scenario)))}
  )

  output$mapPerc <- renderPlot({
    argus::map(2, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataPrcntAbsMapx())
  },
  height=function(){225*length(unique(dataMapx()$param))},
  width=function(){max(600, 450*length(unique(dataMapx()$scenario)))}
  )

  output$mapDiff <- renderPlot({
    argus::map(3, input$mapLegend, input$mapYear, input$scenarioRefSelected, dataMapx(), dataDiffAbsMapx())
  },
  height=function(){225*length(unique(dataMapx()$param))},
  width=function(){max(600, 450*length(unique(dataMapx()$scenario)))}
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
}

