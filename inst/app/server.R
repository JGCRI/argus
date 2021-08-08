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
library(sp)

#...........................
# Options
#...........................

options(shiny.maxRequestSize=100*1024^2)
#options(shiny.trace = TRUE)
pal_all <- rmap::mappings()$pal_all
enableBookmarking(store = c("server"))

#...........................
# Server object
#...........................

server <- function(input, output, session) {

  #toggling on dropdown menus to ensure loading of bookmark data

  toggleDropdownButton("inputx", session = session)
  toggleDropdownButton("linestoryboard", session = session)
  # NOTE:
  # To collapse code for easy reading place cursor here and enter: ALT+0
  # To Expand code again place cursor here and enter: ALT+SHIFT+O (O not 0)

  #...........................
  # Storyboard
  #...........................

  # Toggle Sidebar

  # observeEvent(input$storyboard, {
  #   shinyjs::toggle(id = "Sidebar")
  #
  #   if (input$toggleSidebar %% 2 == 1) {
  #     icon <-  icon("caret-down","fa-1x")
  #   } else {
  #     icon <-  icon("caret-up","fa-1x")
  #   }
  #   updateActionButton(session,
  #                      "toggleSidebar",
  #                      icon = icon)
  #
  # })

  observeEvent(input$collapse1, {
    print(input$linestoryboard)
  })

  observeEvent(input$linestoryboardtoggle, {
    print(input$linestoryboard)
    text = input$linestoryboard
    title = input$linestoryboardtitle
    print(text)
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        label = "Story Board",
        textInput(inputId="linestoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="linestoryboard",label="Body", value = text, width = "100%", height="50vh", resize="vertical")
    ))
    print(text)
  })

  output$linestoryboardtext <- renderText({
    input$linestoryboard
  })

  output$linestoryboardtexttitle <- renderText({
    input$linestoryboardtitle
#    HTML(paste("<b>", input$linestoryboardtitle, "</b>"))
  })

  observeEvent(input$compregstoryboardtoggle, {
    text = input$compregstoryboard
    title = input$compregstoryboardtitle
        showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        label = "Story Board",
        textInput(inputId="compregstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="compregstoryboard",label="Body", value = text, width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$compregstoryboardtext <- renderText({
    input$compregstoryboard
  })

  output$compregstoryboardtexttitle <- renderText({
    input$compregstoryboardtitle
  })


  observeEvent(input$absvalstoryboardtoggle, {
    text = input$absvalstoryboard
    title = input$absvalstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        label = "Story Board",
        textInput(inputId="absvalstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="absvalstoryboard",value = text, label="Body", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$absvalstoryboardtext <- renderText({
    input$absvalstoryboard
  })

  output$absvalstoryboardtexttitle <- renderText({
    input$absvalstoryboardtitle
    #HTML(paste("<b>",     input$absvalstoryboard, "</b>"))
  })

  observeEvent(input$absdifstoryboardtoggle, {
    text = input$absdifstoryboard
    title = input$absdifstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        textInput(inputId="absdifstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="absdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$absdifstoryboardtext <- renderText({
    input$absdifstoryboard
  })

  output$absdifstoryboardtexttitle <- renderText({
    input$absdifstoryboardtitle
    #HTML(paste("<b>",     input$absdifstoryboardtitle, "</b>"))
  })

  observeEvent(input$percdifstoryboardtoggle, {
    text = input$percdifstoryboard
    title = input$percdifstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        textInput(inputId="percdifstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="percdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$percdifstoryboardtext <- renderText({
    input$percdifstoryboard
  })

  output$percdifstoryboardtexttitle <- renderText({
    input$percdifstoryboardtitle
    #    HTML(paste("<b>", input$percdifstoryboardtitle, "</b>"))
  })


  observeEvent(input$mapabsstoryboardtoggle, {
    text = input$mapabsstoryboard
    title = input$mapabsstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        textInput(inputId="mapabsstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="mapabsstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$mapabsstoryboardtext <- renderText({
    input$mapabsstoryboard
  })

  output$mapabsstoryboardtexttitle <- renderText({
    input$mapabsstoryboardtitle
#    HTML(paste("<b>", input$mapabsstoryboardtitle, "</b>"))
  })

  observeEvent(input$mapabsdifstoryboardtoggle, {
    text = input$mapabsdifstoryboard
    title = input$mapabsdifstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        textInput(inputId="mapabsdifstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="mapabsdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$mapabsdifstoryboardtext <- renderText({
    input$mapabsdifstoryboard
  })

  output$mapabsdifstoryboardtexttitle <- renderText({
    input$mapabsdifstoryboardtitle
    #HTML(paste("<b>",    input$mapabsdifstoryboardtitle, "</b>"))
  })

  observeEvent(input$mappercdifstoryboardtoggle, {
    text = input$mappercdifstoryboard
    title = input$mappercdifstoryboardtitle
    showModal(
      modalDialog(
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        textInput(inputId="mappercdifstoryboardtitle", label="Title", value = title, width = "100%"),
        textAreaInput(inputId="mappercdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
      ))
  })

  output$mappercdifstoryboardtext <- renderText({
    input$mappercdifstoryboard
  })

  output$mappercdifstoryboardtexttitle <- renderText({
    input$mappercdifstoryboardtitle
    #HTML(paste("<b>",input$mappercdifstoryboardtitle, "</b>"))
  })


  #...........................
  # Preloaded Data
  #...........................
  if(T){
    preloaded_df <- argus::preloaded_data()
    print(preloaded_df)

    output$examplesPreloadInput = renderUI({
      selectInput(
        inputId = "examplesPreload",
        label = "Select example preload",
        choices = c(preloaded_df$name,""),
        selected = "",
        multiple = F
      )
    })


    observeEvent(input$examplesPreload, {
      if (input$examplesPreload == ""){
        return()
      }
      temp <- tempfile()
      utils::download.file(preloaded_df$link, temp)
      state <- readRDS(temp)
      rv$data <- state$data
      updateVals(state)
      print("oof")
      #session$sendCustomMessage("setsetting", c("focusMapScenarioSelected", settingfocusMapScenarioSelected))
      #session$sendCustomMessage("openlink", dplyr::filter(preloaded_df, name==input$examplesPreload)$link)
      updatePickerInput(
        inputId = "examplesPreload",
        session=session,
        selected = ""
      )
    })
    #
    # output$examplePreload = renderUI({
    #   pickerInput(
    #     inputId = "scenariosSelected",
    #     label = "Select example preload",
    #     choices = unique(data()$scenario),
    #     selected = unique(data()$scenario),
    #     multiple = TRUE,
    #     options = list(
    #       `actions-box` = TRUE,
    #       `deselect-all-text` = "None",
    #       `select-all-text` = "All",
    #       `none-selected-text` = "None Selected"
    #     )
    #   )
    # })
  }


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
          fluidRow(column(6,div(
                     downloadButton(
                       'bookmark',
                       "Download Bookmark",
                       class = "download_button"
                     ),
                     style = "float:left;width=100%;margin-bottom:40px"
                   )),
                   column(6,div(
                     actionLink(inputId="._bookmark_",
                                label="URL",
                                class = "btn btn-default shiny-download-link download_button",
                                icon = icon("link","fa-1x")
                     ),
                     style = "float:right;width=100%;margin-bottom:40px"
                   ))
                   ),
          fileInput(
            inputId = "readbookmark",
            label = "Upload Bookmark",
            accept = c(".rds"),
            multiple = TRUE,
            width = "100%"
          ),
          textInput(
            inputId = "readurlrds",
            label = "Read Bookmark from URL",
            width = "100%",
            placeholder =  "https://raw.githubusercontent.com/JGCRI/argus/main/inst/extdata/exampleData.csv"
          )
        )
      )
    })

    #...........................
    # URL Bookmark
    #...........................

    setBookmarkExclude(c("mappercdifstoryboardtoggle","mapabsdifstoryboardtoggle","mapabsstoryboardtoggle","percdifstoryboardtoggle","absdifstoryboardtoggle","absvalstoryboardtoggle","compregstoryboardtoggle","linestoryboardtoggle","urlfiledata","filedata","filedata","append", "close", "readfilebutton", "readurlbutton", "readgcambutton", "._bookmark_", "loadbookmark"))

    #URL bookmark onbookmark
    onBookmark(function(state) {
      state$values$data <- rv$data
      print(input$linestoryboard)
    })

    #URL bookmark onRestore
    onRestore(function(state) {
      print(state)
      rv$data <- state$values$data
      session$sendCustomMessage("setsetting", c("inputz", ""))
      session$sendCustomMessage("setsetting", c("linestoryboardtitle", input$linestoryboardtitle))
      session$sendCustomMessage("setsetting", c("linestoryboard", input$linestoryboard))

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          label = "Story Board",
          textInput(inputId="linestoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="linestoryboard",label="Body", value = text, width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          label = "Story Board",
          textInput(inputId="compregstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="compregstoryboard",label="Body", value = text, width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          label = "Story Board",
          textInput(inputId="absvalstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="absvalstoryboard",value = text, label="Body", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          textInput(inputId="absdifstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="absdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          textInput(inputId="percdifstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="percdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          textInput(inputId="mapabsstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="mapabsstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          textInput(inputId="mapabsdifstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="mapabsdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()

      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          textInput(inputId="mappercdifstoryboardtitle", label="Title", value = title, width = "100%"),
          textAreaInput(inputId="mappercdifstoryboard",value = text, label="Story Board", width = "100%", height="50vh", resize="vertical")
        ))

      removeModal()


    })

    #...........................
    # RDS Bookmark
    #...........................



    #rds bookmark download handler
    output$bookmark <- downloadHandler(
      filename <- "argus_bookmark_data.rds",
      content = function(file){
        state <- isolate(reactiveValuesToList(input))
        state$data <- rv$data
        saveRDS(state, file)
      }
    )

    #rds bookmark url handler
    observeEvent(input$readurlrds, {
      if (input$readurlrds == ""){
        return(0)
      }
      tryCatch({
        temp <- tempfile()
        utils::download.file(input$readurlrds, temp)
        state <- readRDS(temp)
        rv$data <- state$data
        removeModal()
        updateVals(state)
      },finally = {
        return(0)
      })

      }, ignoreInit = TRUE
    )

    #rds bookmark upload handler
    observeEvent(input$readbookmark, {
      removeModal()
      state <- readRDS(input$readbookmark$datapath)
      rv$data <- state$data
      updateVals(state)
    })

    #update input values from rds state
    updateVals <- function(state){
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
        updateSliderInput(
          inputId = "focusMapYearSelected",
          session=session,
          min = min(dataMapx()$x),
          max = max(dataMapx()$x),
          value=settingfocusMapYearSelected
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
          updatePickerInput(
            inputId = "subsetRegions",
            session=session,
            choices = unique(data()$subRegion),
            selected = state$subsetRegions)
          session$sendCustomMessage("setsetting", c("subsetRegions", settingsSubsetRegions))
        }

        # Regions Update
        settingsRegions <- state$regionsSelected
        if(any(unique(settingsRegions) %in% unique(data()$subRegion))){
          updatePickerInput(
            session=session,
            inputId = "regionsSelected",
            selected = unique(settingsRegions)[unique(settingsRegions) %in% unique(data()$subRegion)],
          )
        }

        # Parameters Update
        settingsParams <- state$paramsSelected
        if(any(unique(settingsParams) %in% unique(data()$param))){
          updatePickerInput(
            session=session,
            inputId = "paramsSelected",
            selected = unique(settingsParams)[unique(settingsParams) %in% unique(data()$param)],
          )
        }

        # Scenario Update
        settingsScenario <- state$scenariosSelected
        if(!any(unique(settingsScenario) %in% unique(data()$scenario))){
        }
        if(any(unique(settingsScenario) %in% unique(data()$scenario))){
          updatePickerInput(
            session=session,
            inputId = "scenariosSelected",
            selected = unique(settingsScenario)[unique(settingsScenario) %in% unique(data()$scenario)],
          )
        }

        # Reference Scenario Update
        settingsRefScenario <- state$scenarioRefSelected
        if(any(unique(settingsRefScenario) %in% unique(data()$scenario))){
          updatePickerInput(
            session=session,
            inputId = "scenarioRefSelected",
            selected = unique(settingsRefScenario)[unique(settingsRefScenario) %in% unique(data()$scenario)],
          )
        }

        # Line story board
        settingslinestoryboard <- state$linestoryboard
        updateTextAreaInput(
          session=session,
          inputId = "linestoryboard",
          value = settingslinestoryboard
        )
        session$sendCustomMessage("setsetting", c("linestoryboard", settingslinestoryboard))

        settingslinestoryboardtitle <- state$linestoryboardtitle
        updateTextInput(
          session=session,
          inputId = "linestoryboardtitle",
          value = settingslinestoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("linestoryboardtitle", settingslinestoryboardtitle))

        # Line story board
        settingscompregstoryboard <- state$compregstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "compregstoryboard",
          value = settingscompregstoryboard
        )

        session$sendCustomMessage("setsetting", c("compregstoryboard", settingscompregstoryboard))

        # Line story board
        settingscompregstoryboardtitle <- state$compregstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "compregstoryboardtitle",
          value = settingscompregstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("compregstoryboardtitle", settingscompregstoryboardtitle))


                # Line story board
        settingsabsvalstoryboard <- state$absvalstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "absvalstoryboard",
          value = settingsabsvalstoryboard
        )

        session$sendCustomMessage("setsetting", c("absvalstoryboard", settingsabsvalstoryboard))

        # Line story board
        settingsabsvalstoryboardtitle <- state$absvalstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "absvalstoryboardtitle",
          value = settingsabsvalstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("absvalstoryboardtitle", settingsabsvalstoryboardtitle))
                # Line story board
        settingsabsdifstoryboard<- state$absdifstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "absdifstoryboard",
          value = settingsabsdifstoryboard
        )

        session$sendCustomMessage("setsetting", c("absdifstoryboard", settingsabsdifstoryboard))

        # Line story board
        settingsabsdifstoryboardtitle<- state$absdifstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "absdifstoryboardtitle",
          value = settingsabsdifstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("absdifstoryboardtitle", settingsabsdifstoryboardtitle))


         settingspercdifstoryboard<- state$percdifstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "percdifstoryboard",
          value = settingspercdifstoryboard
        )

        session$sendCustomMessage("setsetting", c("percdifstoryboard", settingspercdifstoryboard))

        settingspercdifstoryboardtitle<- state$percdifstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "percdifstoryboardtitle",
          value = settingspercdifstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("percdifstoryboardtitle", settingspercdifstoryboardtitle))

         settingsmapabsdifstoryboard<- state$mapabsdifstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "mapabsdifstoryboard",
          value = settingsmapabsdifstoryboard
        )

        session$sendCustomMessage("setsetting", c("mapabsdifstoryboard", settingsmapabsdifstoryboard))

        settingsmapabsdifstoryboardtitle<- state$mapabsdifstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "mapabsdifstoryboardtitle",
          value = settingsmapabsdifstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("mapabsdifstoryboardtitle", settingsmapabsdifstoryboardtitle))

        settingsmapabsstoryboard<- state$mapabsstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "mapabsstoryboard",
          value = settingsmapabsstoryboard
        )
        session$sendCustomMessage("setsetting", c("mapabsstoryboard", settingsmapabsstoryboard))

        settingsmapabsstoryboardtitle<- state$mapabsstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "mapabsstoryboardtitle",
          value = settingsmapabsstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("mapabsstoryboardtitle", settingsmapabsstoryboardtitle))

        settingsmappercdifstoryboard<- state$mappercdifstoryboard
        updateTextAreaInput(
          session=session,
          inputId = "mappercdifstoryboard",
          value =  settingsmappercdifstoryboard
        )

        session$sendCustomMessage("setsetting", c("mappercdifstoryboard", settingsmappercdifstoryboard))

        settingsmappercdifstoryboardtitle<- state$mappercdifstoryboardtitle
        updateTextInput(
          session=session,
          inputId = "mappercdifstoryboardtitle",
          value =  settingsmappercdifstoryboardtitle
        )

        session$sendCustomMessage("setsetting", c("mappercdifstoryboardtitle", settingsmappercdifstoryboardtitle))

    }



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
    removeModal()
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

  observeEvent(input$help,{
    session$sendCustomMessage("handler1", unique(data()$subRegion))
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
  dataDefault <- argus::exampleData
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
      x <- utils::capture.output(rgcam::localDBConn(gcamdatabasePath_dir,gcamdatabasePath_file), type="message")
      x <- gsub(", ",",",gsub(": ","",gsub("Database scenarios:  ","",x)));x
      gcam_scenarios_extracted <- as.vector(unlist(strsplit(gsub("Database scenarios: ","",x),",")))
      if (!is.null(gcam_scenarios_extracted)){
        rv$validGCAM <- TRUE
      }else{
        rv$validGCAM <- FALSE
      }
      return(gcam_scenarios_extracted)
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
    unique((rmap::mappings()$mapParamQuery)$param)
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
   unique((rmap::mappings()$countryToGCAMReg32)$region)
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



    if (is.null(rv$filedatax) & is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))) {

      data_raw_result <- argus::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value))

    return(data_raw_result)

    } else if(!is.null(rv$filedatax) & is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))) {

      data_raw_result <- NULL

      for (i in 1:length(input$filedata$datapath)){
        argus::parse_local(input$filedata$datapath[i], inpu$urlfiledata$datapath) %>%
            dplyr::select(scenario, subRegion, param, aggregate, class, x, value) -> data_raw_result_i
        data_raw_result_i<-argus::addMissing(data_raw_result_i)
        data_raw_result <- dplyr::bind_rows(data_raw_result, data_raw_result_i)
      }

      return(data_raw_result)

    } else if(is.null(rv$filedatax) & !is.null(rv$dataGCAM) & (is.null(rv$urlfiledatax))){

      data_raw_result <-argus::addMissing(rv$dataGCAM %>%
                                            dplyr::select(scenario, subRegion, param, aggregate, class, x, value))

      return(data_raw_result)

    } else {

      urlfiledatax_path <- strsplit(rv$urlfiledatax, ",")

      data_raw_result <- NULL

      for (i in 1:length(urlfiledatax_path[[1]])){
        argus::parse_remote(gsub(" ", "", urlfiledatax_path[[1]][i], fixed = TRUE))%>%
            dplyr::select(scenario, subRegion, param, aggregate, class, x, value) -> data_raw_result_i

        data_raw_result <- dplyr::bind_rows(data_raw_result, data_raw_result_i)
      }
      return(data_raw_result)
    }

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
    rv$filedatax<-NULL
    rv$dataGCAM<-NULL
    rv$urlfiledatax<-NULL
    removeModal()
  }, ignoreInit = TRUE)


  data <- reactive({
    return(rv$data)
  })

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

    # Reactive year select Select based on inputs
    mapYearx <- function(){
      if(!is.null(isolate(input$mapYear))){
        return(isolate(input$mapYear))
      }else{
        return(sort(unique(dataMapx()$x))[round(length(sort(unique(dataMapx()$x)))/2)])
      }
    }

    # Select Years for Map
    output$selectMapYear = renderUI({
      sliderInput("mapYear", label ="Year", min = min(dataMapx()$x),
                  max = max(dataMapx()$x), step = 5,
                  value=selectFocusMapYearx(), sep="",
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

    #Bookmark modal
    observeEvent(input$button_subset_regions, {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          footer = NULL,
          pickerInput(
            inputId = "subsetRegions",
            label = "Select Regions to Compare",
            choices = unique(dataMapx()$subRegion),
            selected = unique(regionsSelectedx())[1:4],
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "None",
              `select-all-text` = "All",
              `none-selected-text` = "None Selected"
            ))
        )
      )
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
        return(input$focusMapScenarioSelected)
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

    selectFocusMapYearx <-  function(){
     if (is.null(isolate(input$focusMapYearSelected))){
        return(sort(unique(dataMapx()$x))[round(length(sort(unique(dataMapx()$x)))/2)])
      } else{
        return(isolate(input$focusMapYearSelected))
      }
    }

    # Select Years for Map
    output$selectFocusMapYear = renderUI({
      sliderInput(inputId = "focusMapYearSelected",
                  label ="Year",
                  min = min(data()$x),
                  max = max(data()$x), step = 5,
                  value=selectFocusMapYearx(),
                  sep="",
                  animate =T)
    })

  } # Select Reactive Inputs

  #...........................
  # Subsetting Data for Outputs
  #...........................

  if(T){ # Subsetting Data for Outputs

    dataSumx <- reactive({
      # Aggregate across classes
      tblAggsums <- data() %>%
        dplyr::filter(subRegion %in% regionsSelectedx()) %>%
        dplyr::filter(scenario %in% scenariosSelectedx(),
                      param %in% paramsSelectedx()) %>%
        dplyr::mutate(scenario = as.character(scenario)) %>%
        dplyr::filter(aggregate == "sum") %>%
        dplyr::select(scenario, param, x, value) %>%
        dplyr::group_by_at(dplyr::vars(-value)) %>%
        dplyr::summarize_at(c("value"), list( ~ sum(.)))
      tblAggmeans <- data() %>%
        dplyr::filter(subRegion %in% regionsSelectedx()) %>%
        dplyr::filter(scenario %in% scenariosSelectedx(),
                      param %in% paramsSelectedx()) %>%
        dplyr::select(-class) %>%
        dplyr::mutate(scenario = as.character(scenario)) %>%
        dplyr::filter(aggregate == "mean") %>%
        dplyr::select(scenario, param, x, value) %>%
        dplyr::group_by_at(dplyr::vars(-value)) %>%
        dplyr::summarize_at(c("value"), list( ~ mean(.)))

      dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
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
      tbl_pd
    })


    # Map Data

    dataMapx <- reactive({

          # Aggregate across classes
          tblAggsums <- data() %>%
            dplyr::filter(subRegion %in% regionsSelectedx()) %>%
            dplyr::filter(scenario %in% input$scenariosSelected,
                          param %in% paramsSelectedx()) %>%
            dplyr::mutate(subRegion = gsub("-","_",subRegion)) %>%
            dplyr::mutate(scenario = as.character(scenario)) %>%
            dplyr::filter(aggregate == "sum") %>%
            dplyr::select(scenario, param, subRegion, x, value) %>%
            dplyr::group_by_at(dplyr::vars(-value)) %>%
            dplyr::summarize_at(c("value"), list( ~ sum(.)))

          tblAggmeans <- data() %>%
            dplyr::filter(subRegion %in% regionsSelectedx()) %>%
            dplyr::filter(scenario %in% input$scenariosSelected,
                          param %in% paramsSelectedx()) %>%
            dplyr::mutate(subRegion = gsub("-","_",subRegion)) %>%
            dplyr::select(-class) %>%
            dplyr::mutate(scenario = as.character(scenario)) %>%
            dplyr::filter(aggregate == "mean") %>%
            dplyr::select(scenario, param, subRegion, x, value) %>%
            dplyr::group_by_at(dplyr::vars(-value)) %>%
            dplyr::summarize_at(c("value"), list( ~ mean(.)))

        dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

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

  #...........................
  # Focus Page
  #...........................

    if(T){ # Focus Page

  output$focusMap <- renderLeaflet({

    # Read in Raw Data
    dataMapFocus_raw <-
        dataMapx() %>%
          dplyr::ungroup() %>%
          dplyr::select(x,param,scenario,subRegion,value) %>%
          filter(param == focusMapParamSelectedx(),
                 scenario == input$focusMapScenarioSelected,
                 x == input$focusMapYearSelected) %>%
            dplyr::left_join(rmap::mappings("mappingGCAMBasins"),by="subRegion") %>%
            dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                                     TRUE~subRegion)) %>%
            dplyr::select(-subRegionMap) %>%
          dplyr::filter(subRegion!="South_Pacific_Islands")

      if(length(dataMapFocus_raw$x)==0){
        my_title <- tags$p(tags$style("p {color: black; font-size:22px}"),tags$b("There is no data for this year"))

        mapFocus <- leaflet() %>%
        # setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addTiles()%>%
          addControl(my_title, position = "topleft" )
        return(mapFocus)
      }

    mapdf <- rmap::map_find(dataMapFocus_raw)$subRegShapeFound;

    # Prepare for Polygons
    mapdf <- mapdf[mapdf$subRegion %in% dataMapFocus_raw$subRegion,]; mapdf
    mapdf@data <- mapdf@data %>%
      left_join(dataMapFocus_raw %>%
                  dplyr::select(subRegion,value)) %>%
      filter(subRegion %in% unique(dataMapFocus_raw$subRegion)) %>%
      droplevels(); mapdf

    # Create legends and color scales
    bins <- unique(argus::breaks(dataMapFocus_raw,breaks=7)[[1]]);
    pal <- colorBin(grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(9,length(bins)), "YlOrRd"))(length(bins)),
                    domain = dataMapFocus_raw$value, bins = bins)

    # Plot polygons on Leaflet
      labels <- sprintf(
        "<strong>%s</strong><br/>%g",
        mapdf@data$subRegion, mapdf@data$value
      ) %>% lapply(htmltools::HTML)

      if(length(bins)>1){

        coords <- coordinates(mapdf)

        lat_min = min(coords[,2])
        lat_max = max(coords[,2])
        lng_min = min(coords[,1])
        lng_max = max(coords[,1])

        initial_lat = (lat_max + lat_min )/2
        initial_lng = (lng_max + lng_min)/2
        initial_zoom = 2

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
                    position = "bottomright")
      }


    mapFocus

    })

  output$focusChartSum <- renderPlotly({

    #ggplotly()
    (ggplot2::ggplot(dataSumx() %>%
                       dplyr::select(scenario, value, param, x)%>%
                       dplyr::filter(param == focusMapParamSelectedx()),
                     aes(x=x,y=value,
                         group=scenario,
                         color=scenario))+
       geom_line(size=1.25) +
       ggplottheme +
       geom_line() +
       geom_point() +
       xlab(NULL) + ylab(NULL) +
       ggtitle(paste0(focusMapParamSelectedx())) +
       theme(legend.position="bottom",
             legend.title = element_blank(),
             legend.margin=margin(t =0, r = 0, b = 0, l =0, "pt"),
             plot.margin=margin(t =0, r = 0, b = 0, l =0,"pt"),
             text=element_text(size=12),
             aspect.ratio = NULL,
             plot.title = element_text(size =10)))%>%
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
      dplyr::filter(param == focusMapParamSelectedx(),
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

    #ggplotly()
    (ggplot2::ggplot(dataChartPlot,
                     aes(x=x,y=value,
                         group=scenario,
                         fill=class))+
       ggplottheme +
       ggtitle(paste0(input$focusMapScenarioSelected)) +
       scale_fill_manual(breaks=names(palCharts),values=palCharts) +
       scale_y_continuous(position = "left")+
       geom_bar(position="stack", stat="identity") +
       theme(legend.position="bottom",
              strip.text.y = element_blank(),
              legend.title = element_blank(),
              legend.margin=margin(t =0, r = 0, b = 0, l =0, "pt"),
              legend.key.height=unit(0, "cm"),
              text = element_text(size = 12),
              plot.margin=margin(t =0, r = 0, b = 0, l =0,"pt"),
              plot.title = element_text(size =10)) +
      ylab(NULL) + xlab(NULL))%>%
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

      #...........................
      # Map Plot Abs
      #...........................

      output$mapAbs <- renderPlot({


        print("==============")
        print(paste0(unique(dataMapx()$param)))

        withProgress(message = 'Rendering map...', value = 0, {
        argus::plotMap(mapData = dataMapx(),
                       mapX = input$mapYear,
                       diff=NULL)
        })

      },
      height=function(){400*length(unique((dataMapx() %>% dplyr::filter(x == input$mapYear))$param))}
      )

      #...........................
      # Map Plot Diff Abs
      #...........................

      output$mapDiffAbs <- renderPlot({

        withProgress(message = 'Rendering map...', value = 0, {
        argus::plotMap(mapData = dataMapx(),
                       mapX = input$mapYear,
                       scenRef = input$scenarioRefSelected ,
                       diff="absolute")
        })

      },
      height=function(){400*length(unique((dataMapx() %>% dplyr::filter(x == input$mapYear))$param))}
      )

      #...........................
      # Map Plot Diff Percent
      #...........................


     output$mapDiffPrcnt <- renderPlot({

       withProgress(message = 'Rendering map...', value = 0, {
       argus::plotMap(mapData = dataMapx(),
                      mapX = input$mapYear,
                      scenRef = input$scenarioRefSelected ,
                      diff="percent")
       })

     },
     height=function(){400*length(unique((dataMapx() %>% dplyr::filter(x == input$mapYear))$param))}
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
      zip::zip(zipfile=file, files=fs)
    }
  )

  } # Outputs

} # Close Server

