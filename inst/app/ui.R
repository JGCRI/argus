#' ui

#---------------------------
# Libraries Needed (Also add to DESCRIPTION)
#---------------------------
library(shiny)
library(shinyFiles)
library(shinythemes)
library(leaflet)
library(DT)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(plotly)

#---------------------------
# ui object
#---------------------------

ui <- fluidPage(

  #---------------------------
  # Themes
  #---------------------------

  #shinythemes::themeSelector(),
  #theme = shinythemes::shinytheme("spacelab"),

  #---------------------------
  # CSS/html
  #---------------------------

  useShinyjs(),
  useShinyalert(),

  theme ="styles.css",
  # Hide Shiny Errors
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  tags$script("
    Shiny.addCustomMessageHandler('rhm_clic', function(value) {
    Shiny.setInputValue('regionsSelected', value);
    });
  "),


  #---------------------------
  # Side Bar
  #---------------------------

  div(id = "Sidebar",

      absolutePanel(class="floatNav",
                    fixed = TRUE,
                    draggable = FALSE,
                    top = 60,
                    left = "auto",
                    right = 15,
                    bottom = "auto",
                    width = 330,
                    height = "auto",

      selectInput(
        inputId = "inputz",
        label = "Input",
        choices = c("url","csv","gcam", ""),
        selected = "",
        multiple = FALSE,
        selectize = TRUE,
        width = "100%"),
      # Reactive Input Choices Based on Input File-------------------------

      # Scenarios
      uiOutput('selectScenarios'),
      # Ref Scenarios
      uiOutput('selectRefScenarios'),
      # Params
      uiOutput('selectParams'),
      # Regions
      uiOutput('selectRegions'),
      # div(id="map", class="maps")
      # leafletOutput(outputId = "mymap")
    )),


    #---------------------------
    # Main Panel
    #---------------------------

  navbarPage(
    title = "ARGUS",
    position = c("fixed-top"),
    collapsible = FALSE,
    fluid = T,

                  #---------------------------
                  # Main Panel: Home Tab
                  #---------------------------
                  tabPanel("Focus",
                           fluidRow(column(3,uiOutput('selectFocusMapYear')),
                                    column(2,br(),uiOutput('selectFocusMapParam')),
                                    column(2,br(),uiOutput('selectFocusMapScenario'))
                                    ),
                           fluidRow(
                             column(8,div(class="window",leafletOutput(outputId = "focusMap", height=800))),
                             column(4,
                                    plotlyOutput(outputId = "focusChartSum", height=350),
                                    plotlyOutput(outputId = "focusChartBar", height=450))
                           )


                    ),

                  #---------------------------
                  # Main Panel: Summary Tab
                  #---------------------------
    tabPanel("Lines",
             div(align="center",
                  tabsetPanel(type="tabs",
                       tabPanel("All",
                                    fluidRow(
                                      column(6, downloadButton(
                                            'downloadPlotSum',NULL,download = "summaryChart.png",
                                            class = "download_button_in"),style="float:right")),
                                    plotOutput(outputId = "summary")),
                       tabPanel("Compare Regions",
                                fluidRow(
                                  column(6,div(uiOutput('subsetRegions'))),
                                  column(6, downloadButton('downloadPlotSumReg',NULL,download = "summaryChartReg.png",
                                      class = "download_button_in"),style="float:right")),
                                plotOutput(outputId = "summaryReg"))
                  )
                 )
             ),
                  #---------------------------
                  # Main Panel: Charts
                  #---------------------------
                  tabPanel("Charts",
                           fluidRow(column(9),
                             column(3, div(downloadButton('downloadPlotChart',NULL,download = "barCharts.png",  class = "download_button"),
                                           style = "float: right"))),
                           div(align="center",
                                tabsetPanel(type = "tabs",
                                      tabPanel("Absolute Value",
                                               div(class="charts",plotOutput(outputId = "plotAbs", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Absolute Difference",
                                               div(class="charts",plotOutput(outputId = "plotDiff", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Percent Difference",
                                               div(class="charts",plotOutput(outputId = "plotPerc", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      )
                                )
                           )
                  ),
                  #---------------------------
                  # Main Panel: Maps Tab
                  #---------------------------
                  tabPanel("Maps",
                           fluidRow(column(6,div(uiOutput('selectMapYear')),style = "float: left"),
                                        column(5,div(br(),pickerInput(
                                          inputId = "mapLegend",
                                          label = "Legend Type",
                                          choices = c("kmean","pretty"),
                                          selected = "kmean",
                                          multiple = F))),
                                        column(1, div(br(),downloadButton('downloadMap',NULL,download = "map.png",
                                                         class = "download_button"),style="float:right"))),
                           tabsetPanel(type="tabs",
                                           tabPanel("Absolute Value"
                                                    ),
                                           tabPanel("Absolute Difference"
                                                   ),
                                          tabPanel("Percent Difference"
                                                   )
                               )),
                  #---------------------------
                  # Main Panel: Table Tab
                  #---------------------------
                  tabPanel(
                    "Table",
                    fluidRow(
                      column(12, div(downloadButton('downloadTable', NULL, download = "table.csv", class="download_button"),
                                     style = "float: right"))),
                    div(class="charts", DTOutput(outputId = "table"))
                  ),


    #---------------------------
    # NavBar buttons
    #---------------------------
    div(downloadButton('downloadAll', "All",  class = "download_button")),
    div(actionButton(inputId="toggleSidebar", label="Inputs", icon = icon("caret-up","fa-1x"),class = "download_button_input")),
    div(actionLink(inputId='github', label='',class = "icon", icon = icon("github","fa-1x"),onclick ="window.open('https://github.com/JGCRI/argus', '_blank')")),
    div(actionLink(inputId='help', label='', class = "icon",icon = icon("question","fa-1x"),onclick ="window.open('https://jgcri.github.io/argus/', '_blank')")),
    div(actionLink(inputId='loadbookmark', label='', class = "icon", icon = icon("bookmark","fa-1x"))),
    tags$script(HTML("var header = $('.navbar> .container-fluid');
                   header.append($('#toggleSidebar'));
                   header.append($('#downloadAll'));
                   header.append($('#help'));
                   header.append($('#github'));
                   header.append($('#loadsetting'));
                   header.append($('#loadbookmark'));
                   console.log(header)"))
  )

)

