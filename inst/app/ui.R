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
library(shinyBS)

#---------------------------
# ui object
#---------------------------

ui <- function(request) { fluidPage(

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

  tags$script("
    Shiny.addCustomMessageHandler('openlink', function(value) {
    window.open(value, '_blank');
    });
  "),
  tags$script("
    Shiny.addCustomMessageHandler('setsetting', function(value) {
    console.log(value);
    Shiny.setInputValue(value[0], value.slice(1,value.length));
    });
  "),

  #---------------------------
  #Dropdown Button
  #---------------------------

  div(id = "preload",
      dropdownButton(
        inputId = "Preloadbutton",
        label="Preload",
        circle = FALSE,
        up=FALSE,
        right=TRUE,
        uiOutput('examplesPreloadInput'),
        status="preloadButton"
      ),style="float:right;"
  ),


  #---------------------------
  # Side Bar with dropdown
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
                 div(align="center",
                     bsCollapse(id = "collapse0", multiple = FALSE,
                                bsCollapsePanel("Data Story", style = "info",
                                                actionButton(style="position:absolute; left:40px",
                                                             inputId='compregstoryboardtoggle',
                                                             label='Edit',
                                                             class = "download-button", icon = icon("edit","fa-1x")),
                                                textOutput("compregstoryboardtexttitle"),
                                                tags$style("#compregstoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                                verbatimTextOutput("compregstoryboardtext", placeholder = TRUE),
                                                tags$style(type="text/css","#compregstoryboardtext {white-space:pre-wrap;
                                                           text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                                                      text-align: left;width;100%;height:10vh;display: 'inline-block'}")

                                ))),
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
                                    div(downloadButton(
                                            'downloadPlotSum',NULL,download = "summaryChart.png",
                                            class = "download_button_in"),style="float:right"),
                                br(),
                                div(align="center",
                                    bsCollapse(id = "collapse1", multiple = FALSE,
                                           bsCollapsePanel("Data Story", style = "info",
                                                           actionButton(style="position:absolute; left:40px",
                                                                        inputId='linestoryboardtoggle',
                                                                        label='Edit',
                                                                        class = "download-button", icon = icon("edit","fa-1x")),
                                                           textOutput("linestoryboardtexttitle"),
                                                           tags$style("#linestoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                                           verbatimTextOutput("linestoryboardtext", placeholder = TRUE),
                                                           tags$style(type="text/css","#linestoryboardtext {white-space:pre-wrap;
                                                           text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                                                      text-align: left;width;100%;height:10vh;display: 'inline-block'}")

                                           ))),
                                    plotOutput(outputId = "summary")),
                       tabPanel("Compare Regions",
                                div(downloadButton('downloadPlotSumReg',NULL,download = "summaryChartReg.png",
                                               class = "download_button_in"),style="float:right"),
                                br(),
                                div(actionButton("button_subset_regions", "Choose Regions to Compare",class = "update_button"),
                                             align="left"),
                                # div(align="center",
                                #     bsCollapse(id = "collapse2", multiple = FALSE,
                                #                bsCollapsePanel("Data Story", style = "info",
                                #                                actionButton(style="position:absolute; left:40px",
                                #                                             inputId='compregstoryboardtoggle',
                                #                                             label='Edit',
                                #                                             class = "download-button", icon = icon("edit","fa-1x")),
                                #                                textOutput("compregstoryboardtexttitle"),
                                #                                tags$style("#compregstoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                #                                verbatimTextOutput("compregstoryboardtext", placeholder = TRUE),
                                #                                tags$style(type="text/css","#compregstoryboardtext {white-space:pre-wrap;
                                #                            text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                #                                       text-align: left;width;100%;height:10vh;display: 'inline-block'}")
                                #
                                #                ))),
                                plotOutput(outputId = "summaryReg"))
                  )
                 )
             ),
                  #---------------------------
                  # Main Panel: Charts
                  #---------------------------
                  tabPanel("Charts",
                           div(align="center",
                                tabsetPanel(type = "tabs",
                                      tabPanel("Absolute Value",
                                               div(downloadButton('downloadPlotChart',NULL,download = "barCharts.png",  class = "download_button_in"),
                                                   style = "float: right"),
                                               br(),
                                               div(align="center",
                                                   bsCollapse(id = "collapse3", multiple = FALSE,
                                                              bsCollapsePanel("Data Story", style = "info",
                                                                              actionButton(style="position:absolute; left:40px",
                                                                                           inputId='absvalstoryboardtoggle',
                                                                                           label='Edit',
                                                                                           class = "download-button", icon = icon("edit","fa-1x")),
                                                                              textOutput("absvalstoryboardtexttitle"),
                                                                              tags$style("#absvalstoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                                                              verbatimTextOutput("absvalstoryboardtext", placeholder = TRUE),
                                                                              tags$style(type="text/css","#absvalstoryboardtext {white-space:pre-wrap;
                                                           text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                                                      text-align: left;width;100%;height:10vh;display: 'inline-block'}")

                                                              ))),
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right")),
                                                 # column(1, div(downloadButton('downloadPlotChart',NULL,download = "barCharts.png",  class = "download_button"),
                                                 #               style = "float: right"))
                                                 ),
                                               div(class="charts",plotOutput(outputId = "plotAbs", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Absolute Difference",
                                               br(),
                                               div(align="center",
                                                   bsCollapse(id = "collapse4", multiple = FALSE,
                                                              bsCollapsePanel("Data Story", style = "info",
                                                                              actionButton(style="position:absolute; left:40px",
                                                                                           inputId='absdifstoryboardtoggle',
                                                                                           label='Edit',
                                                                                           class = "download-button", icon = icon("edit","fa-1x")),
                                                                              textOutput("absdifstoryboardtexttitle"),
                                                                              tags$style("#absdifstoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                                                              verbatimTextOutput("absdifstoryboardtext", placeholder = TRUE),
                                                                              tags$style(type="text/css","#absdifstoryboardtext {white-space:pre-wrap;
                                                           text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                                                      text-align: left;width;100%;height:10vh;display: 'inline-block'}")

                                                              ))),
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right")),
                                                 column(1, div(downloadButton('downloadPlotChart1',NULL,download = "barCharts.png",  class = "download_button_in"),
                                                               style = "float: right"))),

                                               div(class="charts",plotOutput(outputId = "plotDiff", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Percent Difference",
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right")),
                                                 column(1, div(downloadButton('downloadPlotChart2',NULL,download = "barCharts.png",  class = "download_button"),
                                                               style = "float: right"))),
                                               # div(align="center",
                                               #     bsCollapse(id = "collapse5", multiple = FALSE,
                                               #                bsCollapsePanel("Data Story", style = "info",
                                               #                                actionButton(style="position:absolute; left:40px",
                                               #                                             inputId='percdifstoryboardtoggle',
                                               #                                             label='Edit',
                                               #                                             class = "download-button", icon = icon("edit","fa-1x")),
                                               #                                textOutput("percdifstoryboardtexttitle"),
                                               #                                tags$style("#percdifstoryboardtexttitle {font-weight: bold; font-size: 30px}"),
                                               #                                verbatimTextOutput("percdifstoryboardtext", placeholder = TRUE),
                                               #                                tags$style(type="text/css","#percdifstoryboardtext {white-space:pre-wrap;
                                               #             text-size:20px; background-color:rgba(0,0,0,0) ; border-color:rgba(0,0,0,0);
                                               #                        text-align: left;width;100%;height:10vh;display: 'inline-block'}")
                                               #
                                               #                ))),
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
                                        column(4,
                                               div(br(),pickerInput(
                                          inputId = "mapLegend",
                                          label = "Legend Type",
                                          choices = c("kmean","pretty"),
                                          selected = "kmean",
                                          multiple = F))),
                                    column(1,
                                           br(),
                                           div(
                                             style="float:right",
                                             # dropdownButton(
                                             #   label="Storyboard",
                                             #   circle = FALSE,
                                             #   up=FALSE,
                                             #   right=TRUE,
                                             #   width = "30vw",
                                             #   textAreaInput(inputId="mapstoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                                             # ),
                                             # actionButton(inputId = "mapstoryboardtoggle", label = "Story Board")
                                           )),
                                        column(1, div(br(),downloadButton('downloadMap',NULL,download = "map.png",
                                                         class = "download_button"),style="float:right"))),
                           div(align="center",
                               tabsetPanel(type="tabs",
                                           tabPanel("Absolute Value",
                                                    div(class="charts",
                                                        plotOutput(outputId = "mapAbs", width = "100%", height="100%"),
                                                        style = "margin-right: 20px;margin-left: 20px;")
                                                    ),
                                           tabPanel("Absolute Difference",
                                                    div(class="charts",
                                                        plotOutput(outputId = "mapDiffAbs", width = "100%", height="100%"),
                                                        style = "margin-right: 20px;margin-left: 20px;")
                                                    ),
                                          tabPanel("Percent Difference",
                                                   div(class="charts",
                                                       plotOutput(outputId = "mapDiffPrcnt", width = "100%", height="100%"),
                                                       style = "margin-right: 20px;margin-left: 20px;")
                                                   )
                                          )
                               )
                           ),
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

    div(actionButton(inputId="toggleSidebar", label="Inputs", icon = icon("caret-up","fa-1x"),class = "download_button_input")),
    div(downloadButton('downloadAll', "All",  class = "download_button")),
    div(actionButton(inputId="toggleSidebar", label="Inputs", icon = icon("caret-up","fa-1x"),class = "download_button_input")),
    div(actionLink(inputId='github', label='',class = "icon", icon = icon("github","fa-1x"),onclick ="window.open('https://github.com/JGCRI/argus', '_blank')")),
    div(actionLink(inputId='help', label='', class = "icon",icon = icon("question","fa-1x"),onclick ="window.open('https://jgcri.github.io/argus/', '_blank')")),
    div(actionLink(inputId='loadbookmark', label='', class = "icon", icon = icon("bookmark","fa-1x"))),
    div(actionLink(inputId='togglepreload', label='', class="download_button")),
    tags$script(HTML("var header = $('.navbar> .container-fluid');
                   header.append($('#toggleSidebar'));
                   header.append($('#downloadAll'));
                   header.append($('#preload'));
                   header.append($('#help'));
                   header.append($('#github'));
                   header.append($('#loadsetting'));
                   header.append($('#loadbookmark'));
                   console.log(header)"))
  )

)

}
