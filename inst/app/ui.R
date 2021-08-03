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
        label="Preload",
        circle = FALSE,
        up=FALSE,
        right=TRUE,
        uiOutput('examplesPreloadInput')
      ),
      class="dropdown_button"
  ),


  #---------------------------
  # Side Bar with dropdown
  #---------------------------

  div(id = "Sidebar",

      dropdownButton(
        inputId="inputx",
        label="Input",
        circle = FALSE,
        up=FALSE,
        right=TRUE,
        div(
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
            uiOutput('selectRegions')
            # div(id="map", class="maps")
            # leafletOutput(outputId = "mymap")
            )
      ),
      class="dropdown_button"
    ),


  #---------------------------
  # Side Bar
  #---------------------------

  # div(id = "Sidebar",
  #
  #
  #
  #     absolutePanel(class="floatNav",
  #                   fixed = TRUE,
  #                   draggable = FALSE,
  #                   top = 60,
  #                   left = "auto",
  #                   right = 15,
  #                   bottom = "auto",
  #                   width = 330,
  #                   height = "auto",
  #
  #     selectInput(
  #       inputId = "inputz",
  #       label = "Input",
  #       choices = c("url","csv","gcam", ""),
  #       selected = "",
  #       multiple = FALSE,
  #       selectize = TRUE,
  #       width = "100%"),
  #     # Reactive Input Choices Based on Input File-------------------------
  #
  #     # Scenarios
  #     uiOutput('selectScenarios'),
  #     # Ref Scenarios
  #     uiOutput('selectRefScenarios'),
  #     # Params
  #     uiOutput('selectParams'),
  #     # Regions
  #     uiOutput('selectRegions'),
  #     # div(id="map", class="maps")
  #     # leafletOutput(outputId = "mymap")
  #   )),


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
                 # dropdownButton(
                 #   label="Storyboard",
                 #   circle = FALSE,
                 #   up=FALSE,
                 #   right=TRUE,
                 #   width = "30vw",
                 #   textAreaInput(inputId="linestoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                 # )
                  tabsetPanel(type="tabs",
                       tabPanel("All",
                                    fluidRow(
                                      column(11),
                                      column(1, downloadButton(
                                            'downloadPlotSum',NULL,download = "summaryChart.png",
                                            class = "download_button_in"),style="float:left")
                                      ),
                                bsCollapse(id = "collapse", multiple = FALSE,
                                           bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                           div(actionButton(style="float:right;", inputId='linestoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                           br(),
                                                           # div(
                                                           #   height="10vh",
                                                           #   width = "100%",
                                                           #   style="float:left;",
                                                           #   textOutput("linestoryboardtexttitle",  inline = TRUE),
                                                           #   tags$style(type="text/css","#linestoryboardtexttitle {width:auto;text-align: center;}")
                                                           # ),
                                                           #verbatimTextOutput("linestoryboardtexttitle",  placeholder  = TRUE),
                                                           #tags$style(type="text/css","#linestoryboardtexttitle {width:auto;text-align: center;}"),
                                                           uiOutput("linestoryboardtexttitle"),
                                                           br(),
                                                           # div(
                                                           #   height="10vh",
                                                           #   #width = "100%",
                                                           #   style="float:left;",
                                                           #   verbatimTextOutput("linestoryboardtext", placeholder = TRUE),
                                                           #   tags$style(type="text/css","#linestoryboardtext {white-space: pre-wrap;text-align: left;width;100%;display: 'inline-block'}")
                                                           # ),
                                                           verbatimTextOutput("linestoryboardtext", placeholder = TRUE),
                                                           tags$style(type="text/css","#linestoryboardtext {white-space: pre-wrap;text-align: left;width;100%;height:10vh;display: 'inline-block'}")

                                           )),
                                    plotOutput(outputId = "summary")),
                       tabPanel("Compare Regions",
                                fluidRow(
                                  column(10,div(uiOutput('subsetRegions'),style="float:left;")),
                                  column(1,
                                         div(
                                           style="float:right;",
                                           # dropdownButton(
                                           #   label="Storyboard",
                                           #   circle = FALSE,
                                           #   up=FALSE,
                                           #   right=TRUE,
                                           #   width = "30vw",
                                           #   textAreaInput(inputId="compregstoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                                           #
                                           # )
                                           #actionButton(inputId = "compregstoryboardtoggle", label = "Story Board")
                                         )
                                  ),
                                  column(1, downloadButton('downloadPlotSumReg',NULL,download = "summaryChartReg.png",
                                      class = "download_button_in"),style="float:right")),
                                bsCollapse(id = "collapse", multiple = FALSE,
                                           bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                           div(actionButton(style="float:right;", inputId='compregstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                           br(),
                                                           # div(
                                                           #   height="10vh",
                                                           #   style="float:left;",
                                                           #   verbatimTextOutput("compregstoryboardtitle",  inline = TRUE),
                                                           #   tags$style(type="text/css","#compregstoryboardtitle {width:auto;text-align: center;}")
                                                           # ),
                                                           #verbatimTextOutput("compregstoryboardtitle",  placeholder = TRUE),
                                                           #tags$style(type="text/css","#compregstoryboardtitle {width:auto;text-align: center;}"),
                                                           uiOutput("compregstoryboardtitle"),
                                                           br(),
                                                           # div(
                                                           #   height="10vh",
                                                           #   style="float:left;",
                                                           #   verbatimTextOutput("compregstoryboardtext", placeholder = TRUE),
                                                           #   tags$style(type="text/css","#compregstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                           # )
                                                           verbatimTextOutput("compregstoryboardtext", placeholder = TRUE),
                                                           tags$style(type="text/css","#compregstoryboardtext {white-space: pre-wrap;text-align: left;width:auto;height:10vh;}")

                                           )),
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
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right",
                                                          # dropdownButton(
                                                          #   label="Storyboard",
                                                          #   circle = FALSE,
                                                          #   up=FALSE,
                                                          #   right=TRUE,
                                                          #   width = "30vw",
                                                          #   textAreaInput(inputId="absvalstoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                                                          # )
                                                          #actionButton(inputId = "absvalstoryboardtoggle", label = "Story Board")
                                                        )),
                                                 column(1, div(downloadButton('downloadPlotChart',NULL,download = "barCharts.png",  class = "download_button"),
                                                               style = "float: right"))),
                                               bsCollapse(id = "collapse", multiple = FALSE,
                                                          bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                          div(actionButton(style="float:right;", inputId='absvalstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                          br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   verbatimTextOutput("absvalstoryboardtitle",  inline = TRUE),
                                                                          #   tags$style(type="text/css","#absvalstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                          # ),
                                                                          #verbatimTextOutput("absvalstoryboardtitle",  placeholder = TRUE),
                                                                          #tags$style(type="text/css","#absvalstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                          uiOutput("absvalstoryboardtitle"),
                                                                          br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   verbatimTextOutput("absvalstoryboardtext", placeholder = TRUE),
                                                                          #   tags$style(type="text/css","#absvalstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                          # )
                                                                          verbatimTextOutput("absvalstoryboardtext", placeholder = TRUE),
                                                                          tags$style(type="text/css","#absvalstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")

                                                          )),
                                               div(class="charts",plotOutput(outputId = "plotAbs", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Absolute Difference",
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right",
                                                          # dropdownButton(
                                                          #   label="Storyboard",
                                                          #   circle = FALSE,
                                                          #   up=FALSE,
                                                          #   right=TRUE,
                                                          #   width = "30vw",
                                                          #   textAreaInput(inputId="absdifstoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                                                          # )
                                                          #actionButton(inputId = "absdifstoryboardtoggle", label = "Story Board")
                                                        )),
                                                 column(1, div(downloadButton('downloadPlotChart1',NULL,download = "barCharts.png",  class = "download_button"),
                                                               style = "float: right"))),
                                               bsCollapse(id = "collapse", multiple = FALSE,
                                                          bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                          div(actionButton(style="float:right;", inputId='absdifstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                          br(),
                                                                          #br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   textOutput("absdifstoryboardtitle",  inline = TRUE),
                                                                          #   tags$style(type="text/css","#absdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                          # ),
                                                                          #verbatimTextOutput("absdifstoryboardtitle",  placeholder = TRUE),
                                                                          #tags$style(type="text/css","#absdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                          uiOutput("absdifstoryboardtitle"),
                                                                          br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   verbatimTextOutput("absdifstoryboardtext", placeholder = TRUE),
                                                                          #   tags$style(type="text/css","#absdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                          # )
                                                                          verbatimTextOutput("absdifstoryboardtext", placeholder = TRUE),
                                                                          tags$style(type="text/css","#absdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")

                                                          )),
                                               div(class="charts",plotOutput(outputId = "plotDiff", width = "100%", height="100%"), style = "margin-right: 20px;margin-left: 20px;")
                                      ),
                                      tabPanel("Percent Difference",
                                               fluidRow(
                                                 column(10),
                                                 column(1,
                                                        div(
                                                          style="float:right",
                                                          # dropdownButton(
                                                          #   label="Storyboard",
                                                          #   circle = FALSE,
                                                          #   up=FALSE,
                                                          #   right=TRUE,
                                                          #   width = "30vw",
                                                          #   textAreaInput(inputId="percdifstoryboard",label="Story Board", width = "100%", height="50vh", resize="vertical")
                                                          # )
                                                          #actionButton(inputId = "percdifstoryboardtoggle", label = "Story Board")
                                                        )),
                                                 column(1, div(downloadButton('downloadPlotChart2',NULL,download = "barCharts.png",  class = "download_button"),
                                                               style = "float: right"))),
                                               bsCollapse(id = "collapse", multiple = FALSE,
                                                          bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                          div(actionButton(style="float:right;", inputId='percdifstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                          br(),
                                                                          #br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   textOutput("percdifstoryboardtitle",  inline = TRUE),
                                                                          #   tags$style(type="text/css","#percdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                          # ),
                                                                          #verbatimTextOutput("percdifstoryboardtitle",  placeholder  = TRUE),
                                                                          #tags$style(type="text/css","#percdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                          uiOutput("percdifstoryboardtitle"),
                                                                          br(),
                                                                          # div(
                                                                          #   height="10vh",
                                                                          #   style="float:left;",
                                                                          #   verbatimTextOutput("percdifstoryboardtext", placeholder = TRUE),
                                                                          #   tags$style(type="text/css","#percdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                          # )
                                                                          verbatimTextOutput("percdifstoryboardtext", placeholder = TRUE),
                                                                          tags$style(type="text/css","#percdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")

                                                          )),
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
                                             # )
                                             #actionButton(inputId = "mapstoryboardtoggle", label = "Story Board")
                                           )),
                                        column(1, div(br(),downloadButton('downloadMap',NULL,download = "map.png",
                                                         class = "download_button"),style="float:left"))),
                           tabsetPanel(type="tabs",
                                           tabPanel("Absolute Value",
                                                    br(),
                                                    div(align="center",
                                                    bsCollapse(id = "collapse", multiple = FALSE,
                                                      bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                    div(actionButton(style="float:right;", inputId='mapabsstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                    br(),
                                                                    #br(),
                                                                    # div(
                                                                    #   height="10vh",
                                                                    #   style="float:left;",
                                                                    #   textOutput("mapabsstoryboardtitle",  inline = TRUE),
                                                                    #   tags$style(type="text/css","#mapabsstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                    # ),
                                                                    #verbatimTextOutput("mapabsstoryboardtitle",  placeholder = TRUE),
                                                                    #tags$style(type="text/css","#mapabsstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                    uiOutput("mapabsstoryboardtitle"),
                                                                    br(),
                                                                    # div(
                                                                    #   height="10vh",
                                                                    #   style="float:left;",
                                                                    #   verbatimTextOutput("mapabsstoryboardtext", placeholder = TRUE),
                                                                    #   tags$style(type="text/css","mapabsstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                    # )
                                                                    verbatimTextOutput("mapabsstoryboardtext", placeholder = TRUE),
                                                                    tags$style(type="text/css","#mapabsstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")
                                                                    ))),
                                                    div(
                                                    h2("Argus Maps are under development and will be released soon!"),
                                                    style="margin-top:100px")
                                                    ),
                                           tabPanel("Absolute Difference",
                                                    div(align="center",
                                                        br(),
                                                    bsCollapse(id = "collapse", multiple = FALSE,
                                                               bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                               div(actionButton(style="float:right;", inputId='mapabsdifstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                               br(),
                                                                               #br(),
                                                                               # div(
                                                                               #   height="10vh",
                                                                               #   style="float:left;",
                                                                               #   textOutput("mapabsdifstoryboardtitle",  inline = TRUE),
                                                                               #   tags$style(type="text/css","#mapabsdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                               # ),
                                                                               uiOutput("mapabsdifstoryboardtitle"),
                                                                               #verbatimTextOutput("mapabsdifstoryboardtitle",  placeholder = TRUE),
                                                                               #tags$style(type="text/css","#mapabsdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                               br(),
                                                                               # div(
                                                                               #   height="10vh",
                                                                               #   style="float:left;",
                                                                               #   verbatimTextOutput("mapabsdifstoryboardtext", placeholder = TRUE),
                                                                               #   tags$style(type="text/css","#mapabsdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                               # )
                                                                               verbatimTextOutput("mapabsdifstoryboardtext", placeholder = TRUE),
                                                                               tags$style(type="text/css","#mapabsdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")

                                                                               ))),
                                                    div(
                                                      h2("Argus Maps are under development and will be released soon!"),
                                                      style="margin-top:100px")
                                                   ),
                                          tabPanel("Percent Difference",
                                                   div(align="center",
                                                       br(),
                                                   bsCollapse(id = "collapse", multiple = FALSE,
                                                              bsCollapsePanel("Storyboard",#<i class="fas "></i>
                                                                              div(actionButton(style="float:right;", inputId='mappercdifstoryboardtoggle', label='', class = "download-button", icon = icon("edit","fa-1x"))),
                                                                              br(),
                                                                              #br(),
                                                                              # div(
                                                                              #   height="10vh",
                                                                              #   style="float:left;",
                                                                              #   textOutput("mappercdifstoryboardtitle",  inline = TRUE),
                                                                              #   tags$style(type="text/css","#mappercdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}")
                                                                              # ),
                                                                              #verbatimTextOutput("mappercdifstoryboardtitle",  placeholder = TRUE),
                                                                              #tags$style(type="text/css","#mappercdifstoryboardtitle {width:auto;white-space: pre-wrap;text-align: left;}"),
                                                                              uiOutput("mappercdifstoryboardtitle"),
                                                                              br(),
                                                                              # div(
                                                                              #   height="10vh",
                                                                              #   style="float:left;",
                                                                              #   verbatimTextOutput("mappercdifstoryboardtext", placeholder = TRUE),
                                                                              #   tags$style(type="text/css","#mappercdifstor {width:auto;white-space: pre-wrap;text-align: left;width:94vw;}")
                                                                              # )
                                                                              verbatimTextOutput("mappercdifstoryboardtext", placeholder = TRUE),
                                                                              tags$style(type="text/css","#mappercdifstoryboardtext {width:auto;white-space: pre-wrap;text-align: left;height:10vh;}")
                                                                              ))),
                                                   div(
                                                     h2("Argus Maps are under development and will be released soon!"),
                                                     style="margin-top:100px")
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
    #div(actionButton(inputId="toggleSidebar", label="Inputs", icon = icon("caret-up","fa-1x"),class = "download_button_input")),
    div(actionLink(inputId='github', label='',class = "icon", icon = icon("github","fa-1x"),onclick ="window.open('https://github.com/JGCRI/argus', '_blank')")),
    div(actionLink(inputId='help', label='', class = "icon",icon = icon("question","fa-1x"),onclick ="window.open('https://jgcri.github.io/argus/', '_blank')")),
    div(actionLink(inputId='loadbookmark', label='', class = "icon", icon = icon("bookmark","fa-1x"))),
    div(actionLink(inputId='togglepreload', label='', class="download_button")),
    tags$script(HTML("var header = $('.navbar> .container-fluid');
                   header.append($('#Sidebar'));
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
