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

#---------------------------
# ui object
#---------------------------

ui <- fluidPage(

  #---------------------------
  # CSS/html
  #---------------------------

  theme ="styles.css",
  # Hide Shiny Errors
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  useShinyalert(),

  tags$script("
    Shiny.addCustomMessageHandler('rhm_clic', function(value) {
    Shiny.setInputValue('regionsSelected', value);
    });
  "),

  #     <script async defer
  # 			src="https://maps.googleapis.com/maps/api/js?key=AIzaSyA5aevdcRxbI2nBJ8UGTh_m7kESdN0AVqA&callback=initMap">
  # 		</script>

  tags$script(src = "script.js"),
  tags$div(
    HTML('
      <script src="https://api.mqcdn.com/sdk/mapquest-js/v1.3.2/mapquest.js"></script>
      <link type="text/css" rel="stylesheet" href="https://api.mqcdn.com/sdk/mapquest-js/v1.3.2/mapquest.css"/>
    ')
  ),

  #---------------------------
  # Initial Settings/Theme
  #---------------------------
  #shinythemes::themeSelector(),
  #theme = shinythemes::shinytheme("spacelab"),
  div(downloadButton('downloadAll', "All",  class = "download_button"), style="padding:10px; float: right"),
  div(actionLink(inputId='github', label='', icon = icon("github","fa-1x"),
                 onclick ="window.open('https://github.com/JGCRI/argus', '_blank')"),style="padding:15px 5px;float: right"),
  div(actionLink(inputId='help', label='', icon = icon("question","fa-1x"),
                 onclick ="window.open('https://jgcri.github.io/argus/', '_blank')"),style="padding:15px 5px;float: right"),
  # div(style = "float:left;",fas fa-cog"
  div(actionLink(inputId='loadsetting', label='', icon = icon("cog","fa-1x")
  ),style="padding:15px 5px;float:right"),

  titlePanel(
    p("Argus", style = "color:#3474A7;"),
    windowTitle = "Argus"
  ),

  #---------------------------
  # Side Bar
  #---------------------------
  sidebarLayout(
    sidebarPanel(
      # style="position: fixed; width:30%",
  
      #fluidRow(
        #column(3,
          #actionButton("csv", "csv", width="100%")
         # ),
        #column(3,
        # actionButton("url", "url", width="100%")
        #  ),
        #column(3,
        #actionButton("gcam", "gcam", width="100%")
        #  ),
        #column(3)
        #),
        #class="action-button shiny-bound-input"
      
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
      br(),
      tabsetPanel(
        type = "tabs",
        id="tabs",
        tabPanel(
          "Drop Down Selection",
          br(),
          uiOutput('selectRegions')
        ),
        tabPanel(
            "Map Selection",
            leafletOutput(outputId = "mymap", height="30vh")
        )
      ),
      # div(id="map", class="maps")
      # leafletOutput(outputId = "mymap")
    ),


    #---------------------------
    # Main Panel
    #---------------------------
    mainPanel(
      tabsetPanel(type = "tabs",

                  #---------------------------
                  # Main Panel: Home Tab
                  #---------------------------
                  tabPanel(
                    "Home",
                    style = "margin-bottom: 30px; margin-top: 30px; margin-right: 50px; margin-left: 50px; border-color: #A9A9A9; border-width: thin;border-style: solid;padding: 20px",
                    div(
                      class="charts",
                      h1("Welcome!",style="font-weight: bold; color = #A9A9A9"),
                      p(tags$em("argus"),"is an R shiny app that interactively visualizes data cross scenarios, parameters, and regions."),
                      h3("How-to",style="font-weight: bold; color = #A9A9A9"),
                      hr(style="border-top: 1px solid #bbb;"),
                      tags$ul(
                        tags$li(tags$b("Step 1:"),"Choose Data"),
                        tags$ul(
                          tags$li("Choose a csv, url, or GCAM output directory."),
                          tags$li("Minimum columns required: 'subRegion', 'value'"),
                          tags$li("Optional columns: 'scenario', 'year', 'param', 'class'")),
                        tags$li(tags$b("Step 2:"),"Select scenarios, regions and parameters."),
                        tags$li(tags$b("Step 3 (Optional):"),"Save settings, which can be loaded later."),
                        tags$li(tags$b("Step 4 (Optional):"),"Download indiviudal or all outputs.")),
                      h3("Output Tabs", style="font-weight: bold; color = #A9A9A9"),
                      hr(style="border-top: 1px solid #bbb;"),
                      tags$ul(
                        tags$li(tags$b("Home:"),"Basic instructions on how to run and cite the app."),
                        tags$li(tags$b("Summary:"),"Summary visualization of the input data in the form of a line graph displaying the difference across scenarios."),
                        tags$li(tags$b("Charts:"),"Detailed visualization of the input data in the form of bar graphs displaying the absolute difference across scenarios"),
                        tags$li(tags$b("Maps:"),"Under Development"),
                        tags$li(tags$b("Table:"),"Table displaying the input data, including search and sort functions")),
                      h3("Citation",style="font-weight: bold; color = #A9A9A9"),
                      hr(style="border-top: 1px solid #bbb;"),
                      p("Khan, Z., Tang, S., Wild, T., Vernon, C., 2021. argus - An R shiny application to interactively vizualize data across scenarios, parameters and regions.Journal of Open Source Software, DOI: XXXX"),
                      width = "100%"
                    )),

                  #---------------------------
                  # Main Panel: Summary Tab
                  #---------------------------
                  tabPanel("Summary",
                           br(),
                           tabsetPanel(type = "pills",
                                       tabPanel(
                                         "All",
                                         br(),
                                         fluidRow(column(6),
                                                  column(
                                                    6, div(
                                                      downloadButton(
                                                        'downloadPlotSum',
                                                        NULL,
                                                        download = "summaryChart.png",
                                                        class = "download_button"
                                                      ),
                                                      style = "float: right"
                                                    )
                                                  )),
                                         div(
                                           class="charts",
                                           plotOutput(outputId = "summary")
                                         ),
                                         width = "100%"
                                       ),
                                       tabPanel("Compare Regions",
                                                br(),
                                                fluidRow(
                                                  column(6,div(
                                                    # Regions
                                                    uiOutput('subsetRegions')
                                                  )),
                                                  column(6, div(
                                                    downloadButton(
                                                      'downloadPlotSumReg',
                                                      NULL,
                                                      download = "summaryChartReg.png",
                                                      class = "download_button"
                                                    ),
                                                    style = "float: right"
                                                  )
                                                  )),
                                                div(
                                                  class="charts",
                                                  plotOutput(outputId = "summaryReg")
                                                ),
                                                width = "100%")
                           ),
                           br(),
                           p("*Sum of Regions Selected", style="color:#cc0000")
                  ),
                  #---------------------------
                  # Main Panel: Charts
                  #---------------------------
                  tabPanel("Charts",
                           br(),
                           tabsetPanel(type = "pills",
                                       tabPanel("All",
                                                br(),
                                                div(
                                                  id = "testz",
                                                  fluidRow(
                                                    column(
                                                      9),
                                                    column(
                                                      3, div(downloadButton('downloadPlotChart',NULL, download = "barCharts.png",  class = "download_button"), style = "float: right")
                                                    )),
                                                  tabsetPanel(
                                                    type="pills",
                                                    tabPanel("Absolute Value",
                                                             br(),
                                                             div(
                                                               class="charts",
                                                               plotOutput(outputId = "plotAbs", width = "100%", height="100%"))
                                                    ),
                                                    tabPanel("Absolute Difference",
                                                             br(),
                                                             div(
                                                               class="charts",

                                                               plotOutput(outputId = "plotDiff", width = "100%", height="100%"))
                                                    ),
                                                    tabPanel("Percent Difference",
                                                             br(),
                                                             div(
                                                               class="charts",

                                                               plotOutput(outputId = "plotPerc", width = "100%", height="100%"))
                                                    )
                                                  )
                                                ),
                                       br(),
                                       p("*Sum of Regions Selected", style="color:#cc0000")
                           ))
                  ),
                  #---------------------------
                  # Main Panel: Maps Tab
                  #---------------------------
                  tabPanel("Maps",
                           br(),
                           tabsetPanel(type = "pills",
                                       tabPanel("Base Map",
                                              div(
                                                class="maps",
                                                leafletOutput(outputId = "mymapBase", height="75vh")
                                                )
                                                #div(
                                                #  class="charts",
                                                #  plotOutput(outputId = "mapBase", width = "100%", height="100%"))
                                                ),
                                       tabPanel("Summary",
                                                fluidRow(
                                                  column(
                                                    6,
                                                    div(
                                                uiOutput('selectMapYear')),
                                                style = "float: left"),
                                                column(
                                                  5,
                                                  div(
                                                  br(),
                                                  br(),
                                                  br(),
                                                pickerInput(
                                                  inputId = "mapLegend",
                                                  label = "Legend Type",
                                                  choices = c("kmean","pretty"),
                                                  selected = "kmean",
                                                  multiple = F)))
                                                ,column(
                                                  1, div(
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    downloadButton(
                                                      'downloadMap',
                                                      NULL,
                                                      download = "map.png",
                                                      class = "download_button"
                                                    ),
                                                    style="float:right"
                                                  ))
                                                ),
                                                div(
                                                  id = "testz",
                                                  tabsetPanel(
                                                    type="pills",
                                                    tabPanel("Absolute Value",
                                                             br(),
                                                             div(
                                                               class="charts",
                                                               plotOutput(outputId = "mapAbs", width = "100%", height="100%"))
                                                    ),
                                                    tabPanel("Absolute Difference",
                                                             br(),
                                                             div(
                                                               class="charts",

                                                               plotOutput(outputId = "mapDiff", width = "100%", height="100%"))
                                                    ),
                                                    tabPanel("Percent Difference",
                                                             br(),
                                                             div(
                                                               class="charts",

                                                               plotOutput(outputId = "mapPerc", width = "100%", height="100%"))
                                                    )
                                                    )
                                                )
                                                )
                                       )
                           ),
                  #---------------------------
                  # Main Panel: Table Tab
                  #---------------------------
                  tabPanel(
                    "Table",
                    br(),
                    fluidRow(
                      column(
                        12, div(downloadButton('downloadTable', NULL, download = "table.csv", class="download_button"), style = "float: right")
                      )),
                    br(),
                    div(
                      class="charts",
                      DTOutput(outputId = "table"))
                  )
      )
    )
  )
)

