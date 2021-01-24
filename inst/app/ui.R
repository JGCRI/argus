#' ui

#---------------------------
# Libraries Needed (Also add to DESCRIPTION)
#---------------------------
library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(shinyalert)

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
  #---------------------------
  # Initial Settings/Theme
  #---------------------------
  #shinythemes::themeSelector(),
  #theme = shinythemes::shinytheme("spacelab"),
  actionButton(inputId = "test", label="test"),
  div(downloadButton('downloadAll', "All",  class = "download_button"), style="padding:10px; float: right"),
  div(actionLink(inputId='github', label='', icon = icon("github","fa-1x"),
                 onclick ="window.open('https://github.com/JGCRI/rdataviz', '_blank')"),style="padding:15px 5px;float: right"),
  div(actionLink(inputId='help', label='', icon = icon("question","fa-1x"),
                 onclick ="window.open('https://jgcri.github.io/rdataviz/', '_blank')"),style="padding:15px 5px;float: right"),
  # div(style = "float:left;",fas fa-cog"
  div(actionLink(inputId='loadsetting', label='', icon = icon("cog","fa-1x"),
  ),style="padding:15px 5px;float:right"),

    titlePanel(
    p("RDataViz", style = "color:#3474A7;"),
    windowTitle = "RDataViz"
  ),

  #---------------------------
  # Side Bar
  #---------------------------
  sidebarLayout(
    sidebarPanel(
      # style="position: fixed; width:30%",
      tabsetPanel(
        type = "tabs",
        id="tabs",
        tabPanel(
          "csv",
          br(),
          # CSV Data -------------------------------------
          fileInput(
            inputId = "filedata",
            label = "Upload csv or zip file",
            accept = c(".csv", ".zip"),
            multiple = TRUE,
            width = "100%"
          )),
        tabPanel(
          "url",
          br(),
          textInput(
            inputId = "urlfiledata",
            label = "Enter url to csv or zip file",
            placeholder =  "https://raw.githubusercontent.com/JGCRI/rdataviz/main/inst/extdata/exampleData.csv"),
          br(),
          width = "100%"
        ),
        tabPanel(
          "GCAM",
          br(),
          textInput(
            inputId = "gcamdatabasepath",
            label = "Enter full path to GCAM database",
            placeholder =  "C://example_local_folder/example_database_basexdb"),
          br(),
          width = "100%"
        )
      ),

      # Reactive Input Choices Based on Input File-------------------------
      # GCAM Scenarios
      textOutput("text"),
      uiOutput('gcamScenarios'),

      # Scenarios
      uiOutput('selectScenarios'),
      # Ref Scenarios
      uiOutput('selectRefScenarios'),
      # Params
      uiOutput('selectParams'),
      # Regions
      uiOutput('selectRegions'),

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
                      p(tags$em("rdataviz"),"is an R shiny app that interactively visualizes data cross scenarios, parameters, and regions."),
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
                      p("Khan, Z., Tang, S., Wild, T., Vernon, C., 2021. rdataviz - An R shiny application to interactively vizualize data across scenarios, parameters and regions.Journal of Open Source Software, DOI: XXXX"),
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
                                         fluidRow(column(6, p(
                                           'Sum of Regions Selected'
                                         )),
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
                                                    uiOutput('subsetRegions'),
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
                                                  plotOutput(outputId = "summaryReg"),
                                                ),
                                                width = "100%")
                           )),
                  #---------------------------
                  # Main Panel: Charts
                  #---------------------------
                  tabPanel("Charts",
                           tabsetPanel(type = "pills",
                                       tabPanel("All",
                                                br(),
                                                fluidRow(
                                                  column(
                                                    3, p(
                                                  'Sum of Regions Selected'
                                                    )),
                                                  column(
                                                    2,
                                                         div(
                                                            actionButton(label="Absolute", inputId = "percentDiff",width="100%", class="diff_button")
                                                            )
                                                         ),
                                                  column(
                                                    2,
                                                         actionButton(label="Absolute Difference", inputId = "absDiff",width="100%", class="diff_button")
                                                         ),
                                                  column(
                                                    2,
                                                         actionButton(label="Percent Difference", inputId = "abs", width="100%", class="diff_button")
                                                         ),
                                                  column(
                                                    3, div(downloadButton('downloadPlotChart',NULL, download = "barCharts.png",  class = "download_button"), style = "float: right")
                                                  )),
                                                  br(),
                                                  div(
                                                    class="charts",
                                                    plotOutput(outputId = "plot", width = "100%", height="100%")),
                                       ),
                                       tabPanel("Compare Regions"),
                                       p("hello")
                           )
                  ),
                  #---------------------------
                  # Main Panel: Maps Tab
                  #---------------------------
                  tabPanel("Maps",
                           tabsetPanel(type = "pills",
                                       tabPanel("Summary",
                                                div(
                                                  class="charts",
                                                  uiOutput(outputId = "map"))
                                       ),
                                       tabPanel("Compare Years"),
                                       tabPanel("Class"),
                                       tabPanel("Class Compare years")
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
