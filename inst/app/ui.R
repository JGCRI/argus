#' ui

#---------------------------
# Libraries Needed (Also add to DESCRIPTION)
#---------------------------
library(shiny)
library(shinythemes)
library(leaflet)
library(DT)

#---------------------------
# Overall Strtucture
#---------------------------

# Side Bar:
#   - Input csv
#   - Input gcam project
#   - Input scenarios
#   - Input ref scenario
#   - Input parameters
#   - Input regions
#   - Input years
# Main Panel:
#   - Summary (Summary plots aggregated by chosen params and regions)
#   - Charts (Bar Charts comparing scenarios aggregated by chosen params and regions)
#   - Maps (Maps comapring scenarios aggregated by params for single years)
#   - Table (Raw data table being used)

#---------------------------
# ui object
#---------------------------
ui <- fluidPage(
  #---------------------------
  # CSS/html
  #---------------------------
  theme ="styles.css",
  # Hide Shiny Errors
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  # tags$style(HTML("#plot {overflow-y:scroll; overflow-x:hidden}")),

  #---------------------------
  # Initial Settings/Theme
  #---------------------------
  #shinythemes::themeSelector(),
  #theme = shinythemes::shinytheme("spacelab"),

  div(downloadButton('downloadAll', "All",  class = "download_button"), style="padding:10px; float: right"),
  div(actionLink(inputId='github', label='', icon = icon("github","fa-1x"),
                 onclick ="window.open('https://github.com/JGCRI/rdataviz', '_blank')"),style="padding:15px 5px;float: right"),
  div(actionLink(inputId='help', label='', icon = icon("question","fa-1x"),
                 onclick ="window.open('https://jgcri.github.io/rdataviz/', '_blank')"),style="padding:15px 5px;float: right"),

  titlePanel(
    p("RDataViz", style = "color:#3474A7"),
    windowTitle = "RDataViz"
    ),

   #---------------------------
  # Side Bar
  #---------------------------
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        id="tabs",
        tabPanel(
          "File",
          br(),
          # CSV Data -------------------------------------
          fileInput(
            inputId = "filedata",
            label = "Upload csv, zip, or GCAM folder",
            accept = c(".csv", ".zip"),
            multiple = TRUE,
            width = "100%"
        )),
        tabPanel(
          "URL input",
          br(),
          textInput(
            inputId = "urlfiledata", label = "Enter url to csv, zip, or GCAM folder", placeholder =  "https://raw.githubusercontent.com/JGCRI/rdataviz/main/inst/extdata/exampleData.csv"),
          br(),
          width = "100%"
        )
      ),

      # Reactive Input Choices Based on Input File-------------------------

      # Scenarios
      uiOutput('selectScenarios'),
      # Ref Scenarios
      uiOutput('selectRefScenarios'),
      # Params
      uiOutput('selectParams'),
      # Regions
      uiOutput('selectRegions'),

      # ShapeFile Data
      fileInput(
        inputId = "filemap",
        label = "Custom shapefile (Optional)",
        multiple = TRUE,
        accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj')
      )

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
          h1("Welcome!",style="font-weight: bold; color = #A9A9A9"),
          p(tags$em("rdataviz"),"is an R shiny app that interactively visualizes data cross scenarios, parameters, and regions."),
          h3("Citation",style="font-weight: bold; color = #A9A9A9"),
          hr(style="border-top: 1px solid #bbb;"),
          p("Khan, Z., Tang, S., Wild, T., Vernon, C., 2021. rdataviz - An R shiny application to interactively vizualize data across scenarios, parameters and regions.Journal of Open Source Software, DOI: XXXX"),
          h3("How-to",style="font-weight: bold; color = #A9A9A9"),
          hr(style="border-top: 1px solid #bbb;"),
          tags$ul(
          tags$li(tags$b("Step 1:"),"Choose Project Folder (For saving all project files)"),
          tags$li(tags$b("Step 2:"),"Load/Save Settings (This will be a file to save all current options on the app, which can be loaded in the future to return to a certain state)"),
          tags$li(tags$b("Step 3:"),"Choose Data"),
          tags$ul(
            tags$li("Choose a csv, zip, or GCAM output directory containing columns: 'subRegion', 'scenario', 'year', 'param', 'class', 'value'."),
            tags$li("GCAM and url input are still under development.")),
          tags$li(tags$b("Step 4:"),"Select scenarios, regions and parameters"),
          tags$li(tags$b("Step 5:"),"Save settings"),
          tags$li(tags$b("Step 6:"),"Download all / Explore output")),
          h3("Output Tabs", style="font-weight: bold; color = #A9A9A9"),
          hr(style="border-top: 1px solid #bbb;"),
          tags$ul(
            tags$li(tags$b("Home:"),"Basic instructions on how to run and cite the app."),
            tags$li(tags$b("Summary:"),"Summary visualization of the input data in the form of a line graph displaying the difference across scenarios."),
            tags$li(tags$b("Charts:"),"Detailed visualization of the input data in the form of bar graphs displaying the absolute difference across scenarios"),
            tags$li(tags$b("Maps:"),"Under Development"),
            tags$li(tags$b("Table:"),"Table displaying the input data, including search and sort functions")),
          width = "100%"
        ),

        #---------------------------
        # Main Panel: Summary Tab
        #---------------------------
        tabPanel("Summary",
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
                     plotOutput(outputId = "summary"),
                     width = "100%"
                   ),
                   tabPanel("Compare Regions",
                            br(),
                            fluidRow(column(12, div(
                                downloadButton(
                                  'downloadPlotSumReg',
                                  NULL,
                                  download = "summaryChartReg.png",
                                  class = "download_button"
                                ),
                                style = "float: right"
                              )
                            )),
                            fluidRow(column(12,div(
                              # Regions
                              uiOutput('subsetRegions'),

                            ))),
                            plotOutput(outputId = "summaryReg"),
                            width = "100%")
                 )),
        #---------------------------
        # Main Panel: Charts
        #---------------------------
        tabPanel("Charts",
                 tabsetPanel(type = "pills",
                   tabPanel("All",
          br(),
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          )),
          column(
            6, div(downloadButton('downloadPlotChart',NULL, download = "barCharts.png",  class = "download_button"), style = "float: right")
          )),
          br(),
          plotOutput(outputId = "plot", width = "100%")
                 ),
          tabPanel("Compare Regions")
          )
        ),
        #---------------------------
        # Main Panel: Maps Tab
        #---------------------------
        tabPanel("Maps",
                 tabsetPanel(type = "pills",
                   tabPanel("Summary",
                 uiOutput(outputId = "map")
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
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          )),
          column(
            6, div(downloadButton('downloadTable', NULL, download = "table.csv", class="download_button"), style = "float: right")
          )),
          br(),
          DTOutput(outputId = "table")
        )
      )
    )
  )
)
