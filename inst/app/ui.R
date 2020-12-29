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

  theme = shinythemes::shinytheme("spacelab"),

  div(downloadButton('downloadAll', "All", style = "font-size:12px !important;color:#FFFFFF;background-image: linear-gradient(#3399f3, #3399f3 50%, #3399f3);border:0px;"), style="padding:10px;float: right"),

  titlePanel(
    p("RDataViz", style = "color:#3474A7"),
    windowTitle = "RDataViz"
    ),

   #---------------------------
  # Side Bar
  #---------------------------
  sidebarLayout(
    sidebarPanel(
      # CSV Data -------------------------------------
      p(
        "Upload a csv file with columns: 'subRegion', 'scenario', 'year', 'param', 'class', 'value'."
      ),


      fileInput(
        inputId = "filedata",
        label = "Upload csv or zip",
        accept = c(".csv", ".zip")
      ),

      textInput(
        inputId = "urlfiledata", label = "Enter url of file", value = "https://raw.githubusercontent.com/JGCRI/rdataviz/main/inst/extdata/exampleData.csv"),

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
      tabsetPanel(
        type = "tabs",

        #---------------------------
        # Main Panel: Home Tab
        #---------------------------
        tabPanel(
          "Home",
          h2("Welcome!"),
          p(
            "This is an R shiny app that interactively visualizes data cross scenarios, parameters, and regions."
          ),
          br(),
          p(
            "GCAM and url input are still under development."
          ),
          br(),
          p(
            "For more information, please visit these links: "
          ),
          a(href = "https://github.com/JGCRI/rmap/blob/master/rdataviz.pdf", "- Cheatsheet"),
          br(),
          a(href = "https://github.com/JGCRI/rdataviz", "- Github"),
          br(),
          a(href = "https://jgcri.github.io/rdataviz/", "- Webpage"),
          br(),
          width = "100%"
        ),

        #---------------------------
        # Main Panel: Summary Tab
        #---------------------------
        tabPanel(
          "Summary",
          br(),
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          ), style = "display: inline-block;"),
          column(
            6, div(downloadButton('downloadPlotSum',NULL, download = "summaryChart.png", style = "font-size:12px !important;color:#FFFFFF;background-image: linear-gradient(#3399f3, #3399f3 50%, #3399f3);border:0px"), style = "float: right")
          )),
          br(),
          plotOutput(outputId = "summary"),
          width = "100%"
        ),
        #---------------------------
        # Main Panel: Charts
        #---------------------------
        tabPanel(
          "Charts",
          br(),
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          )),
          column(
            6, div(downloadButton('downloadPlotChart',NULL, download = "barCharts.png", style = "font-size:12px !important;color:#FFFFFF;background-image: linear-gradient(#3399f3, #3399f3 50%, #3399f3);border:0px"), style = "float: right")
          )),
          br(),
          plotOutput(outputId = "plot", width = "100%")
        ),
        #---------------------------
        # Main Panel: Maps Tab
        #---------------------------
        tabPanel("Maps", uiOutput(outputId = "map")),
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
            6, div(downloadButton('downloadTable', NULL, download = "table.csv", style = "font-size:12px !important;color:#FFFFFF;background-image: linear-gradient(#3399f3, #3399f3 50%, #3399f3);border:0px"), style = "float: right")
          )),
          br(),
          DTOutput(outputId = "table")
        )
      )
    )
  )
)
