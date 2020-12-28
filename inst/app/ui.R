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
  titlePanel(p("RDataViz", style = "color:#3474A7")),

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
        label = "Upload csv",
        accept = c(".csv")
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
      tabsetPanel(
        type = "tabs",

        #---------------------------
        # Main Panel: Summary Tab
        #---------------------------
        tabPanel(
          "Summary",
          br(),
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          )),
          column(
            6,  div(downloadButton('downloadPlotSum', 'Download Plot'), style = "float: right")
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
          #fluidRow(column(12,  div(downloadButton('downloadPlot', 'Download Plot'), style = "float: right"))),
          fluidRow(column(6, p(
            'Sum of Regions Selected'
          )),
          column(
            6, div(downloadButton('downloadPlotChart', 'Download Plot'), style = "float: right")
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
        tabPanel("Table", DTOutput(outputId = "table"))
      )
    )
  )
)
