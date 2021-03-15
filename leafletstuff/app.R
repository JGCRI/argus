#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

ui <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "mymap")
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {#define the color pallate for the magnitidue of the earthquake

  output$mymap <- renderLeaflet({
    a <- read.csv("oof.csv") %>% group_by(subRegion, piece) %>% group_split() 
    base <- a[[1]]
    base <- base %>% mutate(id = subRegion)
    for (i in 2:length(a)){
      d <- a[[i]] %>% mutate(id = subRegion)
      base <- base %>% add_row(lat=NA, long=NA) %>% bind_rows(d)
    }
    print(base$id)
    return(leaflet() %>% addTiles() %>% addPolygons(data=base, label = unique(base$subRegion), layerId = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE))
  })
  
  observeEvent(input$mymap_shape_click,{
    print(input$mymap_shape_click)
  })
  
}
shinyApp(ui = ui, server = server)


