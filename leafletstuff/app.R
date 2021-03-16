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

oof <- read.csv("oof.csv")
selected <- setNames(rep(1, length(unique(oof$subRegion))), unique(oof$subRegion))

ui <- fluidPage(
    mainPanel(
        leafletOutput(outputId = "mymap")
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {#define the color pallate for the magnitidue of the earthquake
  xchange <- reactiveVal(selected)
  
  output$mymap <- renderLeaflet({
    print(selected)
    a <-  oof %>% group_by(subRegion, piece) %>% group_split() 
    base <- a[[1]]
    base <- base %>% mutate(id = subRegion)
    z <- leaflet() %>% addTiles()
    z <- z %>% addPolygons(data=base, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)    
    for (i in 2:length(a)){
      z <- z %>% addPolygons(data=a[[i]], group =unique(a[[i]]$subRegion), lat=~lat, lng=~long, color = "#4287f5", stroke=TRUE)
    }
    for (i in 2:length(a)){#group =unique(a[[i]]$subRegion),
      z <- z %>% addPolygons(data=a[[i]],  label = unique(a[[i]]$subRegion), group="a", layerId = ~group, lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)  
       #base <- base %>% add_row(lat=NA, long=NA) %>% bind_rows(d)
    }

    z <- z%>%
      addLayersControl(
        overlayGroups = unique(oof$group),
        options = layersControlOptions(collapsed = FALSE)
      )
    #z<-leaflet() %>% addTiles() %>% addPolygons(data=base, layerId = ~group, label = unique(base$subRegion), lat=~lat, lng=~long, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
    return(z)
  })
  
  observeEvent(input$mymap_shape_click,{
    print(input$mymap_shape_click)
    l <- gsub('\\.', '', gsub('\\d', '', input$mymap_shape_click$id))
    print(l)
    selectedx <- xchange()
    if (selectedx[l]==1){
      leafletProxy("mymap") %>% hideGroup(l)
      selectedx[l] <- 0
    }else if(selectedx[l]==0){
      print(l)
      leafletProxy("mymap") %>% showGroup(l)  
      leafletProxy("mymap") %>% hideGroup("a")
      leafletProxy("mymap") %>% showGroup("a")     
      selectedx[l] <- 1
    }
    xchange(selectedx)
  })
  
}
shinyApp(ui = ui, server = server)


