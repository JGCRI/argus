#' server

#---------------------------
# Libraries Needed
#---------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(cowplot)
library(rdataviz)
library(rmap)
library(shinyWidgets)
library(zip)

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
# Server object
#---------------------------

server <- function(input, output) {
  #---------------------------
  # Load Default Datasets from rdataviz
  #---------------------------
  dataDefault <- rdataviz::exampleData
  map <- rmap::mapGCAMReg32
  ggplottheme <- ggplot2::theme_bw()

  #---------------------------
  # Data File (CSV)
  #---------------------------
  data <- reactive({
    if (is.null(input$filedata)) {
      rdataviz::addMissing(
        dataDefault %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      )
    } else {
      rdataviz::addMissing(
        read.csv(input$filedata$datapath) %>%
          as.data.frame() %>%
          dplyr::select(scenario, subRegion, param, aggregate, class, x, value)
      )
    }
  })

  #---------------------------
  # Scenarios Select
  #---------------------------
  output$selectScenarios = renderUI({
    pickerInput(
      inputId = "scenariosSelected",
      label = "Select Scenarios",
      choices = unique(dataSum()$scenario),
      selected = unique(dataSum()$scenario),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Ref Scenario Select
  #---------------------------
  output$selectRefScenarios = renderUI({
    pickerInput(
      inputId = "scenarioRefSelected",
      label = "Select Ref Scenario",
      choices = unique(dataSum()$scenario),
      selected = unique(dataSum()$scenario)[1],
      multiple = F,
    )
  })

  #---------------------------
  # Parameters Select
  #---------------------------
  output$selectParams = renderUI({
    pickerInput(
      inputId = "paramsSelected",
      label = "Select Params",
      choices = c("Chosen Mix", unique(dataSum()$param)),
      selected = "Chosen Mix",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Regions Select
  #---------------------------
  output$selectRegions = renderUI({
    pickerInput(
      inputId = "regionsSelected",
      label = "Select Regions",
      choices = c("All", unique(data()$subRegion)),
      selected = "All",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "None Selected"
      )
    )
  })

  #---------------------------
  # Reactive Regions Select based on inputs
  #---------------------------
  regionsSelectedx <- reactive({
    if (input$regionsSelected == "All" &
        length(input$regionsSelected) == 1) {
      unique(data()$subRegion)
    } else{
      input$regionsSelected
    }
  })

  #---------------------------
  # Reactive Params based on inputs
  #---------------------------
  paramsSelectedx <- reactive({
    if (any(input$paramsSelected == "Chosen Mix") &
        length(input$paramsSelected) == 1) {
      paramsCheck <- unique(data()$param)[unique(data()$param) %in%
                                            rdataviz::constants()$chosenMix]
      if (length(paramsCheck) >= 1) {
        paramsCheck
      } else{
        unique(data()$param)
      }
    } else{
      input$paramsSelected
    }
  })

  #---------------------------
  # Data Summary
  #---------------------------
  dataSum <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::select(-class) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

  })

  # Filter Data after Reactive Choices -------------------
  dataSumx <- reactive({
    dataSum() %>%
      dplyr::filter(scenario %in% input$scenariosSelected,
                    param %in% paramsSelectedx())
  })

  #---------------------------
  # Data Map
  #---------------------------
  dataMap <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, subRegion, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(subRegion %in% regionsSelectedx()) %>%
      dplyr::select(-class) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, subRegion, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

  })

  # Filter Data after Reactive Choices -------------------
  dataMapx <- reactive({
    dataMap() %>%
      dplyr::filter(scenario %in% input$scenariosSelected,
                    param %in% paramsSelectedx())
  })


  #---------------------------
  # Data Chart
  #---------------------------
  dataChartx <- reactive({
    # Aggregate across classes
    tblAggsums <- data() %>%
      dplyr::filter(
        scenario %in% input$scenariosSelected,
        param %in% paramsSelectedx(),
        subRegion %in% regionsSelectedx()
      ) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "sum") %>%
      dplyr::select(scenario, param, class, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ sum(.)))
    tblAggmeans <- data() %>%
      dplyr::filter(
        scenario %in% input$scenariosSelected,
        param %in% paramsSelectedx(),
        subRegion %in% regionsSelectedx()
      ) %>%
      dplyr::mutate(scenario = as.character(scenario)) %>%
      dplyr::filter(aggregate == "mean") %>%
      dplyr::select(scenario, param, class, x, value) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarize_at(c("value"), list( ~ mean(.)))

    dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()
  })

  #---------------------------
  # Data Chart Absolute Diff
  #---------------------------
  dataDiffAbsx <- reactive({
    diffText <- " Diff Abs"

    if (is.null(input$scenarioRefSelected)) {
      print(paste("No reference scenario provided", sep = ""))
      print(paste(
        "Using ",
        unique(dataChartx()$scenario)[1],
        " as reference",
        sep = ""
      ))
      scenRef_i = unique(dataChartx()$scenario)[1]
    } else{
      if (!input$scenarioRefSelected %in% unique(dataChartx()$scenario)) {
        print(paste(
          "scenario ",
          input$scenarioRefSelected,
          " not in scenarios",
          sep = ""
        ))
        print(paste(
          "Using ",
          unique(dataChartx()$scenario)[1],
          " as reference",
          sep = ""
        ))
        scenRef_i = unique(dataChartx()$scenario)[1]
      } else{
        scenRef_i <- input$scenarioRefSelected
      }
    } # Check if Ref Scenario Chosen

    # Calculate Diff Values
    tbl_pd <- dataChartx() %>%
      dplyr::filter(scenario == scenRef_i)

    for (k in unique(dataChartx()$scenario)[unique(dataChartx()$scenario) !=
                                            scenRef_i]) {
      tbl_temp <- dataChartx() %>%
        dplyr::filter(scenario %in% c(scenRef_i, k))
      tbl_temp <- tbl_temp %>%
        tidyr::spread(scenario, value)

      tbl_temp[is.na(tbl_temp)] <- 0

      tbl_temp <- tbl_temp %>%
        dplyr::mutate(!!paste(k, diffText, sep = "") := get(k) - get(scenRef_i)) %>%
        dplyr::select(-dplyr::one_of(c(k, scenRef_i)))
      tbl_temp <- tbl_temp %>%
        tidyr::gather(key = scenario, value = value, -c(names(tbl_temp)[!names(tbl_temp) %in% paste(k, diffText, sep =
                                                                                                      "")]))
      tbl_pd <- dplyr::bind_rows(tbl_pd, tbl_temp)
    }

    tbl_pd <- tbl_pd %>%
      dplyr::mutate(scenario = factor(scenario,
                                      levels = c(scenRef_i,
                                                 unique(
                                                   tbl_pd$scenario
                                                 )[unique(tbl_pd$scenario) != scenRef_i])))
    tbl_pd
  })


  #---------------------------
  # Summary Plot
  #---------------------------
  summaryPlot <- function(){
    ggplot2::ggplot(dataSumx(),
                    aes(x=x,y=value,
                        group=scenario,
                        color=scenario)) +
      ggplottheme +
      geom_line() +
      ylab(NULL) +  xlab(NULL) +
      facet_wrap(.~param, scales="free_y",
                 labeller = labeller(param = label_wrap_gen(15)))+
      theme(legend.position="top",
            plot.margin=margin(0,20,0,0,"pt"),
            aspect.ratio=1)
  }

  output$summary <- renderPlot({
    summaryPlot()
  },
  height=function(){min(1000,max(500,50*length(unique(dataSumx()$param))))}
  )

  output$downloadPlotSum <- downloadHandler(
    filename = "summaryPlot.png",
    content = function(file) {
      ggsave(file,plot=summaryPlot())
    })

  #---------------------------
  # Chart Plot
  #---------------------------
  chartPlot <- function(){

    dataChartPlot <- dataDiffAbsx()

    plist <- list()
    for(i in 1:length(unique(dataChartPlot$param))){

      # Check Color Palettes
      palAdd <- rmap::colors()$pal_Basic
      missNames <- unique(dataChartPlot$class)[!unique(dataChartPlot$class) %in%
                                                     names(rmap::colors()$pal_rmap)]
      if (length(missNames) > 0) {
        palAdd <- palAdd[1:length(missNames)]
        names(palAdd) <- missNames
        palCharts <- c(rmap::colors()$pal_rmap, palAdd)
      } else{
        palCharts <- rmap::colors()$pal_rmap
      }

      plist[[i]] <-  ggplot2::ggplot(dataChartPlot %>%
                                       filter(param==unique(dataChartPlot$param)[i])%>%
                                       droplevels(),
                                     aes(x=x,y=value,
                                         group=scenario,
                                         fill=class)) +
        ggplottheme +
        ylab(NULL) + xlab(NULL) +
        scale_fill_manual(breaks=names(palCharts),values=palCharts) +
        scale_y_continuous(position = "right")+
        geom_bar(position="stack", stat="identity") +
        facet_grid(param~scenario, scales="free",switch="y")+
        theme(legend.position="right",
              legend.title = element_blank(),
              plot.margin=margin(20,20,0,0,"pt"),
              aspect.ratio=1)}
    cowplot::plot_grid(plotlist=plist,ncol=1,align = "v")
  }


  output$plot <- renderPlot({
    chartPlot()
  },
  height=function(){300*length(unique(dataChartx()$param))}
  )

  output$downloadPlotChart <- downloadHandler(
    filename = "barChart.png",
    content = function(file) {
      ggsave(file,plot=chartPlot(),width=13,height=max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
    })



  #---------------------------
  # Maps
  #---------------------------

    output$map <- renderUI({

      dataMapxi = dataMapx() %>%
        filter(param %in% paramsSelectedx()[1])

      mapx <- (rmap::mapFind(dataMapxi))$subRegShapeFound;
      mapx@data <- mapx@data %>%
        dplyr::left_join(data)%>%
        dplyr::select("subRegion","value"); mapx@data
      mapx_1 <- tm_shape(mapx) +
        tm_polygons(col = "value",
                    style = "fixed",
                    breaks = c(0, 25, 50, 75, 100),
                    legend.hist = TRUE) +
        tm_layout(legend.outside = T,
                  legend.show = F)

      m1<-tmap_leaflet(mapx_1)
      m2<-tmap_leaflet(mapx_1) %>% clearControls()
      sync(m1,m2,ncol=2)
    })

  #---------------------------
  # Data Table
  #---------------------------
  output$table <- renderDT(
    # Point to reactive values
    data(),
    filter = "top"
  )

  output$downloadTable <- downloadHandler(
    file = "table.csv",
    content = function(file) {
      print(class(data()))
      write.csv(data() , file)
    })

  #---------------------------
  # Download All
  #---------------------------
  output$downloadAll <- downloadHandler(
    file = "all.zip",
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- c("table.csv", "summaryCharts.png", "barCharts.png")
      write.csv(data(), "table.csv")
      ggsave("summaryCharts.png",plot=summaryPlot())
      ggsave("barCharts.png",plot=chartPlot(),width=13,height=max(10,min(45,5*length(unique(dataChartx()$param)))),units="in")
      print(fs)
      zip::zip(zipfile=file, files=fs)
    }
  )


}
