
setwd("C:/Users/mamid/Downloads/Shiny/CrossFilter")
library(shiny)
library(magrittr)
library(leaflet)
ships <- read.csv("ships.csv")
head(ships)

ui <- fixedPage(style = "padding: 100px",
                tags$div(style = "width: 50%; float: left", leaflet::leafletOutput("map")),
                tags$div(style = "width: 50%; float: right", DT::dataTableOutput("tbl"))
)

server <- shinyServer(function(input, output) {
  
  
  
  in_bounding_box <- function(data, lat, long, bounds) {
    data %>%
      dplyr::filter(lat > bounds$south & lat < bounds$north & long < bounds$east & long > bounds$west)
  }
  
  data_map <- reactive({
    if (is.null(input$map_bounds)){
      ships
    } else {
      bounds <- input$map_bounds
      in_bounding_box(ships, lat, long, bounds)
    }
  })
  
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(data_map(), 
                  #selection = "single", 
                  extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
                  options = list(deferRender = TRUE, scrollY = 300,
                                 scroller = TRUE,  dom = 'tp', stateSave = TRUE))
  })
  
  output$map <- leaflet::renderLeaflet({
    k <- leaflet::leaflet(data_map()) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        radius = ~ speed/10,
        stroke = FALSE,
        fillOpacity = 0.5)
    row_selected = data_map()[input$tbl_rows_selected,]
    if(length(row_selected)){
      k %>%
        addCircleMarkers(
          layerId = as.character(row_selected$id),
          lng=row_selected$long, 
          lat=row_selected$lat,
          color = "red")
    }
      
      })
  })
# Run the application
shinyApp(ui = ui, server = server)

