library(leaflet)
library(htmltools)

leafletMapUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(outputId = ns("resource_map"))
  
}

leafletMapServer <- function(input, output, session, map_dat) {
  
  output$resource_map <- renderLeaflet({
    labs <- sprintf("<h5>%s</h5>\n<h5>%s</h5>\n<p>%s</p>", map_dat$Admin2, prettyNum(map_dat$cases_per, big.mark = ","), map_dat$Province_State)
    
    leaflet(data = map_dat) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~Long_, lat = ~Lat, layerId = ~UID,
                       label = ~map(labs, HTML), stroke = TRUE, color = "black", weight = 1,
                       radius = ~cases_per/20, fillOpacity = 1, fillColor = "#4B9CD3")
  })
  
  return(reactive({ input$resource_map_marker_click$id }))
  
}