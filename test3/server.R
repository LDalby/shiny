library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(viridis)
library(dplyr)
load('Data/maps.RData')
load('Data/bag.RData')
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  getSpData<-reactive({
    # Copy the GIS data
    joinedDataset = vejlerne
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, getData(), by="Polyref"))
    joinedDataset
  })

  getData<-reactive({
    # Subset based on user input:
    bag[bag$Scenario == input$scenario & 
        bag$Species == input$species & 
        bag$Entity == input$entity &
        bag$SeasonNumber == input$year,]
  })

  output$vejlerneMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2])
  })

  # Incremental changes to the map:
 observe({
    theData<-getSpData() 
    # colour palette mapped to data
    # pal = colorNumeric(palette = "Blues", theData$Numbers, na.color = "#FFFFFF") 
    ncol = length(unique(theData$Numbers))
    pal = colorNumeric(palette = viridis(ncol), theData$Numbers, na.color = "#FFFFFF") 
    # set text for the clickable popup labels
    vejlerne_popup <- paste0("<strong>", theData$Entity, ": </strong>", 
                            theData$Numbers)
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("vejlerneMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(fillColor = pal(theData$Numbers), 
                  fillOpacity = 1, 
                  color = "#BDBDC3", 
                  weight = 0.5,
                  popup = vejlerne_popup) %>%
      clearControls() %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = ~Numbers)  
  })

output$nfields <- renderValueBox({
    theData = getData()
    nfields = unique(theData$Polyref)
    valueBox(
      value = length(nfields),
      subtitle = switch(unique(theData$Entity),
        "Udbytte" = "Marker med udbytte",
        "Antal" = "Marker med gæs",
        "foobar")
    )
})

output$totalbag <- renderValueBox({
    theData = getData()
    totalbag = totalbag[Scenario == unique(theData$Scenario) &
                        Species == unique(theData$Species) &
                        SeasonNumber == unique(theData$SeasonNumber), TotalBag]
    valueBox(
      value = totalbag,
      subtitle = "Samlet udbytte"
    )
})

  observe({
    theData = getSpData()
    proxy <- leafletProxy("vejlerneMap", data = theData)
    # Remove any existing markers, and only if the markers are
    # enabled, create a new ones.
    roostpop = paste0("<strong>Rasteplads for: </strong>", roosts$Species)
    proxy %>% clearMarkers()
      if(input$roosts) {
      proxy %>% addMarkers(data = roosts,
                           popup = roostpop)
    }
  })
}
