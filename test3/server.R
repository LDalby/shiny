library(shiny)
library(leaflet)
library(RColorBrewer)
# bag = read.table("Data/snouter4.txt", header = TRUE, stringsAsFactors = FALSE)
# vejlerne = readOGR(dsn="Data", layer="Fields")
# names(vejlerne) =  "Polyref"
# vejlerne = vejlerne[!vejlerne$Polyref %in% c(134266, 136277,156216,163713,139680,141133),]
# bb = bbox(vejlerne)
# roosts = readOGR(dsn = "Data", layer = 'Roosts')
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
    bag[bag$Scenario == input$scenario & bag$Species == input$species & bag$Entity == input$entity,]
  })

  output$vejlerneMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    # Create a palette that maps factor levels to colors
    # roostpal = colorFactor(c("navy", "red", "yellow"),
    #                          domain = c("Greylag", "Barnacle", "Pinkfoot"))
    leaflet() %>% addTiles() %>%
      fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2])# %>% 
      # addCircleMarkers(data = roosts, popup = ~Species, color = ~roostpal(Species),
     
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
 observe({
    theData<-getSpData() 
    # colour palette mapped to data
    # pal <- colorQuantile("Blues", theData$Numbers, n = 10) 
    pal <- colorNumeric("Blues", theData$Numbers, na.color = "#FFFFFF") 
    # set text for the clickable popup labels
    vejlerne_popup <- paste0("<strong>", theData$Entity, ": </strong>", 
                            theData$Numbers)
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("vejlerneMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Numbers), 
                  fillOpacity = 1, 
                  color = "#BDBDC3", 
                  weight = 1,
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
                        Species == unique(theData$Species), mean]
    valueBox(
      value = totalbag,
      subtitle = "Gns. totalt udbytte"
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

# shinyApp(ui, server)