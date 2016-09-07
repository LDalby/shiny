library(shiny)
library(leaflet)
library(RColorBrewer)
bag = read.table("Data/snouter2.txt", header = TRUE)
vejlerne = readOGR(dsn="Data", layer="Fields")
vejlerne = spTransform(vejlerne, CRS("+init=epsg:4326"))
names(vejlerne) =  "PolyRefNum"
vejlerne = vejlerne[!vejlerne$PolyRefNum %in% c(134266, 136277,156216,163713,139680,141133),]
bb = bbox(vejlerne)
server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  getDataSet<-reactive({
    # Subset based on user input:
    dataSet = bag[bag$scenario == input$scenario & bag$Species == input$species,]
    # Copy our GIS data
    joinedDataset<-vejlerne
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="PolyRefNum"))
    joinedDataset
  })

  output$vejlerneMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2])
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
 observe({
    theData<-getDataSet() 
    # colour palette mapped to data
    # pal <- colorQuantile("Blues", theData$Numbers, n = 10) 
    pal <- colorNumeric("Blues", theData$Numbers, na.color = "#FFFFFF") 
    # set text for the clickable popup labels
    vejlerne_popup <- paste0("<strong>Numbers Shot: </strong>", 
                            theData$Numbers)
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("vejlerneMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Numbers), 
                  fillOpacity = 0.9, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = vejlerne_popup)  
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    theData = getDataSet()
    proxy <- leafletProxy("vejlerneMap", data = theData)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      # pal = colorQuantile("Blues", theData$Numbers, n = 10)
      pal = colorNumeric("Blues", theData$Numbers, na.color = "#FFFFFF") 
      proxy %>% addLegend(position = "bottomright",
        pal = pal,
        values = ~Numbers
        # values = as.numeric(quantile(theData$Numbers, probs = seq(0, 1, .1), na.rm = TRUE))
      )
    }
  })
}

# shinyApp(ui, server)