library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)
library(RColorBrewer)

# Prepare the map of fields
vejlerne = readOGR(dsn="Data", layer="Fields")
vejlerne = spTransform(vejlerne, CRS("+init=epsg:4326"))
# Find the edges of our map
bounds<-bbox(vejlerne)

# The bag 
bag = read.csv("Data/snouter.csv")

function(input, output, session){
  
  getDataSet<-reactive({
    # Subset based on user input:
    dataSet<-bag[bag$Year == input$dataYear,]
    # Copy our GIS data
    joinedDataset<-vejlerne
    names(joinedDataset) = "PolyRefNum"    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="PolyRefNum"))
    joinedDataset
  })
  
  # Due to use of leafletProxy below, this should only be called once
  output$vejlerneMap<-renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      # Centre the map
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=10 
      )       
  })
  
  observe({
    theData<-getDataSet() 
    # colour palette mapped to data
    pal <- colorQuantile("Reds", theData$Numbers, n = 10) 
    # set text for the clickable popup labels
    vejlerne_popup <- paste0("<strong>Numbers Shot: </strong>", 
                            theData$Numbers)
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("vejlerneMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Numbers), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = vejlerne_popup)  
  })
  # year selecter; values based on those present in the dataset
  output$yearSelect<-renderUI({
    yearRange<-sort(unique(as.numeric(bag$Year)), decreasing=FALSE)
    selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1])
  })
}