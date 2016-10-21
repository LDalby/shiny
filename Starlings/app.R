# App for visualization of the Starling project.
#
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
load('Data/fields.RData')
load('Data/starlings.RData')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("hjortkaerMap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("fieldseason", "Field season", choices=c("Crop2015", "Crop2016Early", "Crop2016Late"),
      selected = "Crop2015"
    ),
    selectInput("bird", "Bird", choices=sort(unique(spstarlings$LoggerID)),
      selected = "S1"
    ),
    checkboxInput("availgrid", "Show availibity grid", FALSE),
    checkboxInput("ringingsite", "Show ringing site", TRUE)
  )
)


server <- function(input, output, session) {
  output$hjortkaerMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2])
  })
# Reactive expression for the data subsetted to what the user selected
  getSpData<-reactive({
    # Return a subset based on user input:
    tmp = fields[, input$fieldseason]
    names(tmp) = "Cover"
    tmp
  })

  getBirdData<-reactive({
    # Return a subset based on user input:
    spstarlings[spstarlings$LoggerID == input$bird,]
  })

  # Incremental changes to the map:
 observe({
    theData<-getSpData() 
    # colour palette mapped to data
    pal = colorFactor(palette = "Set1", theData$Cover, na.color = "#FFFFFF") 
    # # set text for the clickable popup labels
    fields_popup <- paste0("<strong>", "Cover", ": </strong>",
                            theData$Cover)
    # # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("hjortkaerMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(fillColor = pal(theData$Cover),
                  fillOpacity = 1, 
                  color = "#BDBDC3", 
                  weight = 0.5,
                  popup = fields_popup)  %>%
      clearControls() %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = ~Cover)  
      
  })

 observe({
   theData<-getSpData()
   bird = getBirdData()
   proxy <- leafletProxy("hjortkaerMap", data = theData)
   # Remove any existing markers, and only if the markers are
   # enabled, create a new ones.
   # proxy %>% clearShapes()
     proxy %>% addCircles(data = bird,
                           radius = 1
                          )
 })
 
 observe({
   theData<-getSpData()
   proxy <- leafletProxy("hjortkaerMap", data = theData)
   # Remove any existing markers, and only if the markers are
   # enabled, create a new ones.
   proxy %>% clearMarkers()
   if(input$ringingsite) {
     proxy %>% addMarkers(data = ringingsite,
                          popup = "Ringing site")
   }
   if(input$availgrid) {
     proxy %>% addCircleMarkers(data = newavll,
                          radius = 2,
                          fillColor = 'black',
                          fillOpacity = 0.8,
                          stroke = FALSE,
                          popup = "Availibity point"
                          )
   }
 })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

