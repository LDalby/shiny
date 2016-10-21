# App for visualization of the Starling project.
#
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
load('Data/fields.RData')
load('Data/starlings.RData')

# Choices for drop-down
vars <- c(
  "2015" = "Crop2015",
  "2016Early" = "Crop2016Early",
  "2016Late" = "Crop2016Late"
)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("hjortkaerMap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("fieldseason", "Field season", choices= vars,
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
  getBirdChoices<-reactive({
    # Get the right group of birds for the selected season
    switch(input$fieldseason, 
      'Crop2015' = c("S4a", "S9a"),
      'Crop2016Early' = c("S17", "S21"),
      'Crop2016Late' = c("S1", "S15"))
  })
  # Update the selectinput with based on getBirdChoices
  observe({
     choices = getBirdChoices()
     updateSelectInput(session, "bird", 
                        choices = choices,
                        selected = choices[1])
  })

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
                  fillOpacity = 0.9, 
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
   proxy <- leafletProxy("hjortkaerMap", data = theData)
   # Remove any existing markers, and only if the markers are
   # enabled, create a new ones.
   proxy %>% clearMarkers()
   # Add bird fix points:
   bird = getBirdData()
   date = paste0("<strong>", "Date", ": </strong>", bird$Date)
   alt = paste0("<strong>", "Altitude", ": </strong>", bird$Alt)
   speed = paste0("<strong>", "Speed", ": </strong>", bird$Speed)
   dist = paste0("<strong>", "Distance", ": </strong>", round(bird$Dist))
   fixpop = paste(sep = "<br/>", date, alt, speed, dist)
   proxy %>% addCircleMarkers(data = bird,
                              radius = 1,
                              popup = fixpop)
   # Show ringing site:
   if(input$ringingsite) {
     proxy %>% addMarkers(data = ringingsite,
                          popup = "Ringing site")
   }
   # Show availability grid:
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

