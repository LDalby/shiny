library(shiny)
library(leaflet)
library(RColorBrewer)
bag = read.csv("Data/snouter.csv")
vejlerne = readOGR(dsn="Data", layer="Fields")
vejlerne = spTransform(vejlerne, CRS("+init=epsg:4326"))
bb = bbox(vejlerne)
server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    bag[bag$Year >= input$range[1] & bag$Year <= input$range[2],]
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, bag$Year)
  })

  output$map <- renderLeaflet({
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
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
        fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = bag)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
        pal = pal, values = ~mag
      )
    }
  })
}

# shinyApp(ui, server)