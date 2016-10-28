# App for visualization of the Starling project.
#
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(data.table)
load('Data/fields.RData')
load('Data/starlings.RData')
load('Data/data.RData')

# Choices for drop-down
vars <- c(
  "2015" = "Crop2015",
  "2016Early" = "Crop2016Early",
  "2016Late" = "Crop2016Late"
)

ui <- navbarPage("Starlings", id = "nav",
 tabPanel("Map", 
   div(class="outer",
      tags$head(
        # Test if this will cure the full screen issue
        includeCSS("style.css")
        ),
      leafletOutput("hjortkaerMap", height = "100%", width = "100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
       width = 200, height = "auto",
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
   ),
 tabPanel("Data",
          fluidPage(
            titlePanel('Download data'),
            sidebarLayout(
              sidebarPanel(
                downloadButton('downloadData', 'Download')
              ),
              mainPanel(
                fluidRow(
                  DT::dataTableOutput("table")
                )
              )
            )
          )
   )
 )

server <- function(input, output, session) {
  getBirdChoices<-reactive({
    # Get the right group of birds for the selected season
    switch(input$fieldseason, 
      'Crop2015' = c("S10-2015", "S2-2015", "S3-2015", "S5-2015", "S7-2015", "S8-2015", "S9-2015"),
      'Crop2016Early' = c( "S14-2016", "S13-2016", "S17-2016", "S4a-2016", "S11-2016", "S9a-2016", "S12-2016", "S1-2016"),
      'Crop2016Late' = c( "S4b-2016", "S9b-2016"))
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
    tmp = fields[, c(input$fieldseason, "PolyID")]
    names(tmp) = c("Cover", "PolyID")
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
    coverpop = paste0("<strong>", "Cover", ": </strong>", theData$Cover)
    idpop = paste0("<strong>", "ID", ": </strong>", theData$PolyID)
    fields_popup <- paste(coverpop, idpop, sep = "<br/>")
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
   birdid = paste0("<strong>", "LoggerID", ": </strong>", bird$LoggerID)
   date = paste0("<strong>", "Date", ": </strong>", bird$Date)
   alt = paste0("<strong>", "Altitude", ": </strong>", bird$Alt)
   speed = paste0("<strong>", "Speed", ": </strong>", bird$Speed)
   dist = paste0("<strong>", "Distance", ": </strong>", round(bird$Dist))
   sex = paste0("<strong>", "Sex", ": </strong>", bird$Sex)
   fixpop = paste(sep = "<br/>", birdid, sex, date, alt, speed, dist)
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
 
 output$table <- DT::renderDataTable(DT::datatable({
   data = starlings[!Cover %in% c('Forest', 'Garden', 'Building'),]
 }))
 
 output$downloadData <- downloadHandler(
   filename = "Starlings.txt",
   content = function(file) {
     write.table(starlings, file, row.names = FALSE, quote = FALSE, sep = '\t')
   }
 )
}
# Run the application 
shinyApp(ui = ui, server = server)

