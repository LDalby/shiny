library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
# bag = fread("Data/snouter4.txt")
load('Data/bag.RData')
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("vejlerneMap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("scenario", "Scenario", choices=sort(unique(bag$Scenario)),
      selected = 'Baseline'
    ),
    selectInput("species", "Species", choices=unique(bag$Species),
      selected = 'Pinkfoot'
    ),
    selectInput("entity", "Entity", choices=unique(bag$Entity),
      selected = 'Udbytte'
    ),
    checkboxInput("roosts", "Vis rastepladser", TRUE),

    valueBoxOutput('nfields', width = NULL),

    valueBoxOutput('totalbag', width = NULL)

  )
)
