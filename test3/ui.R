library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
bag = fread("Data/snouter2.txt")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("vejlerneMap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("scenario", "Scenario", choices=unique(bag$scenario),
      selected = 'Baseline'
    ),
    selectInput("species", "Species", choices=unique(bag$Species),
      selected = 'Pinkfoot'
    ),
    checkboxInput("legend", "Show legend", TRUE)
  )
)
