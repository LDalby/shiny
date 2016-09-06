library(shiny)
library(leaflet)
library(RColorBrewer)
bag = read.csv("Data/snouter.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("vejlerneMap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("year", "Year", min(bag$Year), max(bag$Year),
      value = range(bag$Year), step = 0.1
    ),
    checkboxInput("legend", "Show legend", TRUE)
  )
)