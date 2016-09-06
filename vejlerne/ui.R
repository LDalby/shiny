library(shinydashboard)
library(leaflet)
library(DT)

header<-dashboardHeader(title="Snouter hunting bag")

body<-dashboardBody(
  fluidRow(
    column(width = 10,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("vejlerneMap", height=600)
               )
    ),
    column(width=2,
           box(width=NULL, 
               uiOutput("yearSelect")
               )
           )
    )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)