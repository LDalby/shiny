library(shiny)
library(data.table)
library(ralmass)
# Input own data:
hb = fread('e:/almass/WorkDirectories/Goose/WD11/HuntingBagRecord.txt')
hb[, Species:=sapply(GameType, ConvertGameType)]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$distPlot <- renderPlot({
    x    <- hb[, GameType]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = 'white')
  })
})
