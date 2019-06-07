library(plotly)
library(shiny)

#binding files
library(data.table)
library(dplyr)
library(readr)

library(plyr)
library(tidyverse)

run_bubbles <- function() {
  
  colors = c('black', 'green', 'red', 'blue', 'orange')


ui <- fluidPage(
  
  fluidRow(
    column(4,
          helpText("If there would be a referendum on introducing basic income today, how would you vote?" ),
          
          selectInput("answer", 
                      label = "Choose the answer to explore",
                      choices = list("I would vote for it", 
                                     "I would probably vote for it",
                                     "I would probably vote against it", 
                                     "I would vote against it",
                                     "I would not vote"),
                      selected = "I would vote for it"),
    
 
           helpText("Which correlation do you want to explore?" ),
           
           selectInput("correlation", 
                       label = "Choose the correlation to explore",
                       choices = list("Happy Planet Index", 
                                      "Well-being",
                                      "Life Expectancy", 
                                      "Ecological footprint",
                       selected = "Well-being"))
    ),
    column(8,
           plotlyOutput("bubble")
    )
  )
)

server <- function(input, output, session) {
  
  
  output$bubble <- renderPlotly({
    
    answer <- switch(input$answer, 
                     "I would vote for it" = bubble_data$vote_for,
                     "I would probably vote for it" = bubble_data$probably_for,
                     "I would probably vote against it" = bubble_data$probably_against,
                     "I would vote against it" = bubble_data$against,
                     "I would not vote" = bubble_data$no_vote)
    
    correlation <- switch(input$correlation, 
                          "Happy Planet Index" = bubble_data$HPI, 
                          "Well-being" = bubble_data$Wellbeing,
                          "Life Expectancy" = bubble_data$Life_Expectancy, 
                          "Ecological footprint" = bubble_data$Footprint)
    
    plot_ly(bubble_data, x = correlation, y = answer, color = ~Region, colors = colors, text = ~Country, type = 'scatter', mode = 'markers', size = 12)%>%
            
    layout(
           xaxis = list(title=input$correlation, showgrid = FALSE),
           yaxis = list(title=paste(input$answer, "(percentage)"),showgrid = FALSE))
  })  
}

shinyApp(ui, server)

}

run_bubbles()
