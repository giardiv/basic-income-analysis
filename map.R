# Geospatial data available in the geojson format!
library(shiny)
library(leaflet)


require(leaflet)
require(htmltools)

#SUB QUESTION find spatial patterns!



# Add bar charts other attributes


# Fix layout!
ui <- fluidPage(
  fluidRow(
    column(2,
           # sidebarLayout(
             # sidebarPanel(
               helpText("If there would be a referendum on introducing basic income today, how would you vote?" ),
               
               selectInput("answer", 
                           label = "Choose the answer to explore",
                           choices = list("I would vote for it", 
                                          "I would probably vote for it",
                                          "I would probably vote against it", 
                                          "I would vote against it",
                                          "I would not vote"),
                           selected = "I would vote for it")
           
    ),
   
             # mainPanel (
                     
   column(8,
          column(8,
          
          # textOutput("message", container = h3),
          
      leafletOutput("mymap")
      ),
      column(4,
             plotOutput(outputId = "distPlot1"),
             plotOutput(outputId = "distPlot2")
             )
   )
    
   )
)
      
           
# )


server <- function(input, output, session) {
 
  output$mymap <- renderLeaflet({
    
    
    answer <- switch(input$answer, 
                   "I would vote for it" = map_data$vote_for,
                   "I would probably vote for it" = map_data$probably_for,
                   "I would probably vote against it" = map_data$probably_against,
                   "I would vote against it" = map_data$against,
                   "I would not vote" = map_data$no_vote)
    
    colorbins <- switch(input$answer, 
                     "I would vote for it" = map_data$vote_for,
                     "I would probably vote for it" = map_data$probably_for,
                     "I would probably vote against it" = map_data$probably_against,
                     "I would vote against it" = map_data$against,
                     "I would not vote" = map_data$no_vote)
    
    
    # Change colors (and bins?)
    pal <- colorBin("YlOrRd", answer, na.color = "#d3d3d3", bins = 6)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      %s%%",
      map_data$admin, format(round(answer, 2), nsmall = 2)
    ) %>% lapply(htmltools::HTML)
    
  mymap <-  leaflet(map_data) %>%
    setView(14, 56, 3) %>%
    addProviderTiles("MapBox")
  
  # Check if this can be removed
  
  # , options = providerTileOptions(
    #   id = "mapbox.light",
    #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
  
  mymap %>% addPolygons(
    fillColor = ~pal(answer),
    weight = 2,
    opacity = 1,
    layerId = map_data$admin,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )
  })
  
  observeEvent(input$mymap_shape_click, {
    
    output$distPlot1 <- renderPlot({
      plot_age <- age_data %>% filter(Country == input$mymap_shape_click$id)
      x <- plot_age %>% filter(question_bbi_2016wave4_basicincome_vote == input$answer)
      barplot(x$perc , names.arg = x$age_group, col = "#75AADB", border = "white", main = input$mymap_shape_click$id, xlab = "Age group", ylab = "percentage of total")
    })

    output$distPlot2 <- renderPlot({
      plot_gender <- gender_data %>% filter(Country == input$mymap_shape_click$id)
      x <- plot_gender %>% filter(question_bbi_2016wave4_basicincome_vote == input$answer)
      barplot(x$perc , names.arg = x$gender, col = "#75AADB", border = "white", main = input$mymap_shape_click$id, xlab = "Gender", ylab = "percentage of total")
    })
  })
}

shinyApp(ui, server)
