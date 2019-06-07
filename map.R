# Geospatial data available in the geojson format!
library(shiny)
library(leaflet)


require(leaflet)
require(htmltools)

#SUB QUESTION find spatial patterns!

run_map <- function() {
  map_data <- spCbind(spdf, xtra1)
 
  
# Fix layout!
  
ui <- fluidPage(
  
  fluidRow(
    column(12,
           h1(textOutput("selected_answer"), align = "center")
    ),
    column(12,
           h6("Click on a country to compare the demographic information of the voters of a country. Click on the country again to remove it from the comparison.", align = "center")
    ),
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
    
    column(10,
           column(6,
                  leafletOutput("mymap")
           ),
           
           column(6,
                  plotOutput(outputId = "age_plot"),
                  plotOutput(outputId = "job_plot"),
                  plotOutput(outputId = "gender_plot")
           )
           
    )
    
  )
)

     
           
# )


server <- function(input, output, session) {
  output$selected_answer <- renderText({input$answer})  
  
  
  output$mymap <- renderLeaflet({
  
    
    answer <- switch(input$answer, 
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
  ) %>%
  
  addLegend(position = "bottomright", pal = pal, values = answer,
            title = "Values",
            labFormat = labelFormat(prefix = "(", suffix = ")%"),
            opacity = 1
  )
  })
  
  observeEvent(input$mymap_shape_click, {
    answer <- switch(input$answer, 
                     "I would vote for it" = "vote_for",
                     "I would probably vote for it" = "probably_for",
                     "I would probably vote against it" = "probably_against",
                     "I would vote against it" = "against",
                     "I would not vote" = "no_vote")
    
    output$age_plot <- renderPlot({
      if(!exists(paste("bar_age_data", answer, sep = ""))){ dataset2 = data.frame(assign(paste("bar_age_data", answer, sep = ""), 
                                                                                         value = (add_country(age_data, "age_group", input$answer, input$mymap_shape_click$id)), 
                                                                                         envir = .GlobalEnv))
      }
      else{
        dataset2 = eval(parse(text = paste("bar_age_data", answer, sep = "")))
        if(input$mymap_shape_click$id %in% row.names(dataset2)){ 
          dataset2 = remove_country(dataset2, input$mymap_shape_click$id)
          print(dataset2)
        }
        else{
          dataset2 = rbind(dataset2, add_country(age_data, "age_group", input$answer, input$mymap_shape_click$id))
        }
      }
      final_colors <- colors_bar[1:nrow(dataset2)]
      print(row.names(dataset2))
      
      barplot(as.matrix(dataset2), main = "Age group", xlim = c(0,100), col = final_colors, names.arg = list("14-25", "26-39", "40-65"), beside=TRUE, legend = row.names(dataset2), horiz = TRUE, border = "white", xlab = "Percentage of total", ylab = "Age group")
      
      data.frame(assign(paste("bar_age_data", answer, sep = ""), 
                        value = dataset2,
                        envir = .GlobalEnv))
      })
    
    output$job_plot <- renderPlot({
      if(!exists(paste("bar_job_data_", answer, sep = ""))){ dataset1 = data.frame(assign(paste("bar_job_data_", answer, sep = ""), 
                                                                                            value = (add_country(job_data, "dem_full_time_job", input$answer, input$mymap_shape_click$id)), 
                                                                                            envir = .GlobalEnv))
      }
      else{
        dataset1 = eval(parse(text = paste("bar_job_data_", answer, sep = "")))
        if(input$mymap_shape_click$id %in% row.names(dataset1)){ 
          dataset1 = remove_country(dataset1, input$mymap_shape_click$id)
          print(dataset1)
        }
        else{
          dataset1 = rbind(dataset1, add_country(job_data, "dem_full_time_job", input$answer, input$mymap_shape_click$id))
        }
      }
      final_colors <- colors_bar[1:nrow(dataset1)]
      print(row.names(dataset1))
      
      barplot(as.matrix(dataset1), main = "Full-time job", xlim = c(0,100), col = final_colors, names.arg = list("no", "yes"), beside=TRUE, legend = row.names(dataset1), horiz = TRUE, border = "white", xlab = "Percentage of total", ylab = "Full-time job")
      
      data.frame(assign(paste("bar_job_data_", answer, sep = ""), 
                        value = dataset1,
                        envir = .GlobalEnv))
    })
    
    output$gender_plot <- renderPlot({
      print(gender_data)
      if(!exists(paste("bar_gender_data_", answer, sep = ""))){ dataset = data.frame(assign(paste("bar_gender_data_", answer, sep = ""), 
                                                             value = (add_country(gender_data, "gender", input$answer, input$mymap_shape_click$id)), 
                                                             envir = .GlobalEnv))
      }
      else{
        dataset = eval(parse(text = paste("bar_gender_data_", answer, sep = "")))
        if(input$mymap_shape_click$id %in% row.names(dataset)){ 
          dataset = remove_country(dataset, input$mymap_shape_click$id)
          print(dataset)
          }
        else{
          dataset = rbind(dataset, add_country(gender_data, "gender", input$answer, input$mymap_shape_click$id))
        }
      }
      final_colors <- colors_bar[1:nrow(dataset)]
      print(row.names(dataset))
      
      barplot(as.matrix(dataset), main = "Gender", xlim = c(0,100), col = final_colors, names.arg = list("female", "male"), beside=TRUE, legend = row.names(dataset), horiz = TRUE, border = "white", xlab = "Percentage of total", ylab = "Gender")
   
      data.frame(assign(paste("bar_gender_data_", answer, sep = ""), 
                        value = dataset,
                        envir = .GlobalEnv))
       })
    
    
  })
}

shinyApp(ui, server)

}


run_map()
