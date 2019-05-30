# Geospatial data available in the geojson format!
library(geojsonio)
spdf <- geojson_read("data/europe.geo.json",  what = "sp")
require(leaflet)
require(htmltools)
library(plyr)
library(tidyverse)
library(plotly)
library(gtools)

names(spdf)

bi_effect_answer <- c(
  '‰Û_ do more volunteering work',
  '‰Û_ gain additional skills',
  '‰Û_ look for a different job',
  '‰Û_ spend more time with my family',
  '‰Û_ stop working',
  '‰Û_ work as a freelancer',
  '‰Û_ work less',
  'A basic income would not affect my work choices',
  'None of the above'
)
age_group <- list(
  '14_25',
  '26_39',
  '40_65'
)

items <- read.csv("data/basic_income_dataset_dalia.csv")

minAge = min(items$age)
maxAge = max(items$age)
rowHeight = 200


ui <- fluidPage(
  
  
  titlePanel("Basic income analyser"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput("age", 
                  label = "Age",
                  min = minAge, max = maxAge, value = c(minAge, maxAge))
      
    ),
    
    mainPanel(
      plotlyOutput("answer_1_14_25")

    )
  )
)

Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

new_values <- tibble()


server <- function(input, output) {

  spdf$rep = 5
  #bins <- c(0, 1, 2, 3, 4, 5, 6, 7, Inf)
  #pal <- colorBin("YlOrRd", domain = agg_data$pct, bins = bins)
  #print(spdf$iso_a2)
  
  items$counted <- 1
  (summ <- ddply(items, .(country_code, age_group, question_bbi_2016wave4_basicincome_effect), summarize, counter=sum(counted)))
  agg_data = ddply(summ, .(country_code, age_group), mutate, pct = counter / sum(counter) * 100)
  
  
  
  tibble_data = as_tibble(agg_data)
  data <- tibble_data %>% filter(age_group == '14_25', question_bbi_2016wave4_basicincome_effect == '‰Û_ work less')
  print(data)
  
  x <- c('FR', 'BE', 'IT')
  values <- data %>% filter(country_code %in% x)
  y <- values$pct
  data <- data.frame(x, y)
  
  p1 <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>%
    layout(title = "January 2013 Sales Report",
           xaxis = list(title = ""),
           yaxis = list(title = ""))
  
  bar_plots = c()
  for(answer in bi_effect_answer){
    line = c()
    for(age in age_group){
    
      temp_data <- tibble_data %>% filter(age_group == age, question_bbi_2016wave4_basicincome_effect == answer)
      x <- c('FR', 'BE', 'IT')
      values <- temp_data %>% filter(country_code %in% x)
      
      temp_values <- values
      temp_values_1 <<- values
      for(region in x){
        if(nrow(values %>% filter(country_code == region)) == 0){
          temp_values_1 <<- values %>% add_row(country_code = region, age_group = "40_65",question_bbi_2016wave4_basicincome_effect = "‰Û_ gain additional skills",counter = 0, pct = 0)
        }
        temp_values <<- temp_values_1
      }
      
      values <- temp_values_1
      
      
      y <- values$pct
      
      data <- data.frame(x, y)
      
      line <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                                              marker = list(color = 'rgb(158,202,225)',
                                                            line = list(color = 'rgb(8,48,107)',
                                                                        width = 1.5))) %>% layout(title = "1")
      
      print(answer)
      print(age)
      #bar_plots = c(bar_plots, line)
    }
  }
  
  #--------------------------------------------- RADAR
    
    (summ <- ddply(items, .(country_code, age_group, question_bbi_2016wave4_basicincome_effect), summarize, counter=sum(counted)))
    agg_data = ddply(summ, .(country_code, age_group), mutate, pct = counter / sum(counter) * 100)
    data_countries_age = as_tibble(agg_data)
  
    country_1 = "FR"
    country_2 = "BE"
    
    radar_values_1 <- data_countries_age %>% filter(country_code == country_1, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    radar_values_2 <- data_countries_age %>% filter(country_code == country_2, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    
    
    answers = mixedsort(bi_effect_answer)
    
    print(radar_values_1)
    print(answer)
    
    output$answer_1_14_25 <- renderPlotly({
      
      p <- plot_ly(
        type = 'scatterpolar',
        
        fill = 'toself'
      ) %>%
      add_trace(
        r = radar_values_1$pct,
        name = country_1,
        theta = answers
      ) %>%
        add_trace(
          r = radar_values_2$pct,
          name = country_2,
          theta = answers
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T
              #range = c(0,50)
            )
          ),
          showlegend = T
        )
    })

  output$answer_2_14_25 <- renderPlotly({
    x <- c('FR', 'BE', 'IT')
    values <- data %>% filter(country_code %in% x)
    y <- values$pct
    data <- data.frame(x, y)
    
    p <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)',
                                           width = 1.5))) %>%
      layout(title = "January 2013 Sales Report",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
  })
  
}

shinyApp(ui = ui, server = server)