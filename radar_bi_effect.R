

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

countries <- c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GB','GR','HR','HU','IE','IT',
               'LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK')

# GET COUNTRY LIST
#(temp <- ddply(items, .(country_code),summarize, counter=sum(counted)))
#print(temp$country_code)




work_effect_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("How basic income would affect work choices",
           h1("How basic income would affect work choices"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("plotType", "Plot type",
                            c("Scatter"="p", "Line"="l")
               ),
               selectInput("country_1", h3("Region 1"), 
                           choices = countries, selected = "FR"),
               
               selectInput("country_2", h3("Region 2"), 
                           choices = countries)
             ),
             mainPanel(
               plotlyOutput("radar_bi_effect")
             )
           )
  )
}



radar_bi_effect <- function(input, output, session) {

  (summ <- ddply(items, .(country_code, age_group, question_bbi_2016wave4_basicincome_effect), summarize, counter=sum(counted)))
  agg_data = ddply(summ, .(country_code, age_group), mutate, pct = counter / sum(counter) * 100)
  data_countries_age = as_tibble(agg_data)
  
  
  country_1 = 'FR' #input$country_1
  country_2 = 'BE'
  
  answers = mixedsort(bi_effect_answer)
  
  
  radar_bi_effect <- renderPlotly({
    radar_values_1 <- data_countries_age %>% filter(country_code == input$country_1, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    radar_values_2 <- data_countries_age %>% filter(country_code == country_2, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    
    
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

  return(radar_bi_effect)
}