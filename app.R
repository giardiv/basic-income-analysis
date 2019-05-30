library(markdown)
library(shiny)
library(plyr)
library(tidyverse)
library(plotly)
library(gtools)


# ---------------------------------------------------------------------------- IMPORTS
#source('radar_bi_effect.R')

# ---------------------------------------------------------------------------- DATA
items <- read.csv("data/basic_income_dataset_dalia.csv")
items$counted <- 1

# ---------------------------------------------------------------------------- CONSTS
countries <- c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GB','GR','HR','HU','IE','IT',
               'LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK')

# ---------------------------------------------------------------------------- UI
ui <- navbarPage("Basic income analyser",
           tabPanel("Regional intention",
                    h1("MAPS")
           ),
           navbarMenu("Demographic motivation",
                      #work_effect_ui("work_effect"),
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
                                               choices = countries, selected = "IT")
                                 ),
                                 mainPanel(
                                   plotlyOutput("radar_bi_effect")
                                 )
                               )
                      ),
                      
                      tabPanel("Arguments against basic income",
                               h1("Arguments against basic income")
                      ),
                      tabPanel("Arguments for basic income",
                               h1("Arguments for basic income ")
                      ),
                      
                      # ---- EXAMPLE OF FLUID ROW
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        h2("md") #includeMarkdown("about.md")
                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
                      # EXAMPLE ----
           ),
           tabPanel("Environnement influence",
                    h1("SCATTERPLOOOT")
           )
)


# ---------------------------------------------------------------------------- SERVER
server <- function(input, output) {
  
# ---------------------------------------------------------------------------- DEMOGRAPHIC
  #df <- callModule(radar_bi_effect, "work_effect")
  #output$radar_bi_effect <- df
  
  (summ <- ddply(items, .(country_code, age_group, question_bbi_2016wave4_basicincome_effect), summarize, counter=sum(counted)))
  agg_data = ddply(summ, .(country_code, age_group), mutate, pct = counter / sum(counter) * 100)
  data_countries_age = as_tibble(agg_data)
  
  
  for(country in countries){
    temp_values <- data_countries_age
    temp_values_1 <<- data_countries_age
    for(region in x){
      if(nrow(values %>% filter(country_code == region)) == 0){
        temp_values_1 <<- values %>% add_row(country_code = region, age_group = "40_65",question_bbi_2016wave4_basicincome_effect = "‰Û_ gain additional skills",counter = 0, pct = 0)
      }
      temp_values <<- temp_values_1
    }
    values <- temp_values_1
  }
  
  
  answers = mixedsort(bi_effect_answer)
  
  output$radar_bi_effect <- renderPlotly({
    radar_values_1 <- data_countries_age %>% filter(country_code == input$country_1, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    radar_values_2 <- data_countries_age %>% filter(country_code == input$country_2, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    
    p <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = radar_values_1$pct,
        name = input$country_1,
        theta = answers
      ) %>%
      add_trace(
        r = radar_values_2$pct,
        name = input$country_2,
        theta = answers
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T
          )
        )
      )
  })
  
  
# DEMOGRAPHIC ----------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)
