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
                                   selectInput("country_1", h5("Region 1"), 
                                               choices = countries, selected = "FR"),
                                   hr(),
                                   checkboxInput("compare", "Compare", value = TRUE),
                                   conditionalPanel("input.compare == 1",
                                    selectInput("country_2", h5("Region 2"), 
                                               choices = countries, selected = "IT")
                                   ),
                                   hr(),
                                   checkboxInput("gender_enable", "Enable gender filter", value = TRUE),
                                   conditionalPanel("input.gender_enable == 1",
                                                    radioButtons("gender", h3("Gender"),
                                                choices = list("Women only" = "female", "Men only" = "male"),selected = "male"))
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
  

  answers = mixedsort(bi_effect_answer)
  
  output$radar_bi_effect <- renderPlotly({
    list <- c("country_code", "age_group", "question_bbi_2016wave4_basicincome_effect")
    (summ <- ddply(items, list, summarize, counter=sum(counted)))
    agg_data = ddply(summ, .(country_code, age_group), mutate, pct = counter / sum(counter) * 100)
    data_countries_age = as_tibble(agg_data)
    
    
    radar_values_1 <- data_countries_age %>% filter(country_code == input$country_1, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    radar_values_2 <- data_countries_age %>% filter(country_code == input$country_2, age_group == "14_25")  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    
    if(input$gender_enable){
      #radar_values_1 <- radar_values_1 %>% filter(gender == input$gender)
    }
    
    temp_values <- radar_values_1
    temp_values_1 <<- radar_values_1
    for(answer in answers){
      if(nrow(radar_values_1 %>% filter(question_bbi_2016wave4_basicincome_effect == answer)) == 0){
        temp_values_1 <<- radar_values_1 %>% add_row(country_code = input$country_1, age_group = "14_25",question_bbi_2016wave4_basicincome_effect = answer,counter = 0, pct = 0)
      }
      temp_values <<- temp_values_1
    }
    radar_values_1 <- temp_values_1
    
    temp_values_bis <- radar_values_2
    temp_values_2 <<- radar_values_2
    for(answer in answers){
      if(nrow(radar_values_2 %>% filter(question_bbi_2016wave4_basicincome_effect == answer)) == 0){
        temp_values_2 <<- radar_values_2 %>% add_row(country_code = input$country_1, age_group = "14_25",question_bbi_2016wave4_basicincome_effect = answer,counter = 0, pct = 0)
      }
      temp_values_bis <<- temp_values_2
    }
    radar_values_2 <- temp_values_2
    
    p <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = radar_values_1$pct,
        name = input$country_1,
        theta = answers
      )  %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T
          )
        )
      ) 
    
    if(input$compare){
    p <- p %>% add_trace(
          r = radar_values_2$pct,
          name = input$country_2,
          theta = answers
    )
    }
    
    return(p)
  })
  
  
# DEMOGRAPHIC ----------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)
