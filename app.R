library(markdown)
library(shiny)
library(plyr)
library(tidyverse)
library(plotly)
library(gtools)
library(knitr)

source("map.R")
source("bubble.R")
source("Data prep.R")


# ---------------------------------------------------------------------------- IMPORTS
#source('radar_bi_effect.R')

# ---------------------------------------------------------------------------- DATA
read_data()
items <- read.csv("data/basic_income_dataset_dalia.csv")
items$counted <- 1

# ---------------------------------------------------------------------------- CONSTS
countries <- c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GB','GR','HR','HU','IE','IT',
               'LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK')

age_group <- list(
  'all',
  '14_25',
  '26_39',
  '40_65'
)

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

read_chunk("map.R", labels = "map_code")

# ---------------------------------------------------------------------------- UI
ui <- navbarPage("Basic income analyser",
           tabPanel("Regional intention",
           run_map()
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
                                               choices = countries, selected = "NL")
                                   ),
                                   hr(),
                                   checkboxInput("gender_enable", "Enable gender filter", value = FALSE),
                                   conditionalPanel("input.gender_enable == 1",
                                                    radioButtons("gender", h5("Gender"),
                                                choices = list("Women only" = "female", "Men only" = "male"),selected = "male")),
                                   hr(),
                                   selectInput("age", h5("Age group"), 
                                               choices =  list(
                                                 'All age' = 'all',
                                                 '14 to 25 yo' = '14_25',
                                                 '26 to 39 yo' = '26_39',
                                                 '40 to 65 yo' = '40_65'
                                               ), selected = "all"),
                                   hr(),
                                   checkboxInput("rural_enable", "Enable rurality filter", value = FALSE),
                                   conditionalPanel("input.rural_enable == 1",
                                                    radioButtons("rural", h5("Rurality"),
                                                                 choices = list("Rural only" = "rural", "Urban only" = "urban"),selected = "urban")),
                                   hr(),
                                   selectInput("education", h5("Education level"), 
                                               choices =  list(
                                                 'All levels' = 'all',
                                                 'Low level' = 'low',
                                                 'Medium level' = 'medium',
                                                 'High level' = 'high'
                                               ), selected = "all"),
                                   hr(),
                                   checkboxInput("job_enable", "Enable full time job filter", value = FALSE),
                                   conditionalPanel("input.job_enable == 1",
                                                    radioButtons("job", h5("Has a full time job"),
                                                                 choices = list("Employed only" = "yes", "Unemployed only" = "no"),selected = "yes")),
                                   hr(),
                                   checkboxInput("children_enable", "Enable children filter", value = FALSE),
                                   conditionalPanel("input.children_enable == 1",
                                                    radioButtons("children", h5("Has children"),
                                                                 choices = list("Has children only" = "yes", "Doesn't has children only" = "no"),selected = "yes"))
                                   
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
                    run_bubbles()
           )
)


# ---------------------------------------------------------------------------- SERVER
server <- function(input, output) {
  
# ---------------------------------------------------------------------------- DEMOGRAPHIC
  #df <- callModule(radar_bi_effect, "work_effect")
  #output$radar_bi_effect <- df
  

  answers = mixedsort(bi_effect_answer)
  
  output$radar_bi_effect <- renderPlotly({
    list <- c("country_code", "question_bbi_2016wave4_basicincome_effect")
    if(input$gender_enable){
      list <- c(list, "gender")
    }
    if(input$age != 'all'){
      list <- c(list, "age_group")
    }
    if(input$rural_enable){
      list <- c(list, "rural")
    }
    if(input$education != 'all'){
      list <- c(list, "dem_education_level")
    }
    
    if(input$job_enable){
      list <- c(list, "dem_full_time_job")
    }
    if(input$children_enable){
      list <- c(list, "dem_has_children")
    }
    
    (summ <- ddply(items, list, summarize, counter=sum(counted)))
    agg_data = ddply(summ, .(country_code), mutate, pct = counter / sum(counter) * 100)
    data_countries_age = as_tibble(agg_data)
    
    
    radar_values_1 <- data_countries_age %>% filter(country_code == input$country_1)  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    radar_values_2 <- data_countries_age %>% filter(country_code == input$country_2)  %>% arrange(question_bbi_2016wave4_basicincome_effect)
    
    if(input$gender_enable){
      radar_values_1 <- radar_values_1 %>% filter(gender == input$gender)
      radar_values_2 <- radar_values_2 %>% filter(gender == input$gender)
    }
    if(input$age != 'all'){
      radar_values_1 <- radar_values_1 %>% filter(age_group == input$age)
      radar_values_2 <- radar_values_2 %>% filter(age_group == input$age)
    }
    
    if(input$rural_enable){
      radar_values_1 <- radar_values_1 %>% filter(rural == input$rural)
      radar_values_2 <- radar_values_2 %>% filter(rural == input$rural)
    }
    if(input$education != 'all'){
      radar_values_1 <- radar_values_1 %>% filter(dem_education_level == input$education)
      radar_values_2 <- radar_values_2 %>% filter(dem_education_level == input$education)
    }
    if(input$job_enable){
      radar_values_1 <- radar_values_1 %>% filter(dem_full_time_job == input$job)
      radar_values_2 <- radar_values_2 %>% filter(dem_full_time_job == input$job)
    }
    if(input$children_enable){
      radar_values_1 <- radar_values_1 %>% filter(dem_has_children == input$children)
      radar_values_2 <- radar_values_2 %>% filter(dem_has_children == input$children)
    }
    
    
    temp_values <- radar_values_1
    temp_values_1 <<- radar_values_1
    for(answer in answers){
      if(nrow(radar_values_1 %>% filter(question_bbi_2016wave4_basicincome_effect == answer)) == 0){
        temp_values_1 <<- radar_values_1 %>% add_row(country_code = input$country_1,question_bbi_2016wave4_basicincome_effect = answer,counter = 0, pct = 0)
      }
      temp_values <<- temp_values_1
    }
    radar_values_1 <- temp_values_1
    
    temp_values_bis <- radar_values_2
    temp_values_2 <<- radar_values_2
    for(answer in answers){
      if(nrow(radar_values_2 %>% filter(question_bbi_2016wave4_basicincome_effect == answer)) == 0){
        temp_values_2 <<- radar_values_2 %>% add_row(country_code = input$country_1,question_bbi_2016wave4_basicincome_effect = answer,counter = 0, pct = 0)
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
