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
source('functions.R')

# ---------------------------------------------------------------------------- DATA

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
                                   h3("Overview of the motivation profile by country"),
                                   plotlyOutput("radar_bi_effect"),
                                   h3("Precise comparaison of the motivation arguments"),
                                   plotlyOutput("bar_bi_effect")
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
    
    radar_values_1 <- get_radar_values(input, input$country_1)
    radar_values_2 <- get_radar_values(input, input$country_2)
    
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
  
  output$bar_bi_effect <- renderPlotly({
    radar_values_1 <- get_radar_values(input, input$country_1)
    radar_values_2 <- get_radar_values(input, input$country_2)
    
    q <- plot_ly(x = answers, y = radar_values_1$pct, type = 'bar', name = input$country_1) %>%
      layout(yaxis = list(title = 'Proportion of intention'), barmode = 'group')
    
    if(input$compare){
      q <- q %>% add_trace(
        y = radar_values_2$pct,
        name = input$country_2
      ) 
    }
    
    return(q)
  })
  
  
# DEMOGRAPHIC ----------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)
