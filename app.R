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

source("arguments_for.R")
source("arguments_against.R")
source("bi_work_effect.R")


# ---------------------------------------------------------------------------- IMPORTS
source('functions.R')

# ---------------------------------------------------------------------------- DATA

items <- read.csv("data/basic_income_dataset_dalia_v2.csv")
items$counted <- 1

# ---------------------------------------------------------------------------- CONSTS
countries <- c(
"Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech","Denmark",
"Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
"Italy","Latvia","Lituania","Luxembourg","Malta","Netherlands","Poland",
"Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United-Kingdom")

bi_effect_answer <- c(
  'do more volunteering work',
  'gain additional skills',
  'look for a different job',
  'spend more time with my family',
  'stop working',
  'work as a freelancer',
  'work less',
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
                               includeCSS("style.css"),
                               style = "height:2000px;",
                               bi_effect()
                      ),
                      
                      tabPanel("Arguments for basic incomes",
                               includeCSS("style.css"),
                               style = "height:2000px;",
                        arguments_for()
                      ),
                      tabPanel("Arguments against basic income", 
                               includeCSS("style.css"),
                               style = "height:2000px;",
                               arguments_against()
                      )
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
  

  
  
  
# DEMOGRAPHIC ----------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)
