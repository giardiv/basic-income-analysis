library(markdown)
library(shiny)
library(plyr)
library(tidyverse)
library(plotly)
library(gtools)
library(knitr)

# Read initial data
source("Data prep.R")
read_data()

# ---------------------------------------------------------------------------- IMPORTS
source("map.R")
source("bubble.R")


source("arguments_for.R")
source("arguments_against.R")
source("bi_work_effect.R")

source('functions.R')




# ---------------------------------------------------------------------------- UI
ui <- navbarPage("Basic income analyser",
           tabPanel("Regional intention",
            run_map()
           ),
           navbarMenu("Demographic motivation",
                      #work_effect_ui("work_effect"),
                      tabPanel("How basic income would affect work choices",
                               includeCSS("style.css"),
                               bi_effect()
                      ),
                      
                      tabPanel("Arguments for basic incomes",
                               includeCSS("style.css"),
                        arguments_for()
                      ),
                      tabPanel("Arguments against basic income", 
                               includeCSS("style.css"),
                               arguments_against()
                      )
           ),
           tabPanel("Environnement influence",
                    run_bubbles()
           )
)


# ---------------------------------------------------------------------------- SERVER
server <- function(input, output) {
  

}

shinyApp(ui = ui, server = server)
