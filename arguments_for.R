library(shiny)

items <- read.csv("data/basic_income_dataset_dalia_v2.csv")
items$counted <- 1

# ---------------------------------------------------------------------------- CONSTS
countries <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech","Denmark",
  "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
  "Italy","Latvia","Lituania","Luxembourg","Malta","Netherlands","Poland",
  "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United-Kingdom")

bi_arg_for <- c(
  'volunteering' = 'It increases appreciation for household work and volunteering',
  'responsibility' = 'It encourages financial independence and self-responsibility',
  'needs' = 'It reduces anxiety about financing basic needs',
  'opportunity' = 'It creates more equality of opportunity',
  'everyone' = 'It increases solidarity, because it is funded by everyone',
  'expense' = 'It reduces bureaucracy and administrative expenses',
  'none' = 'None of the above'
)

arguments_for <- function() {
  
  ui <-  fluidPage(style = "height:2000px;",
                   fluidRow(style = "height:2000px;",
                 h1("Which of the following arguments FOR the basic income do you find convincing?"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("country_1", h5("Region 1"), 
                                 choices = countries, selected = "France"),
                     hr(),
                     checkboxInput("compare", "Compare", value = TRUE),
                     conditionalPanel("input.compare == 1",
                                      selectInput("country_2", h5("Region 2"), 
                                                  choices = countries, selected = "Netherlands")
                     ),
                     hr(),
                     checkboxInput("gender_enable", "Enable gender filter", value = TRUE),
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
                     h3("Overview of the argument by country"),
                     plotlyOutput("radar_bi_effect"),
                     h3("Precise comparaison of the arguments"),
                     plotlyOutput("bar_bi_effect")
                   )
                 )
              )
  )
  
  
  
  server <- function(input, output, session) {
    
    output$radar_bi_effect <- renderPlotly({
      
      radar_values_1 <- get_radar_values_arg(input, input$country_1, TRUE)
      radar_values_2 <- get_radar_values_arg(input, input$country_2, TRUE)
      
      p <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = radar_values_1,
          name = input$country_1,
          theta = bi_arg_for
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
          r = radar_values_2,
          name = input$country_2,
          theta = bi_arg_for
        ) 
      }
      
      return(p)
    })
    
    output$bar_bi_effect <- renderPlotly({
      radar_values_1 <- get_radar_values_arg(input, input$country_1, TRUE)
      radar_values_2 <- get_radar_values_arg(input, input$country_2, TRUE)
      
      q <- plot_ly(x = bi_arg_for, y = radar_values_1, type = 'bar', name = input$country_1) %>%
        layout(yaxis = list(title = 'Proportion of choices'), barmode = 'group')
      
      if(input$compare){
        q <- q %>% add_trace(
          y = radar_values_2,
          name = input$country_2
        )
      }
      
      return(q)
    })
    
  }
  
  shinyApp(ui, server)

}


arguments_for()