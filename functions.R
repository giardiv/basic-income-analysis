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

bi_arg_for_list <- c(
  'volunteering','responsibility','needs','opportunity','everyone','expense','none'
)


get_radar_values <- function(input, country){

  items <- read.csv("data/basic_income_dataset_dalia_v2.csv")
  items$counted <- 1
  
  answers = mixedsort(bi_effect_answer)
  
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
  
  
  radar_values_1 <- data_countries_age %>% filter(country_code == country)  %>% arrange(question_bbi_2016wave4_basicincome_effect)
  
  if(input$gender_enable){
    radar_values_1 <- radar_values_1 %>% filter(gender == input$gender)
  }
  if(input$age != 'all'){
    radar_values_1 <- radar_values_1 %>% filter(age_group == input$age)
  }
  
  if(input$rural_enable){
    radar_values_1 <- radar_values_1 %>% filter(rural == input$rural)
  }
  if(input$education != 'all'){
    radar_values_1 <- radar_values_1 %>% filter(dem_education_level == input$education)
  }
  if(input$job_enable){
    radar_values_1 <- radar_values_1 %>% filter(dem_full_time_job == input$job)
  }
  if(input$children_enable){
    radar_values_1 <- radar_values_1 %>% filter(dem_has_children == input$children)
  }
  
  
  temp_values <- radar_values_1
  temp_values_1 <<- radar_values_1
  for(answer in answers){
    if(nrow(radar_values_1 %>% filter(question_bbi_2016wave4_basicincome_effect == answer)) == 0){
      temp_values_1 <<- radar_values_1 %>% add_row(country_code = country,question_bbi_2016wave4_basicincome_effect = answer,counter = 0, pct = 0)
    }
    temp_values <<- temp_values_1
  }
  radar_values_1 <- temp_values_1
  
  return(radar_values_1)
}


get_radar_values_arg <- function(input, country){
  
  items <- read.csv("data/basic_income_dataset_dalia_v2.csv")
  items$counted <- 1
  
  
  answers = mixedsort(bi_arg_for_list)
  
  list <- c(#bi_arg_for_list, 
            "country_code")
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
  
  
  (summ <- ddply(items, list, summarize, counter=sum(counted),
                 c_volunteering=sum(volunteering),
                 c_responsibility=sum(responsibility),
                 c_needs=sum(needs),
                 c_opportunity=sum(opportunity),
                 c_everyone=sum(everyone),
                 c_expense=sum(expense),
                 c_none=sum(none)
        ))
  agg_data = ddply(summ, list, summarize, 
                   pct_volunteering = c_volunteering / counter * 100,
                   pct_responsibility = c_responsibility / counter * 100,
                   pct_needs = c_needs / counter * 100, 
                   pct_opportunity = c_opportunity / counter * 100,
                   pct_everyone = c_everyone / counter * 100,
                   pct_expense = c_expense / counter * 100,
                   pct_none = c_none / counter * 100,
                )
  data_countries_age = as_tibble(agg_data)
  
  
  
  radar_values_1 <- data_countries_age %>% filter(country_code == country) # %>% arrange(question_bbi_2016wave4_basicincome_effect)
  
  if(input$gender_enable){
    radar_values_1 <- radar_values_1 %>% filter(gender == input$gender)
  }
  if(input$age != 'all'){
    radar_values_1 <- radar_values_1 %>% filter(age_group == input$age)
  }
  
  if(input$rural_enable){
    radar_values_1 <- radar_values_1 %>% filter(rural == input$rural)
  }
  if(input$education != 'all'){
    radar_values_1 <- radar_values_1 %>% filter(dem_education_level == input$education)
  }
  if(input$job_enable){
    radar_values_1 <- radar_values_1 %>% filter(dem_full_time_job == input$job)
  }
  if(input$children_enable){
    radar_values_1 <- radar_values_1 %>% filter(dem_has_children == input$children)
  }
  
  
  return(c(
    radar_values_1$pct_volunteering,
    radar_values_1$pct_responsibility,
    radar_values_1$pct_needs,
    radar_values_1$pct_opportunity,
    radar_values_1$pct_everyone,
    radar_values_1$pct_expense,
    radar_values_1$pct_none
  ))
}
