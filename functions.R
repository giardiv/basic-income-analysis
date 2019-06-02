
get_radar_values <- function(input, country){
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
  
  print(radar_values_1)
  
  return(radar_values_1)
}
