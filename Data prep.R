library(geojsonio)
library(maptools)

add_country = function(data, column, question, input) {
  plot_age_1 <- data %>% filter(Country == input)
  x_1 <- plot_age_1 %>% filter(question_bbi_2016wave4_basicincome_vote == question)
  x_1 = mutate(x_1, percentage = (counter / sum(counter)) * 100   )
  
  matrix_1 = x_1 %>% remove_rownames %>% column_to_rownames(var=column)
  matrix_1$question_bbi_2016wave4_basicincome_vote <- NULL
  matrix_1$counter <- NULL
  matrix_1$perc <- NULL
  Country = (matrix_1$Country)[1]
  matrix_1$Country <- NULL
  matrix_1 = t(matrix_1)
  row.names(matrix_1)[1] <- Country
  matrix_1 = data.frame(matrix_1)
  return(matrix_1)
}

remove_country = function(data, country) {
  
  data <- data[!rownames(data) %in% country, ]
  return(data)
}
 
read_data = function(){
##Make sure to set wd to git folder
# spatial data for map
spdf <<- geojson_read("europe.geo.json",  what = "sp")

# Data frames
Raw_Data <<- read_csv("data/Raw_full_names.csv") 
HPI <<- read_csv("data/HPI.csv")

### Cleaning raw data
# voting patterns
Raw_Data$counted <- 1
(summ <<- ddply(Raw_Data, .(Country, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
agg_data = ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

tibble_data = as_tibble(agg_data)

# votes for
votes <<- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would vote for it')
colnames(votes)[colnames(votes)=="perc"] <<- "vote_for"
votes$question_bbi_2016wave4_basicincome_vote <<- NULL
# votes$counter <- NULL

# probably for
votes["probably_for"] <<- (tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would probably vote for it'))["perc"]

# no vote
votes["no_vote"] <<- (tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would not vote'))["perc"]

## workaround for missing data
# probably against
probably_against <<- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would probably vote against it')
colnames(probably_against)[colnames(probably_against)=="perc"] <<- "probably_against"
probably_against$question_bbi_2016wave4_basicincome_vote <<- NULL
probably_against$counter <- NULL
#against
against <<- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would vote against it')
colnames(against)[colnames(against)=="perc"] <<- "against"
against$question_bbi_2016wave4_basicincome_vote <<- NULL
against$counter <<- NULL

votes <<- merge(votes, probably_against, by="Country")
votes <<- merge(votes, against, by="Country")
###

# binding dataframe to spatial data
o <<- match(spdf$admin, votes$Country)
xtra1 <<- votes[o,]
row.names(xtra1) <<- row.names(spdf)
map_data <- spCbind(spdf, xtra1)

### Data for bar charts
## age groups
(summ <- ddply(Raw_Data, .(Country, age_group, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
age_data <<- ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

## Gender
(summ <<- ddply(Raw_Data, .(Country, gender, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
gender_data <<- ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

## Full-time job
(summ <<- ddply(Raw_Data, .(Country, dem_full_time_job, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
job_data <<- ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

colors_bar <<- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

rm(list = ls()[grep("^bar", ls())], envir = .GlobalEnv)


### Data for bubble chart
bubble_data <<-  merge(HPI, votes, by="Country")
}



