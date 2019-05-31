library(geojsonio)
library(maptools)

##Make sure to set wd to git folder
# spatial data for map
spdf <- geojson_read("europe.geo.json",  what = "sp")

# Data frames
Raw_Data <- read_csv("data\\Raw_full_names.csv") 
HPI <- read_csv("data\\HPI.csv")

### Cleaning raw data
# voting patterns
Raw_Data$counted <- 1
(summ <- ddply(Raw_Data, .(Country, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
agg_data = ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

tibble_data = as_tibble(agg_data)

# votes for
votes <- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would vote for it')
colnames(votes)[colnames(votes)=="perc"] <- "vote_for"
votes$question_bbi_2016wave4_basicincome_vote <- NULL
votes$counter <- NULL

# probably for
votes["probably_for"] <- (tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would probably vote for it'))["perc"]

# no vote
votes["no_vote"] <- (tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would not vote'))["perc"]

## workaround for missing data
# probably against
probably_against <- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would probably vote against it')
colnames(probably_against)[colnames(probably_against)=="perc"] <- "probably_against"
probably_against$question_bbi_2016wave4_basicincome_vote <- NULL
probably_against$counter <- NULL
#against
against <- tibble_data %>% filter(question_bbi_2016wave4_basicincome_vote == 'I would vote against it')
colnames(against)[colnames(against)=="perc"] <- "against"
against$question_bbi_2016wave4_basicincome_vote <- NULL
against$counter <- NULL

votes <- merge(votes, probably_against, by="Country")
votes <- merge(votes, against, by="Country")
###

# binding dataframe to spatial data
o <- match(spdf$admin, votes$Country)
xtra1 <- votes[o,]
row.names(xtra1) <- row.names(spdf)
map_data <- spCbind(spdf, xtra1)


### Data for bar charts
## age groups
(summ <- ddply(Raw_Data, .(Country, age_group, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
age_data = ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

## Gender
(summ <- ddply(Raw_Data, .(Country, gender, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
gender_data = ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)

## Full-time job
(summ <- ddply(Raw_Data, .(Country, dem_full_time_job, question_bbi_2016wave4_basicincome_vote), summarize, counter=sum(counted)))
job_data = ddply(summ, .(Country), mutate, perc = (counter / sum(counter)) * 100)


### Data for bubble chart
# bubble_data <-  merge(HPI, data, by="Country")


