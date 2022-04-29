###########################################################################
# Script 4 - Baseline correction of sentiment analyses data per federal state AND gender
###########################################################################
# Again, only resulting aggregates are made available.

# read in library
library(tidyverse)

# load gender_region_data from exploratory script
# load range_date_depr, range_date_anxi from baseline_correction script
# load range_date_anx_waves, range_date_depr_waves
# load process_survey_data.R
source('scripts/Survey_Processing.R')



# based on the exclusion criterion (min 100k posts per year per federal state) Burgenland
# Kärnten, Niederoesterreich and Voralberg not considered in the following.
# Also remove NAs
gender_region_twitter_2019 <- gender_region_twitter_2019 %>%
  drop_na() %>% # removes any row containing a missing value
  filter(!(federal_state %in% c("Burgenland", "KÃ¤rnten", "NiederÃ¶sterreich", "Vorarlberg"))) %>% 
  filter(!gender == "unknown") %>%
  mutate(federal_state = factor(federal_state), gender = factor(gender)) 

gender_region_twitter_2020 <- gender_region_twitter_2020 %>%
  drop_na() %>% 
  filter(!federal_state %in% c("Burgenland", "KÃ¤rnten", "NiederÃ¶sterreich", "Vorarlberg") ) %>%
  filter(!gender == "unknown") %>%
  mutate(federal_state = factor(federal_state), gender = factor(gender)) 

gender_region_twitter_2020_rest <- gender_region_twitter_2020_rest %>%
  drop_na() %>%
  filter(!federal_state %in% c("Burgenland", "KÃ¤rnten", "NiederÃ¶sterreich", "Vorarlberg")) %>%
  filter(!gender == "unknown") %>%
  filter(!user_category == "organisational") %>%
  mutate(federal_state = factor(federal_state), gender = factor(gender)) %>%
  select(id, gender, federal_state)

# sum overlapping id's --> exclude these
# sum(gender_region_twitter_2020$id %in% gender_region_twitter_2020_rest$id)
# sum(gender_region_twitter_2020_rest$id %in% gender_region_twitter_2020$id)

gender_region_twitter_2020 <- gender_region_twitter_2020[!(gender_region_twitter_2020$id
                                                           %in% gender_region_twitter_2020_rest$id), ]

gender_region_twitter_2020 <- rbind(gender_region_twitter_2020, gender_region_twitter_2020_rest)

# rename factor Oberoesterreich (Upper Austria)
levels(gender_region_twitter_2019$federal_state)[1] <- "Oberoesterreich"
levels(gender_region_twitter_2020$federal_state)[1] <- "Oberoesterreich"


###################
# descriptive table: number of included postings per federal state, per gender
table_gender_region <- addmargins(table(gender_region_twitter_2020$gender,
                                        gender_region_twitter_2020$federal_state))

colnames(table_gender_region)[1:5] <- c("Upper Austria", "Salzburg", "Styria",
                                   "Tyrol", "Vienna")

# save
save(table_gender_region, file = "data/tables.RData")
###################


# join id's from id_twitter_baseline and id_twitter_2020 with gender_region_twitter_2019/2020$id
id_twitter_baseline <- tibble(id = id_twitter_baseline)
id_twitter_2020 <- tibble(id = id_twitter_2020)
gs_twitter_id <- cbind(gs_twitter, id_twitter_2020, date_twitter_2020)
liwc_twitter_id <- cbind(liwc_twitter, id_twitter_2020, date_twitter_2020)
gs_twitter_baseline_id <- cbind(gs_twitter_baseline, id_twitter_baseline, date_twitter_baseline)
liwc_twitter_baseline_id <- cbind(liwc_twitter_baseline, id_twitter_baseline, date_twitter_baseline)


# join id's and sentiment data
join_gs_baseline <- inner_join(gs_twitter_baseline_id, gender_region_twitter_2019, by = "id")
names(join_gs_baseline)[5] <- "date"

join_liwc_baseline <- inner_join(liwc_twitter_baseline_id, gender_region_twitter_2019, by = "id")
names(join_liwc_baseline)[10] <- "date"

join_gs_2020 <- inner_join(gs_twitter_id, gender_region_twitter_2020, by = "id")
names(join_gs_2020)[5] <- "date"

join_liwc_2020 <- inner_join(liwc_twitter_id, gender_region_twitter_2020, by = "id")
names(join_liwc_2020)[10] <- "date"


##############################################################################
# Baseline-corrrection liwc & gs Twitter data per federal state & gender
##############################################################################

# compute the sum of the scores at a daily level and divide by the total number of tweets that day
# --> take the mean grouped by date 

join_gs_baseline_df <- join_gs_baseline %>%
  group_by(date, federal_state, gender) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))

join_gs_2020_df <- join_gs_2020 %>%
  group_by(date, federal_state, gender) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))

join_liwc_baseline_df <- join_liwc_baseline %>%
  group_by(date, federal_state, gender) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

join_liwc_2020_df <- join_liwc_2020 %>%
  group_by(date, federal_state, gender) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))


# to further baseline correct these values according to the weekday in 2019, add
# a column that categorizes the dates into weekdays

join_gs_baseline_df <- join_gs_baseline_df %>%
  mutate(weekdays = format(date, format = "%u"))

join_gs_2020_df <- join_gs_2020_df %>%
  mutate(weekdays = format(date, format = "%u"))

join_liwc_baseline_df <- join_liwc_baseline_df %>%
  mutate(weekdays = format(date, format = "%u"))

join_liwc_2020_df <- join_liwc_2020_df %>%
  mutate(weekdays = format(date, format = "%u"))


# for baseline correction: calculate the mean for each weekday per federal state and gender in 2019

join_gs_baseline_df <- join_gs_baseline_df %>%
  group_by(weekdays, federal_state, gender) %>%
  summarize(positive = mean(positive), negative = mean(negative))

join_liwc_baseline_df <- join_liwc_baseline_df %>%
  group_by(weekdays, federal_state, gender) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))


# subtract and divide mean to baseline correct 2020 data
federal_liwc_corrected <- left_join(join_liwc_2020_df, join_liwc_baseline_df, 
                                    by = c("weekdays", "federal_state", "gender"))
# join_liwc_corrected %>%
#   filter(federal_state == "Wien")


for (i in 4:9){
  federal_liwc_corrected[, i + 7] <- (federal_liwc_corrected[, i] - federal_liwc_corrected[, i + 7]) / federal_liwc_corrected[, i + 7]
}

# change names
names(federal_liwc_corrected)[c(4:9, 11:16)] <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", 
                                                  "Posemo.corrected", "Negemo.corrected", 
                                                  "Anx.corrected", "Anger.corrected", "Sad.corrected",
                                                  "Social.corrected")


# for GS:
federal_gs_corrected <- left_join(join_gs_2020_df, join_gs_baseline_df, 
                                  by = c("weekdays", "federal_state", "gender"))

for (i in 4:5){
  federal_gs_corrected[, i + 3] <- (federal_gs_corrected[, i] - federal_gs_corrected[, i + 3]) / federal_gs_corrected[, i + 3]
}
# change names
names(federal_gs_corrected)[c(4, 5, 7, 8)] <- c("positive", "negative", 
                                                "positive.corrected", "negative.corrected")

# save these datatables for factor analyses
gs_daily_corrected <- federal_gs_corrected
liwc_daily_corrected <- federal_liwc_corrected

save(gs_daily_corrected, file = "data/gs_daily_corrected.RData")
save(liwc_daily_corrected, file = "data/liwc_daily_corrected.RData")
# load("data/gs_daily_corrected.RData")
# load("data/liwc_daily_corrected.RData")

# For further analyses, compute different baseline-corrected values for different time spans. 

# First for range_date that encompasses the dates until 75% of the participants responded 
load("data/range_date_75.RData")

# initialize lists 
liwc_federal_gender <- vector(mode = "list", length = 14)
gs_federal_gender <- vector(mode = "list", length = 14)
range_date_2 <- range_date_75

for(j in 1:14){
  range_date_waves <- NULL
  for(i in 1:12){
    range_date_2[[i]][1] <- range_date_75[[i]][1] - j
    
    # first, create two dataframes where the first column displays the survey wave dates and the second 
    # column the according wave.
    range_date_waves_df <- data.frame(Date = range_date_2[[i]][1] + 0:diff(range_date_2[[i]]),
                                      Wave = rep(i, diff(range_date_2[[i]]) + 1))
    range_date_waves <- rbind(range_date_waves, range_date_waves_df)
  }
  names(range_date_waves)[1] <- "date"
  
  gs_federal_gender[[j]] <- inner_join(gs_daily_corrected,
                                       range_date_waves, by = "date")
  
  liwc_federal_gender[[j]] <- inner_join(liwc_daily_corrected, 
                                         range_date_waves, by = "date")
  
  gs_federal_gender[[j]] <- gs_federal_gender[[j]] %>%
    group_by(Wave, federal_state, gender) %>%
    summarize(positive = mean(positive), negative = mean(negative),
              positive.corrected = mean(positive.corrected),
              negative.corrected = mean(negative.corrected))
  
  liwc_federal_gender[[j]] <- liwc_federal_gender[[j]] %>%
    group_by(Wave, federal_state, gender) %>%
    summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
              Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
              Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
              Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
              Social.corrected = mean(Social.corrected))
  
}
rm(range_date_waves_df)
rm(range_date_waves)
rm(range_date_2)

save(liwc_federal_gender, file = "data/liwc_federal_gender.RData")
save(gs_federal_gender, file = "data/gs_federal_gender.RData")




# for main analyses define seperate variables: for depression: 14 days prior to survey onset
# for anxiety and anger: 7 days prior to survey onset
gender_gs_corrected_depr <- gs_federal_gender[[14]]
gender_gs_corrected_anx <- gs_federal_gender[[7]]
gender_liwc_corrected_depr <- liwc_federal_gender[[14]]
gender_liwc_corrected_depr <- liwc_federal_gender[[7]]

# save baseline-corrected variables
save(gender_gs_corrected_depr, file = "data/gender_gs_corrected_depr.RData")
save(gender_gs_corrected_anx, file = "data/gender_gs_corrected_anx.RData")
save(gender_liwc_corrected_depr, file = "data/gender_liwc_corrected_depr.RData")
save(gender_liwc_corrected_anx, file = "data/gender_liwc_corrected_anx.RData")

