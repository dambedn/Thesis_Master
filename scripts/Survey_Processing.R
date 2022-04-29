
###########################################################################
# Script 2 - Pre-processing survey data
###########################################################################

# Since the survey data was assessed by other authors, the raw data cannot be open sourced by us.
# Only the aggregates of the variables as a function of survey wave, gender and or federal state
# will be made available!

###########################################################################

# This second script pre-processes the survey data 
# of a 12-wave representative Austrian survey from 2020, containing ~ 1000
# participants per wave.

# NOTE: ONLY THE CODE AND THE RESULTING AGGREGATED DATA BUT NOT THE RAW DATA IS AVAILABLE FOR THIS SECTION. 

# libraries
library(tidyverse)
#library(janitor) #for cleaning column names
library(haven) # to import spss data

#tutorial on important and formatting data from SPSS: https://tutorials.methodsconsultants.com/posts/reading-sas-spss-or-stata-files-into-r-using-haven/

# read data from spss file
d0 <- read_sav('./data/COVID19_survey_wave1-12.sav', user_na = TRUE)
names(d0) <- tolower(names(d0))

#which variables contain questionnaire data (as opposed to standard variables from survey tool)
#names(d0)
attach(d0)
#head(weight)
#check the last content variable (last before time001)
#attributes(d0$pa01_01)
#xtabs(~pa01_01+welle, d0)

#names(d0[8:284]) #192 for dataset with 6 waves
contentcols <- 8:284 # excluded total score variables, need to be added manually afterwards

#select only the columns with survey content, delete soscisurvey info
d1 <- d0[,contentcols]

# add the total score variables (for anxiety, depression, suicidal thoughts etc)
# "select" subsets columns defined in second parameter, first parameter: data object
d1 <- cbind(d1, select(d0, starts_with("sum")))

# add lastdata to d1
d1 <- cbind(d1, lastdata = d0$lastdata)

#looking at attributes of single or multiple variables
#attributes(d1$me06_01)$label # gets itemquestion
#lapply(select(d1, starts_with("me")), attributes)

# print variable names and labels
# pipes: result of first command is input to the first parameter of subsequent command
d1_names <- d1 %>% 
  sjlabelled::get_label() %>% 
  enframe() %>% #convert list to tibble
  rename(code = name, 
         varname = value)
d1_labels <-   sjlabelled::get_labels(d1, values="as.prefix") %>% enframe() %>% unnest(cols=c(value)) %>% 
  rename(code = name, 
         labels = value)
d1_variables <- d1_names %>% 
  inner_join(d1_labels) %>% 
  #delete all labels with free text entry ("offene Eingabe" or "Anderes)
  filter(!str_detect(varname, "offene Eingabe")) %>% 
  filter(!str_detect(varname, "Anderes")) %>% 
  #free text entry Postleitzahl
  filter(!str_detect(varname, "Postleitzahl")) %>% 
  #free text entry for other selected variables: Zukunftsorgen (worries about future), praktische hilfe
  filter(!code %in% c("co14_01", "co12_13a","pp02_01"))

# write to file
# write.csv(d1_variables, file="./data/suvey_variable_names_labels.csv", row.names=F)

# #set question text as variable names - not used because the question item text is too long for good variable names
# # convert column names to variable label using purrr::set_names(): 
#   set_names(d1 %>% sjlabelled::get_label() %>% enframe() %>% pull(value))
# d1 = d1 %>% 
#   set_names(d1 %>%       # convert variable names to informative labels
#               sjlabelled::get_label() %>% # pull labels from original tibble
#               enframe() %>%               # convert list of column names to a new tibble
#               na_if("") %>%               # if variable label is empty string, convert to NA
#               mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
#               pull(value)) %>%            # extract new variable name into a character vector
#   janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"  

#some important variables for our analysis ####

#sample size pro welle
# xtabs(~welle, data=d1)

#prosocial only in wave 2, 3, 6, 11, 12
# xtabs(~sum_psv+welle, data=d1)

# select variables linked to depression
depressionvars <- d1 %>% 
  select(starts_with("ph01"))

# check whether there are missing values in depression questions
# depressionvars %>%
#   apply(2, "==", - 9) %>%
#   as.vector() %>%
#   sum()

# sum(depressionvars==-9)
# sum(is.na(depressionvars))

# select variables linked to anxiety
anxvars <- d1 %>% 
  select(starts_with("an0"))

# missing values?
# anxvars  %>%
#   apply(2, "==", -9) %>%
#   as.vector() %>%
#   sum()

# sum(anxvars==-9) # returns error
# sum(is.na(anxvars))

#social media variables, different waves for different social media platforms or apps
# mediavars <- d1 %>% 
#   select(starts_with("me"))

#covid related questions
covidvars <- d1 %>% 
  select(c("welle", starts_with("co"))) 

# write.csv(covidvars, file="./data/survey_data_covid_variables.csv", row.names=F)

#opinion on measures (should be weaker, stay the same, stronger)
# str(d1$co05_01)
# xtabs(~co05_01+welle, data=d1)

#burdens in different life areas: 
# str(d1$co06_01)
# xtabs(~co06_01+welle, data=d1)


#other measure related questions
# co19_01: information about regulations/measures
# co20_01: knowledge about measures
# co21_01-co21_11: communication of government?

#write covid related variable names and labels to separate file
co_variables <- d1_variables %>%
  filter(code %in% c(names(covidvars)))
# write.csv(co_variables, file="./data/survey_covid_variables_names_labels.csv", row.names=F)

#rename the important variables to something more easily understandable ####
d2 <- d1 %>% 
  rename(wave = welle, 
         #demographics
         gender=sd01,
         age = sd02,
         region = sd03,
         education = sd04,
         proportion_gender = sd12_01, 
         proportion_age = sd12_02, 
         proportion_region=sd12_03, 
         proportion_education=sd12_04,
         income = sd14,
         postal_code =plz,
         city_rural = stadt,
         #positive life areas:  27 items
         more_wellbeing = co07_15, 
         more_relaxed = co07_16, 
         more_family = co07_17,
         more_friends = co07_18,
         more_hobby = co07_19,
         less_stress = co07_20,
         less_boredom = co07_21,
         more_sport = co07_22,
         healthier_nutrition = co07_23,
         better_sleep = co07_24,
         happier =co07_25, 
         more_connected=co07_26,
         #emotions/affective disorders, #the variables called "sum" are the mean of all depression items (not the sum)
         depression = sum_phq,
         anxiety = sum_angst,
         anger_last_week= co08_01,
         #conflicts
         last_week_conflicts = ko01_01, # last two weeks
         domestic_violence_last_week_psych = ko01_02,
         domestic_violence_last_week_physic = ko01_03,
         last_weeks_conflicts_work = ko01_04,
         change_conflicts = ko02_01, 
         #media
         usage_rank_social_media = me01_01, 
         twitter_account = me02, 
         tweeting_freq = me03_01,
         tweet_read_min_1 = me03_02,
         standard_account = me04, 
         standard_commenting_freq = me05_01,
         standard_read_min_1comment = me06_01,
         fb_account = me07, 
         fb_write_or_share_freq = me08_01,
         fb_read_min_1post = me08_02,
         insta_account = me09, 
         insta_post_freq =me10_01,
         insta_watch_min_1post = me11_01,
         youtube_account = me12, 
         youtube_post_freq = me13_01, 
         youtube_watch_min1post = me14_01,
         tiktok_account = me15, 
         tiktok_post_freq = me16_01, 
         tiktok_watch_min_1post = me17_01,
         #messenger apps,installed (only in wave 12)
         installed_whatsapp = me19_01, 
         installed_telegram = me19_02, 
         installed_fb_messenger = me19_03, 
         installed_wechat = me19_04, 
         installed_signal = me19_05, 
         installed_viber = me19_06,
         installed_threema = me19_07, 
         installed_noneofthese = me19_08, 
         #messenger apps, regularly used (only in wave 12)
         use_regularly_whatsapp = me20_01, 
         use_regularly_telegram = me20_02, 
         use_regularly_fb_messenger = me20_03, 
         use_regularly_wechat = me20_04, 
         use_regularly_signal = me20_05, 
         use_regularly_viber = me20_06,
         use_regularly_threema = me20_07, 
         use_regularly_noneofthese = me20_08, 
         #other psychological questionnaires
         suicidal_thoughts = sum_sui,
         relationship_satisfaction = sum_bz,
         pandemic_anxiety=sum_pa,
         ptsd = sum_ptsd,
         prosocial_behavior = sum_psv,
         resilience = sum_res, 
         likelihood_getting_help = ps02_01,
         #psychological illness
         psych_ill = sd09_01,
         ill_depression = sd10_01,
         ill_bipolar = sd10_02,
         ill_ocd = sd10_03,
         ill_phobia = sd10_04,
         ill_other_anxiety = sd10_05,
         ill_ptsd = sd10_06,
         ill_personality = sd10_07,
         ill_addiction = sd10_08,
         ill_adhd = sd10_09,
         ill_eating = sd10_10,
         ill_stress = sd10_11,
         ill_psychosomatic = sd10_12,
         ill_psychotic = sd10_13,
         ill_dementia = sd10_14,
         ill_sleep = sd10_15,
         ill_other = sd10_16,
         ill_autism = sd10_17,
         #physical illness
         phy_ill = sd09_02,
         phys_heart = sd11_01,
         phys_diabetis = sd11_02,
         phys_breath = sd11_03,
         phys_other = sd11_04)%>% 
  #keep only variables with a new name, delete the codes,most contain a 0, some sd and some co variables need to be excluded in addition because no 0 in the code
  select(!(contains("0")|starts_with("sd")| starts_with("co"))) %>% 
  select(-c(kkinder, kindersp)) %>% 
  #mutate some variables to factors with labels
  mutate_at(c("wave", "gender", "age", "region", "education", 
              "proportion_gender", "proportion_age", "proportion_region", 
              "proportion_education", "postal_code", "city_rural", "income", 
              "more_wellbeing", "more_relaxed", "less_stress", 
              "happier", "more_connected", "anger_last_week", "domestic_violence_last_week_psych", 
              "change_conflicts", "usage_rank_social_media", "twitter_account", 
              "standard_account", "tweeting_freq", "tweet_read_min_1", 
              "standard_commenting_freq", "standard_read_min_1comment", 
              "fb_account", "insta_account", "fb_write_or_share_freq", 
              "fb_read_min_1post", "insta_post_freq", "insta_watch_min_1post", 
              "youtube_account", "tiktok_account", "youtube_post_freq", 
              "youtube_watch_min1post", "tiktok_post_freq", "tiktok_watch_min_1post", 
              "me19", "installed_whatsapp", "installed_telegram", "installed_fb_messenger", 
              "installed_wechat", "installed_signal", "installed_viber", 
              "installed_threema", "installed_noneofthese", "use_regularly_whatsapp", 
              "use_regularly_telegram", "use_regularly_fb_messenger", "use_regularly_wechat", 
              "use_regularly_signal", "use_regularly_viber", "use_regularly_threema", 
              "use_regularly_noneofthese", "domestic_violence_last_week_physic",
              "last_week_conflicts", "last_weeks_conflicts_work"), ~as_factor(.) )
# summary(d2)

# derive characteristics of self-reported Twitter and Der Standard forum users
# social-media usage was surveyed only in waves 3-7 for Twitter and 4-6 for Der Standard
# compute number of users per wave to get an overview
n_user_twitter <- addmargins(xtabs(~ twitter_account+wave, data=d2)[, 3:7])
# xtabs(~ tweeting_freq+wave, data=d2)[, 3:7]
# xtabs(~ tweet_read_min_1+wave, data=d2)[, 3:7] # how many times do you read at least one Tweet
n_user_stand <- addmargins(xtabs(~ standard_account+wave, data=d2)[, 4:7])
# xtabs(~ standard_commenting_freq+wave, data=d2)[, 4:7]
# xtabs(~  standard_read_min_1comment+wave, data=d2)[, 4:7]

# compute proportion of Twitter/Der Standard users
prop_user_twitter <- n_user_twitter["ja", "Sum"] / n_user_twitter["Sum", "Sum"]
prop_user_stand <- n_user_stand["ja", "Sum"] / n_user_stand["Sum", "Sum"]

# change factor labels for grouping by gender results in the same
# structure as with twitter data (alphabetic order in english, female -> male)
d2$gender <-  factor(d2$gender, labels = c("male", "female", "diverse"))
d2$gender <- factor(d2$gender, levels = c("female", "male", "diverse"))
#save(d2, file='data/survey_data.RData')

# compute Twitter / Der Standard users' demographic profile
# Firstly only subset the ones, with a Twitter/Der Standard account
d2_twitter <- d2
d2_twitter$wave <- as.numeric(do.call(rbind, str_split(d2$wave, pattern = " "))[, 2]) # only wave number as name
d2_twitter <- d2_twitter %>%
  filter(wave %in% 3:7) %>% # only filter waves in which twitter usage was surveyed
  filter(twitter_account == "ja") 

d2_stand <- d2
d2_stand$wave <- as.numeric(do.call(rbind, str_split(d2$wave, pattern = " "))[, 2]) # only wave number as name
d2_stand <- d2_stand %>%
  filter(wave %in% 4:7) %>% 
  filter(standard_account == "ja") 

# display proportion of gender, age, income categories as well as region of users
prop_gender_twitter <- prop.table(xtabs(~ gender, data=d2_twitter))
names(prop_gender_twitter) <- c("Female", "Male", "Diverse")
prop_gender_stand <- prop.table(xtabs(~ gender, data=d2_stand))
names(prop_gender_stand) <- c("Female", "Male", "Diverse")

prop_age_twitter <- prop.table(xtabs(~ age, data=d2_twitter))[-1] # exclude "not replied" column
names(prop_age_twitter) <- gsub(pattern = "Jahre", replacement = "Years", x = names(prop_age_twitter))
prop_age_stand <- prop.table(xtabs(~ age, data=d2_stand))[-1]
names(prop_age_stand) <- names(prop_age_twitter)

prop_income_twitter <- prop.table(xtabs(~ income, data=d2_twitter))[-1]
names(prop_income_twitter) <- gsub(pattern = "bis unter", replacement = "-", x = names(prop_income_twitter))
names(prop_income_twitter) <- gsub(pattern = "\\?", replacement = "€", x = names(prop_income_twitter))
names(prop_income_twitter) <- gsub(pattern = "und mehr", replacement = "+", x = names(prop_income_twitter))
prop_income_stand <- prop.table(xtabs(~ income, data=d2_stand))[-1]
names(prop_income_stand) <- names(prop_income_twitter)

prop_region_twitter <- prop.table(xtabs(~ region, data=d2_twitter))[-1]
names(prop_region_twitter) <- c("Burgenland", "Carinthia", "Lower Austria", "Upper Austria", "Salzburg", "Styria", "Tyrol", "Vorarlberg", "Vienna")
prop_region_stand <- prop.table(xtabs(~ region, data=d2_stand))[-1]
names(prop_region_stand) <- names(prop_region_twitter)

prop_gender <- rbind(prop_gender_twitter, prop_gender_stand)
row.names(prop_gender) <- c("Twitter", "Der Standard")
prop_age <- rbind(prop_age_twitter, prop_age_stand)
row.names(prop_age) <- c("Twitter", "Der Standard")
prop_income <- rbind(prop_income_twitter, prop_income_stand)
row.names(prop_income) <- c("Twitter", "Der Standard")
prop_region <- rbind(prop_region_twitter, prop_region_stand)
row.names(prop_region) <- c("Twitter", "Der Standard")
prop_user <- rbind(prop_user_twitter, prop_user_stand)
row.names(prop_user) <- c("Twitter", "Der Standard")

demographics <- list(proportion = prop_user, gender = prop_gender, age = prop_age, income = prop_income,
                     region = prop_region)

#papaja::printnum(demographics$proportion["Twitter", ], digits = 3)

# save
save(demographics, file = "data/demographics.RData")

attach(d2)
# calculate mean anxiety, anger, depression scale score and suicidal ideation scale score per wave
# still need to exclude the first factor in suicidal ideation, then -1, then mean
#summary(anger_last_week)
#as.numeric(anger_last_week)

# compute the mean values of the variables further needed in the analyses over a
# time span until 75% of the participants answered the survey in each wave (see pre-registration)
dates <- d2$lastdata

length_dates <- length(dates)

# participants per wave
n_wave <- d2 %>%
  select(lastdata, wave) %>%
  group_by(wave) %>%
  arrange(lastdata)%>%
  summarize(length(lastdata)) 

n_wave <- data.frame(rbind(n_wave))
# Welle -> Wave 
n_wave$wave  <- gsub(pattern = "Welle", replacement = "", x  = n_wave$wave)
colnames(n_wave) <- c("Wave (dates)", "Number of participants")
# save n_wave to report in the thesis
save(n_wave, file = "data/n_wave.RData")

# Now we select only the first 75% of the participants per survey wave who finished first 
n_wave_75 <- trunc(n_wave$`Number of participants` * 0.75)  

d2_75 <- vector(mode = "list", length = 12)
for (i in 1:12){
  d2_75[[i]] <- d2 %>%
    filter(str_detect(wave, paste("Welle", i, "\\("))) %>%
    arrange(lastdata) %>% # sort by time participants finished the survey
    slice(1:n_wave_75[i]) # extract first 75% of the participants
}


# list to dataframe
d2_75 <- do.call(rbind, d2_75)


# transform into Date object
d2_75$lastdata <- as.Date(d2_75$lastdata)

# names of d2_75$wave are still illustrate full time period
d2_75_date <- d2_75 %>% 
  select(wave, lastdata) %>%
  group_by(wave) %>%
  #arrange(lastdata) %>%
  #summarise(range(lastdata)) %>%
  mutate(range = paste0("(", format.Date(range(lastdata)[1], format = "%d.%m."),
                        "-", format.Date(range(lastdata)[2], format = "%d.%m."),
                        ")")) 

d2_75_date <- d2_75_date$range


# save objects
save(d2_75, file = "data/d2_75.RData")
save(d2_75_date, file = "data/d2_75_date.RData")

# Given the data that only contains 75% of the participants, compute mean scores
# per survey wave of the variables we are interested the most in
# main variables
d2_vars <- d2_75 %>%
  group_by(wave) %>%
  summarize(anxiety = mean(anxiety), depression = mean(depression), 
            anger = mean(as.numeric(anger_last_week) - 1), 
            suicidal_thoughts = mean(suicidal_thoughts))


# rename the wave column with the date in which the mean value of participants (approx. 750/2)
# finished the questionnaire
d2_75$lastdata <- as.Date(d2_75$lastdata)

date_mean <- rep(NA, 12)
for (i in 1:12){
  date_mean[i] <- d2_75 %>%
    filter(str_detect(wave, paste("Welle", i))) %>%
    slice(n = as.integer(round(750/2))) %>%
    select(lastdata)
}

date_mean <- do.call(c, date_mean)


d2_vars[, 1] <- date_mean

# save mean values of important variables for ts plots
# save(d2_vars, file = "data/d2_vars.Rdata")


# in later analysis steps, I perform linear regression analyses, in which  
# the specific survey constructs per survey wave split by gender and federal state 
# are the dependent variables. Hence, also compute these mean scores.

# only include male and female gender for compatibility with twitter data
d2_75_gender <- d2_75 %>%
  filter(gender %in% c("male", "female")) %>%
  mutate(gender = factor(gender))

# source("scripts/process_survey_data.R")
# load('../data/survey_data.R')
d2_vars_federal <- d2_75_gender %>%
  group_by(wave, region, gender) %>%
  summarize(anxiety = mean(anxiety), depression = mean(depression), 
            anger = mean(as.numeric(anger_last_week) - 1), 
            suicidal_thoughts = mean(suicidal_thoughts)) %>% 
  ungroup()

##########
# create a table with number of participants per gender and federal state
table_survey <- d2_75 %>%
  filter((gender %in% c("male", "female")) & (!region %in% c("Burgenland", "Kärnten", "Niederösterreich", "Vorarlberg"))) %>%
  mutate(gender = factor(gender), region = factor(region))

table_survey <- addmargins(table(table_survey$gender, table_survey$region))

# transform into data frame to report it in APA style in thesis
table_survey <- data.frame(rbind(table_survey))
row.names(table_survey) <- c("Female", "Male", "Sum")
colnames(table_survey) <- c("Upper Austria", "Salzburg", "Styria", "Tyrol", 
                            "Vienna", "Sum")


# save to load object within thesis
save(table_survey, file = "data/table_survey.RData") 

##########


# exclude Burgenland, Kärnten, Niederösterreich & Voralberg
d2_vars_federal <- d2_vars_federal %>%
  filter(!region %in% c("Burgenland", "Kärnten", "Niederösterreich", "Vorarlberg"))

# save(d2_vars_federal, file = "data/d2_vars_federal.RData")

# Some analyses will also be computed with a smaller sample of survey participants
# that contains only the first 50% responders per survey wave  
n_wave_50 <- trunc(n_wave$`Number of participants` * 0.50)

d2_50 <- vector(mode = "list", length = 12)
for (i in 1:12){
  d2_50[[i]] <- d2 %>%
    filter(str_detect(wave, paste("Welle", i, "\\("))) %>%
    arrange(lastdata) %>%
    slice(1:n_wave_50[i])
}

d2_50 <- do.call(rbind, d2_50)

# transform into date object
d2_50$lastdata <- as.Date(d2_50$lastdata)

# save object
save(d2_50, file = "data/d2_50.RData")


d2_vars_50 <- d2_50 %>%
  group_by(wave) %>%
  summarize(anxiety = mean(anxiety), depression = mean(depression), 
            anger = mean(as.numeric(anger_last_week) - 1), 
            suicidal_thoughts = mean(suicidal_thoughts))

# rename first column (wave) into a Date object
date_mean <- rep(NA, 12)
for (i in 1:12){
  date_mean[i] <- d2_50 %>%
    filter(str_detect(wave, paste("Welle", i))) %>%
    slice(n = as.integer(round(500/2))) %>%
    select(lastdata)
}

date_mean <- do.call(c, date_mean)
date_mean <- as.Date(date_mean)

d2_vars_50[, 1] <- date_mean

save(d2_vars_50, file = "data/d2_vars_50.RData")


# variable mean per survey wave split by gender and federal state
# only include male and female gender for compatibility with twitter data
d2_50_gender <- d2_50 %>%
  filter(gender %in% c("male", "female")) %>%
  mutate(gender = factor(gender))

d2_vars_federal_50 <- d2_50_gender %>%
  group_by(wave, region, gender) %>%
  summarize(anxiety = mean(anxiety), depression = mean(depression), 
            anger = mean(as.numeric(anger_last_week) - 1), 
            suicidal_thoughts = mean(suicidal_thoughts)) %>% 
  ungroup()


# exclude Burgenland, Kärnten, Niederösterreich & Voralberg
d2_vars_federal_50 <- d2_vars_federal_50 %>%
  filter(!region %in% c("Burgenland", "Kärnten", "Niederösterreich", "Vorarlberg"))

save(d2_vars_federal_50, file = "data/d2_vars_federal_50.RData")
