
###########################################################################
# Script 3 - Baseline correction of sentiment analyses data
###########################################################################
# Aggregates of this script will be made available

# read in library
library(tidyverse)

###########################################################################
# Step 1 - Calculate the time span defined as in the pre-registration of 
# each survey wave: For anger & anxiety: 7 days prior to the start of the 
# survey; for depression: 14 days prior. Until 75% of the participants
# finish the survey
###########################################################################


# first: identify time spans for surveys: from start (referring back 1/2 weeks) 
# until 75% of the participants finished the survey
# indicator when survey has been finished: lastdata


# rows of data-frame d2_75 are already sorted by variable "lastdate"
# Firstly, compute ranges of the dates of each survey wave.
load("data/d2_75.RData")
#attach(d2_75)

date_list <- vector(mode = "list", 12)
dates <- as.Date(d2_75$lastdata)

# get an overview of response structure
#hist(dates, breaks = 200)


for (i in 1:12){
  date_list[[i]] <- dates[str_detect(d2_75$wave, paste("Welle", i, "\\("))]
}

range_date <- vector(mode = "list", length = 12)

# par(mfrow = (c(3, 4)))
for (i in 1:12){
  # hist(date_list[[i]], breaks = 15,main = paste0("Welle ", i), col = "blue", freq = TRUE)
  range_date[[i]] <- range(date_list[[i]])
}

# save range_date as range_date_75 for later usage
range_date_75 <- range_date
save(range_date_75, file = "data/range_date_75.RData")

# next step: alter ranges respectively for depressive scales (-2 weeks) as well 
# as anxiety and anger scales (-1 week)

range_date_depr <- range_date
range_date_anxi <- range_date

for(i in 1:12){
  range_date_depr[[i]][1] <- range_date[[i]][1] - 14
  range_date_anxi[[i]][1] <- range_date[[i]][1] - 7
}

# first, create two dataframes where the first column displays the survey wave dates and the second 
# column the according wave. This is done respectively for the depression and the anxiety/anger
# data: these dataframes are also going to be used in the upcoming baseline-corrections
range_date_depr_waves <- NULL
for (i in 1:12){
  range_date_depr_waves_df <- data.frame(Date = range_date_depr[[i]][1] + 0:diff(range_date_depr[[i]]),
                                         Wave = rep(i, diff(range_date_depr[[i]]) + 1))
  range_date_depr_waves <- rbind(range_date_depr_waves, range_date_depr_waves_df)
}
names(range_date_depr_waves)[1] <- "date"
rm(range_date_depr_waves_df)

range_date_anx_waves <- NULL
for (i in 1:12){
  range_date_anx_waves_df <- data.frame(Date = range_date_anxi[[i]][1] + 0:diff(range_date_anxi[[i]]),
                                        Wave = rep(i, diff(range_date_anxi[[i]]) + 1))
  range_date_anx_waves <- rbind(range_date_anx_waves, range_date_anx_waves_df)
}
names(range_date_anx_waves)[1] <- "date"
rm(range_date_anx_waves_df)
rm(range_date_depr)
rm(range_date_anxi)

###########################################################################
# Step 2 - Look at the distribution of the gs negative emotion scores in order
# to decide where to incorporate a threshold
###########################################################################
par(mfrow = c(1, 1))
# For gs_twitter 2020 data
hist(gs_twitter$negative, breaks = 40, main = "gs_twitter",
     col = "red", xlab = "Probability")


# For gs_stand 2020 data
hist(gs_stand_2020$negative, breaks = 40, main = "gs_stand",
     col = "blue", xlab = "Probability")


# Hence, in order to preprocess the gs_data we define a threshold of >0.9
# The liwc data will be dichotimized, in a way that >1 --> 1 and 0 --> 0

# for positive
# For gs_twitter 2020 data
hist(gs_twitter$positive, breaks = 40, main = "gs_twitter",
     col = "red", xlab = "Probability")


# For gs_stand 2020 data
hist(gs_stand_2020$positive, breaks = 40, main = "gs_stand",
     col = "blue", xlab = "Probability")



###########################################################################
# Step 3 - Baseline-corrrection liwc & gs DerStandard data
###########################################################################
# cbind dates to gs_stand and liwc_stand objects

# date_stand_2019 / date_stand_2020 was defined as <- meta_data_stand_2019/20$timestamps
# cbind date objects to gs and liwc 2019/2020 objects
liwc_stand_2019 <- cbind(liwc_stand_2019, date_stand_2019)
names(liwc_stand_2019)[9] <- "date"

liwc_stand_2020 <- cbind(liwc_stand_2020, date_stand_2020)
names(liwc_stand_2020)[9] <- "date"

gs_stand_2019 <- cbind(gs_stand_2019, date_stand_2019)
names(gs_stand_2019)[4] <- "date"

gs_stand_2020 <- cbind(gs_stand_2020, date_stand_2020)
names(gs_stand_2020)[4] <- "date"


# compute the sum of the scores at a daily level and divide by the total number of tweets that day
# --> take the mean grouped by date 

liwc_stand_2019_df <- liwc_stand_2019 %>%
  group_by(date) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

liwc_stand_2020_df <- liwc_stand_2020 %>%
  group_by(date) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))


gs_stand_2019_df <- gs_stand_2019 %>%
  group_by(date) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))

gs_stand_2020_df <- gs_stand_2020 %>%
  group_by(date) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))

# exclude 2021 data
gs_stand_2020_df <- gs_stand_2020_df %>%
  filter(date < "2021-01-01")
liwc_stand_2020_df <- liwc_stand_2020_df %>%
  filter(date < "2021-01-01")


# to further baseline correct these values according to the weekday in 2019, add
# a column that categorizes the dates into weekdays

liwc_stand_2020_df <- liwc_stand_2020_df %>%
  mutate(weekdays = format(date, format = "%u"))

gs_stand_2020_df <- gs_stand_2020_df %>%
  mutate(weekdays = format(date, format = "%u"))

liwc_stand_2019_df <- liwc_stand_2019_df %>%
  mutate(weekdays = format(date, format = "%u"))

gs_stand_2019_df <- gs_stand_2019_df %>%
  mutate(weekdays = format(date, format = "%u"))

# for baseline correction: calculate the mean for each weekday in 2019
liwc_stand_2019_df <- liwc_stand_2019_df %>%
  group_by(weekdays) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

gs_stand_2019_df <- gs_stand_2019_df %>%
  group_by(weekdays) %>%
  summarize(positive = mean(positive), negative = mean(negative))


# subtract and divide mean to baseline correct 2020 data
liwc_stand_corrected <- left_join(liwc_stand_2020_df, liwc_stand_2019_df, by = "weekdays")

for (i in 2:7){
  liwc_stand_corrected[, i + 7] <- (liwc_stand_corrected[, i] - liwc_stand_corrected[, i + 7]) / liwc_stand_corrected[, i + 7]
}
# change names
names(liwc_stand_corrected)[c(2:7, 9:14)] <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", 
                                               "Posemo.corrected", "Negemo.corrected", 
                                               "Anx.corrected", "Anger.corrected", "Sad.corrected",
                                               "Social.corrected")


gs_stand_corrected <- left_join(gs_stand_2020_df, gs_stand_2019_df, by = "weekdays")

for (i in 2:3){
  gs_stand_corrected[, i + 3] <- (gs_stand_corrected[, i] - gs_stand_corrected[, i + 3]) / gs_stand_corrected[, i + 3]
}
# change names
names(gs_stand_corrected)[c(2, 3, 5, 6)] <- c("positive", "negative", 
                                              "positive.corrected", "negative.corrected")

# save daily baseline-corrected values for time series plots
save(gs_stand_corrected, file = "data/gs_stand_corrected.RData")
save(liwc_stand_corrected, file = "data/liwc_stand_corrected.RData")
#load("data/gs_stand_corrected.RData")
#load("data/liwc_stand_corrected.RData")

# join these two dataframes with the range_date_depr/anx_waves dataframes
gs_stand_corrected_depr <- inner_join(gs_stand_corrected,
                                      range_date_depr_waves, by = "date")

gs_stand_corrected_anx <- inner_join(gs_stand_corrected,
                                     range_date_anx_waves, by = "date")

liwc_stand_corrected_depr <- inner_join(liwc_stand_corrected, 
                                        range_date_depr_waves, by = "date")

liwc_stand_corrected_anx <- inner_join(liwc_stand_corrected, 
                                       range_date_anx_waves, by = "date")

# now: these dataframes can be grouped by wave and a grouped mean can be calculated 
gs_stand_corrected_depr <- gs_stand_corrected_depr %>%
  group_by(Wave) %>%
  summarize(positive = mean(positive), negative = mean(negative),
            positive.corrected = mean(positive.corrected),
            negative.corrected = mean(negative.corrected))

gs_stand_corrected_anx <- gs_stand_corrected_anx %>%
  group_by(Wave) %>%
  summarize(positive = mean(positive), negative = mean(negative),
            positive.corrected = mean(positive.corrected),
            negative.corrected = mean(negative.corrected))

liwc_stand_corrected_depr <- liwc_stand_corrected_depr %>%
  group_by(Wave) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
            Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
            Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
            Social.corrected = mean(Social.corrected))

liwc_stand_corrected_anx <- liwc_stand_corrected_anx %>%
  group_by(Wave) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
            Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
            Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
            Social.corrected = mean(Social.corrected))

# save baseline-corrected variables
save(gs_stand_corrected_anx, file = "data/gs_stand_corrected_anx.RData")
save(gs_stand_corrected_depr, file = "data/gs_stand_corrected_depr.RData")
save(liwc_stand_corrected_anx, file = "data/liwc_stand_corrected_anx.RData")
save(liwc_stand_corrected_depr, file = "data/liwc_stand_corrected_depr.RData")


# For further analyses, compute different baseline-corrected values for different time windows 
# for which we included social media data prior to each survey wave to compute emotion and 
# sentiment measures.

# initialize lists 
liwc_stand_corrected_time <- vector(mode = "list", length = 14)
gs_stand_corrected_time <- vector(mode = "list", length = 14)
range_date_2 <- range_date

for(j in 1:14){
  range_date_waves <- NULL
  for(i in 1:12){
    range_date_2[[i]][1] <- range_date[[i]][1] - j
    
    # first, create two dataframes where the first column displays the survey wave dates and the second 
    # column the according wave.
    range_date_waves_df <- data.frame(Date = range_date_2[[i]][1] + 0:diff(range_date_2[[i]]),
                                      Wave = rep(i, diff(range_date_2[[i]]) + 1))
    range_date_waves <- rbind(range_date_waves, range_date_waves_df)
  }
  names(range_date_waves)[1] <- "date"
  
  gs_stand_corrected_time[[j]] <- inner_join(gs_stand_corrected,
                                             range_date_waves, by = "date")
  
  liwc_stand_corrected_time[[j]] <- inner_join(liwc_stand_corrected, 
                                               range_date_waves, by = "date")
  
  gs_stand_corrected_time[[j]] <- gs_stand_corrected_time[[j]] %>%
    group_by(Wave) %>%
    summarize(positive = mean(positive), negative = mean(negative),
              positive.corrected = mean(positive.corrected),
              negative.corrected = mean(negative.corrected))
  
  liwc_stand_corrected_time[[j]] <- liwc_stand_corrected_time[[j]] %>%
    group_by(Wave) %>%
    summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
              Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
              Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
              Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
              Social.corrected = mean(Social.corrected))
  
}
rm(range_date_waves_df)
rm(range_date_waves)

save(liwc_stand_corrected_time, file = "data/liwc_stand_corrected_time.RData")
save(gs_stand_corrected_time, file = "data/gs_stand_corrected_time.RData")

# e.g. liwc_stand_corrected_time[[14]] == liwc_stand_corrected_depr # TRUE

###########################################################################
# Step 6 - Baseline-correction liwc & gs Twitter data
###########################################################################


# cbind dates to gs_twitter and liwc_twitter objects
liwc_twitter_baseline <- cbind(liwc_twitter_baseline, date_twitter_baseline)
names(liwc_twitter_baseline)[9] <- "date"

gs_twitter_baseline <- cbind(gs_twitter_baseline, date_twitter_baseline)
names(gs_twitter_baseline)[4] <- "date"

liwc_twitter <- cbind(liwc_twitter, date_twitter_2020)
names(liwc_twitter)[9] <- "date"

gs_twitter <- cbind(gs_twitter, date_twitter_2020)
names(gs_twitter)[4] <- "date"


# compute the sum of the scores at a daily level and divide by the total number of tweets that day
# --> take the mean grouped by date 

liwc_twitter_baseline_df <- liwc_twitter_baseline %>%
  group_by(date) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

liwc_twitter_df <- liwc_twitter %>%
  group_by(date) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

gs_twitter_baseline_df <- gs_twitter_baseline %>%
  group_by(date) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))

gs_twitter_df <- gs_twitter %>%
  group_by(date) %>%
  summarize(positive = mean(positive > 0.9), negative = mean(negative > 0.9))



# to further baseline correct these values according to the weekday in 2019, add
# a column that categorizes the dates into weekdays

liwc_twitter_df <- liwc_twitter_df %>%
  mutate(weekdays = format(date, format = "%u"))

gs_twitter_df <- gs_twitter_df %>%
  mutate(weekdays = format(date, format = "%u"))

liwc_twitter_baseline_df <- liwc_twitter_baseline_df %>%
  mutate(weekdays = format(date, format = "%u"))

gs_twitter_baseline_df <- gs_twitter_baseline_df %>%
  mutate(weekdays = format(date, format = "%u"))


# for baseline correction: calculate the mean for each weekday in 2019
liwc_twitter_baseline_df <- liwc_twitter_baseline_df %>%
  group_by(weekdays) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social))

gs_twitter_baseline_df <- gs_twitter_baseline_df %>%
  group_by(weekdays) %>%
  summarize(positive = mean(positive), negative = mean(negative))

# subtract and divide mean to baseline correct 2020 data
liwc_twitter_corrected <- left_join(liwc_twitter_df, liwc_twitter_baseline_df, by = "weekdays")

for (i in 2:7){
  liwc_twitter_corrected[, i + 7] <- (liwc_twitter_corrected[, i] - liwc_twitter_corrected[, i + 7]) / liwc_twitter_corrected[, i + 7]
}
# change names
names(liwc_twitter_corrected)[c(2:7, 9:14)] <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", 
                                                 "Posemo.corrected", "Negemo.corrected", 
                                                 "Anx.corrected", "Anger.corrected", "Sad.corrected",
                                                 "Social.corrected")


gs_twitter_corrected <- left_join(gs_twitter_df, gs_twitter_baseline_df, by = "weekdays")
for (i in 2:3){
  gs_twitter_corrected[, i + 3] <- (gs_twitter_corrected[, i] - gs_twitter_corrected[, i + 3]) / gs_twitter_corrected[, i + 3]
}

# change names
names(gs_twitter_corrected)[c(2, 3, 5, 6)] <- c("positive", "negative", 
                                                "positive.corrected", "negative.corrected")

# save basline corrected daily values for time series plots
gs_twitter_corrected <- gs_twitter_corrected %>%
  filter(date > "2019-12-31")

liwc_twitter_corrected <- liwc_twitter_corrected %>%
  filter(date > "2019-12-31")

save(gs_twitter_corrected, file = "data/gs_twitter_corrected.RData")
save(liwc_twitter_corrected, file = "data/liwc_twitter_corrected.RData")
# load("data/gs_twitter_corrected.RData")
# load("data/liwc_twitter_corrected.RData")

# join these two dataframes with the range_date_depr/anx_waves dataframes
gs_twitter_corrected_depr <- inner_join(gs_twitter_corrected,
                                        range_date_depr_waves, by = "date")

gs_twitter_corrected_anx <- inner_join(gs_twitter_corrected,
                                       range_date_anx_waves, by = "date")

liwc_twitter_corrected_depr <- inner_join(liwc_twitter_corrected, 
                                          range_date_depr_waves, by = "date")

liwc_twitter_corrected_anx <- inner_join(liwc_twitter_corrected, 
                                         range_date_anx_waves, by = "date")


# these dataframes can be grouped by wave and a grouped mean can be calculated 
gs_twitter_corrected_depr <- gs_twitter_corrected_depr %>%
  group_by(Wave) %>%
  summarize(positive = mean(positive), negative = mean(negative),
            positive.corrected = mean(positive.corrected),
            negative.corrected = mean(negative.corrected))


gs_twitter_corrected_anx <- gs_twitter_corrected_anx %>%
  group_by(Wave) %>%
  summarize(positive = mean(positive), negative = mean(negative),
            positive.corrected = mean(positive.corrected),
            negative.corrected = mean(negative.corrected))

liwc_twitter_corrected_depr <- liwc_twitter_corrected_depr %>%
  group_by(Wave) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
            Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
            Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
            Social.corrected = mean(Social.corrected))

liwc_twitter_corrected_anx <- liwc_twitter_corrected_anx %>%
  group_by(Wave) %>%
  summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
            Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
            Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
            Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
            Social.corrected = mean(Social.corrected))


# save baseline-corrected variables
save(gs_twitter_corrected_anx, file = "data/gs_twitter_corrected_anx.RData")
save(gs_twitter_corrected_depr, file = "data/gs_twitter_corrected_depr.RData")
save(liwc_twitter_corrected_anx, file = "data/liwc_twitter_corrected_anx.RData")
save(liwc_twitter_corrected_depr, file = "data/liwc_twitter_corrected_depr.RData")



# For further analyses, compute different baseline-corrected values for different time windows. 
# initialize lists 
liwc_twitter_corrected_time <- vector(mode = "list", length = 14)
gs_twitter_corrected_time <- vector(mode = "list", length = 14)
range_date_2 <- range_date

for(j in 1:14){
  range_date_waves <- NULL
  for(i in 1:12){
    range_date_2[[i]][1] <- range_date[[i]][1] - j
    
    # first, create two dataframes where the first column displays the survey wave dates and the second 
    # column the according wave.
    range_date_waves_df <- data.frame(Date = range_date_2[[i]][1] + 0:diff(range_date_2[[i]]),
                                      Wave = rep(i, diff(range_date_2[[i]]) + 1))
    range_date_waves <- rbind(range_date_waves, range_date_waves_df)
  }
  names(range_date_waves)[1] <- "date"
  
  gs_twitter_corrected_time[[j]] <- inner_join(gs_twitter_corrected,
                                               range_date_waves, by = "date")
  
  liwc_twitter_corrected_time[[j]] <- inner_join(liwc_twitter_corrected, 
                                                 range_date_waves, by = "date")
  
  gs_twitter_corrected_time[[j]] <- gs_twitter_corrected_time[[j]] %>%
    group_by(Wave) %>%
    summarize(positive = mean(positive), negative = mean(negative),
              positive.corrected = mean(positive.corrected),
              negative.corrected = mean(negative.corrected))
  
  liwc_twitter_corrected_time[[j]] <- liwc_twitter_corrected_time[[j]] %>%
    group_by(Wave) %>%
    summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
              Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
              Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
              Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
              Social.corrected = mean(Social.corrected))
  
}
rm(range_date_waves_df)
rm(range_date_waves)


save(liwc_twitter_corrected_time, file = "data/liwc_twitter_corrected_time.RData")
save(gs_twitter_corrected_time, file = "data/gs_twitter_corrected_time.RData")
