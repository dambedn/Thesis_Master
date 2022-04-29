###########################################################################
# Script 5 - Baseline correction of sentiment analyses data - for different
# time windows
###########################################################################
# again: only resulting aggregates made available

# read in library
library(tidyverse)

###########################################################################
# Step 1 - Calculate the time span defined as in the pre-registration of 
# each survey wave: 2 days prior to the first participant answering the questionnaire. 
# Until 50% of the participants finished the survey
###########################################################################

# first: identify time spans for surveys: from start (referring back 1/2 weeks) 
# until 50% of the participants finished the survey
# indicator when survey has been finished: lastdata

load("data/d2_50.RData")
#attach(d2_50)

date_list <- vector(mode = "list", 12)
dates <- as.Date(d2_50$lastdata)

# get an overview of response structure
#hist(dates, breaks = 200)


for (i in 1:12){
  date_list[[i]] <- dates[str_detect(d2_50$wave, paste("Welle", i, "\\("))]
}

range_date <- vector(mode = "list", length = 12)

# par(mfrow = (c(3, 4)))
for (i in 1:12){
  # hist(date_list[[i]], breaks = 15,main = paste0("Welle ", i), col = "blue", freq = TRUE)
  range_date[[i]] <- range(date_list[[i]])
}

# save range_date as range_date_50 for later usage
range_date_50 <- range_date
save(range_date_50, file = "data/range_date_50.RData")



# determine time point when 50% of the participants finished the questionnaire
# date_list_2 <- vector(mode = "list", 12)
# for (i in 1:12){
#   date_list_2[[i]] <- dates %>%
#     "["(welle == i)
#   date_list_2[[i]] <- unique(date_list_2[[i]])
#   date_list_2[[i]] <- c(date_list_2[[i]][1] - 2:1, date_list_2[[i]])
#   date_list_2[[i]] <- data.frame(date = date_list_2[[i]], Wave = rep(i, 
#                                                                      length(date_list_2[[i]])))
# }


###########################################################################
# Step 2 - Baseline-corrrection liwc & gs DerStandard data
###########################################################################

load("data/gs_stand_corrected.RData")
load("data/liwc_stand_corrected.RData")


# For further analyses, compute different baseline-corrected values for different time spans. 
# initialize lists 
liwc_stand_corrected_time_50 <- vector(mode = "list", length = 14)
gs_stand_corrected_time_50 <- vector(mode = "list", length = 14)
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
  
  gs_stand_corrected_time_50[[j]] <- inner_join(gs_stand_corrected,
                                                range_date_waves, by = "date")
  
  liwc_stand_corrected_time_50[[j]] <- inner_join(liwc_stand_corrected, 
                                                  range_date_waves, by = "date")
  
  gs_stand_corrected_time_50[[j]] <- gs_stand_corrected_time_50[[j]] %>%
    group_by(Wave) %>%
    summarize(positive = mean(positive), negative = mean(negative),
              positive.corrected = mean(positive.corrected),
              negative.corrected = mean(negative.corrected))
  
  liwc_stand_corrected_time_50[[j]] <- liwc_stand_corrected_time_50[[j]] %>%
    group_by(Wave) %>%
    summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
              Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
              Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
              Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
              Social.corrected = mean(Social.corrected))
  
}
rm(range_date_waves_df)
rm(range_date_waves)

save(liwc_stand_corrected_time_50, file = "data/liwc_stand_corrected_time_50.RData")
save(gs_stand_corrected_time_50, file = "data/gs_stand_corrected_time_50.RData")


###########################################################################
# Step 5 - Baseline-correction liwc & gs Twitter data (see script baseline_correction.R for comparison)
###########################################################################

load("data/gs_twitter_corrected.RData")
load("data/liwc_twitter_corrected.RData")

# For further analyses, compute different baseline-corrected values for different time spans. 
# initialize lists 
liwc_twitter_corrected_time_50 <- vector(mode = "list", length = 14)
gs_twitter_corrected_time_50 <- vector(mode = "list", length = 14)
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
  
  gs_twitter_corrected_time_50[[j]] <- inner_join(gs_twitter_corrected,
                                                  range_date_waves, by = "date")
  
  liwc_twitter_corrected_time_50[[j]] <- inner_join(liwc_twitter_corrected, 
                                                    range_date_waves, by = "date")
  
  gs_twitter_corrected_time_50[[j]] <- gs_twitter_corrected_time_50[[j]] %>%
    group_by(Wave) %>%
    summarize(positive = mean(positive), negative = mean(negative),
              positive.corrected = mean(positive.corrected),
              negative.corrected = mean(negative.corrected))
  
  liwc_twitter_corrected_time_50[[j]] <- liwc_twitter_corrected_time_50[[j]] %>%
    group_by(Wave) %>%
    summarize(Posemo = mean(Posemo), Negemo = mean(Negemo), Anx = mean(Anx), Anger = mean(Anger),
              Sad = mean(Sad), Social = mean(Social), Posemo.corrected = mean(Posemo.corrected),
              Negemo.corrected = mean(Negemo.corrected), Anx.corrected = mean(Anx.corrected),
              Anger.corrected = mean(Anger.corrected), Sad.corrected = mean(Sad.corrected),
              Social.corrected = mean(Social.corrected))
  
}
rm(range_date_waves_df)
rm(range_date_waves)


save(liwc_twitter_corrected_time_50, file = "data/liwc_twitter_corrected_time_50.RData")
save(gs_twitter_corrected_time_50, file = "data/gs_twitter_corrected_time_50.RData")



