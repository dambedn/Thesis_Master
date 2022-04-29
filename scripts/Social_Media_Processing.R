###########################################################################
# Script 1 - Import and transform data
###########################################################################

# In this script the social media-related data is imported and partially transformed 
# (dichotimized, only correct dates are filtered). These transformed variables are 
# saved to .RData objects. These can be loaded at the bottom of the script.

# Importantly, this script and further scripts concerning data pre-processing and
# baseline correction cannot be reproduced since we decided not to share the 
# social media data. Only aggregates of these measures will be made available.

###########################################################################
# Description Data
###########################################################################

# Derstandard documentation of postings in 2019 and 2020
# relevant twitter postings in 2019 and 2020 
# the last two data sources are categorized into meta-data, German Sentiment (GS) and
# Linguistic Inquiry and Word Count (LIWC) analyses
# GS categorizes posts into positive, negative or neutral.
# LIWC categorizes words associated with particular emotions


###########################################################################
# Import data
###########################################################################


# load libraries
library(data.table)
library(bit64)
# library(haven)

# 1. Import survey data
# path <- file.path(getwd(), "COVID19_survey_wave1-12.sav")
# dataset <- read_sav(path)



# 2: Import derstandard data, 2019 & 2020
filename_1 <- "derstandard_articles_2019_gs_liwc.tar.gz"
filename_2 <- "derstandard_articles_2020_gs_liwc.tar.gz"

# unzip .tar.gz file
untar(filename_1, list = TRUE) # lists contents without directly extracting them
untar(filename_1, exdir = "standard_2019_extracted")

untar(filename_2, list = TRUE)
untar(filename_2, exdir = "standard_2020_extracted")

# for extraction of specific component of .tar file use:
# untar("tmp.tar.gz",files="specific_file")


# import e.g. meta-data from Janu 2019: fread() is used for faster import
ts_meta_01_19 <- fread("standard_2019_extracted/cat_metadata_01_19")
head(ts_meta_01_19)

# import meta-data from Jan - Dec 2019

filename_2019 <- character(12)
meta_data_stand_2019 <- vector(mode = "list", length = 12)

for (i in 1:12){
  # define imported file's name
  filename_2019[i] <- file.path("standard_2019_extracted", paste0("cat_metadata_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_19"))
  # quote = "" to avoid warnings. some names are being quoted
  meta_data_stand_2019[[i]] <- fread(filename_2019[i], quote = "")
}


# import meta-data from Jan - Dec 2020
filename_2020 <- character(12)
meta_data_stand_2020 <- vector(mode = "list", length = 12)

for (i in 1:12){
  filename_2020[i] <- file.path("standard_2020_extracted", paste0("cat_metadata_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_20"))
  meta_data_stand_2020[[i]] <- fread(filename_2020[i], quote = "")
}



# transform timestamp to POSIXct to Date in meta_data
for (i in 1:12){
  meta_data_stand_2019[[i]]$timestamps <- as.Date(as.POSIXct(meta_data_stand_2019[[i]]$timestamps
                                                             , origin = "1970-01-01"))
}

for (i in 1:12){
  meta_data_stand_2020[[i]]$timestamps <- as.Date(as.POSIXct(meta_data_stand_2020[[i]]$timestamps
                                                             , origin = "1970-01-01"))
}

# list entries to dataframe
meta_data_stand_2019 <- do.call(rbind, meta_data_stand_2019)
meta_data_stand_2020 <- do.call(rbind, meta_data_stand_2020)


# save variables to avoid re-import
#save(meta_data_stand_2020, file = "meta_data_stand_20.RData")



# import _liwc standard: 

filename_2019 <- character(12)
liwc_stand_2019 <- vector(mode = "list", length = 12)

for (i in 1:12){
  # define imported file's name
  filename_2019[i] <- file.path("standard_2019_extracted", paste0("cat_text_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_19_liwc"))
  liwc_stand_2019[[i]] <- fread(filename_2019[i])
}



filename_2020 <- character(12)
liwc_stand_2020 <- vector(mode = "list", length = 12)

for (i in 1:12){
  filename_2020[i] <- file.path("standard_2020_extracted", paste0("cat_text_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_20_liwc"))
  liwc_stand_2020[[i]] <- fread(filename_2020[i])
}

for (i in 1:12){
  # exclude first row in every entry, since header was included in the analysis
  liwc_stand_2019[[i]] <- liwc_stand_2019[[i]][-1, ]
  liwc_stand_2020[[i]] <- liwc_stand_2020[[i]][-1, ]
}


# number_tokens is the number of words the text contained
# other columns: number of tokens that are in the respective category
# codes: 
#16      Negemo
#17      Anx
#18      Anger
#19      Sad
#31      Social
#13      Posemo
#200      Prosocial
# 
# change names
for (i in 1:12){
  names(liwc_stand_2019[[i]]) <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", "Prosocial", "number_tokens")
}
for (i in 1:12){
  names(liwc_stand_2020[[i]]) <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", "Prosocial", "number_tokens")
}

# list to dataframe
liwc_stand_2019 <- do.call(rbind, liwc_stand_2019)
liwc_stand_2020 <- do.call(rbind, liwc_stand_2020)

# Dichtomize liwc_scores --> 0 -> 0; 1 or <1 -> 1
liwc_stand_2019 <- as.data.frame(liwc_stand_2019)
liwc_stand_2019[, -8][liwc_stand_2019[, -8] > 0] <- 1
liwc_stand_2019 <- as.data.table(liwc_stand_2019)

liwc_stand_2020 <- as.data.frame(liwc_stand_2020)
liwc_stand_2020[, -8][liwc_stand_2020[, -8] > 0] <- 1
liwc_stand_2020 <- as.data.table(liwc_stand_2020)



# import _gs

filename_2019 <- character(12)
gs_stand_2019 <- vector(mode = "list", length = 12)

for (i in 1:12){
  # define imported file's name
  filename_2019[i] <- file.path("standard_2019_extracted", paste0("cat_text_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_19_gs"))
  gs_stand_2019[[i]] <- fread(filename_2019[i], header = FALSE)
}


filename_2020 <- character(12)
gs_stand_2020 <- vector(mode = "list", length = 12)

for (i in 1:12){
  filename_2020[i] <- file.path("standard_2020_extracted", paste0("cat_text_",
                                                                  ifelse(i < 10, paste0("0", i),
                                                                         i), "_20_gs"))
  gs_stand_2020[[i]] <- fread(filename_2020[i], header = FALSE)
}


for (i in 1:12){
  #exclude first row in every entry since header was included in analysis
  gs_stand_2019[[i]] <- gs_stand_2019[[i]][-1, ]
  gs_stand_2020[[i]] <- gs_stand_2020[[i]][-1, ]
}

# change column names to positive, negative and neutral
for (i in 1:12){
  names(gs_stand_2019[[i]]) <- c("positive", "negative", "neutral")
}

for (i in 1:12){
  names(gs_stand_2020[[i]]) <- c("positive", "negative", "neutral")
}

# from list to dataframe
gs_stand_2019 <- do.call(rbind, gs_stand_2019)
gs_stand_2020 <- do.call(rbind, gs_stand_2020)

# subset dates in meta_data_stand_2019 that refer to a date <"2019-01-01"
subset_date <- meta_data_stand_2019$timestamps < as.Date("2019-01-01")
meta_data_stand_2019 <- meta_data_stand_2019[!subset_date, ]
liwc_stand_2019 <- liwc_stand_2019[!subset_date, ]
gs_stand_2019 <- gs_stand_2019[!subset_date, ]

# subset dates in meta_data_stand_2019 that refer to a date >"2019-12-31"
subset_date <- meta_data_stand_2019$timestamps > as.Date("2019-12-31")
meta_data_stand_2019 <- meta_data_stand_2019[!subset_date, ]
liwc_stand_2019 <- liwc_stand_2019[!subset_date, ]
gs_stand_2019 <- gs_stand_2019[!subset_date, ]


# save variables to avoid re-import
# save(liwc_stand_2019, file = "liwc_stand_19.RData")
# save(liwc_stand_2020, file = "liwc_stand_20.RData")
# save(meta_data_stand_2019, file = "meta_data_stand_19.RData")
# save(gs_stand_2019, file = "gs_stand_19.RData")
# save(gs_stand_2020, file = "gs_stand_20.RData")




# 3: Import austriatweets 

# tweets 2020
filename_3 <- "austriatweets_gs_liwc.tar.gz"

# unzip .tar.gz file
untar(filename_3, list = TRUE) # lists contents without directly extracting them
untar(filename_3, exdir = "twitter_extracted")


# import _liwc files

filename_liwc <- character(5)
liwc_twitter <- vector(mode = "list", length = 5)

for (i in 1:5){
  # define imported file's name
  filename_liwc[i] <- file.path("twitter_extracted", paste0("AT",
                                                            i, "_text_liwc"))
  liwc_twitter[[i]] <- fread(filename_liwc[i])
}

# import _gs files

filename_gs <- character(5)
gs_twitter <- vector(mode = "list", length = 5)

for (i in 1:5){
  # define imported file's name
  filename_gs[i] <- file.path("twitter_extracted", paste0("AT",
                                                          i, "_text_gs"))
  gs_twitter[[i]] <- fread(filename_gs[i])
}


# change names
for (i in 1:5){
  names(gs_twitter[[i]]) <- c("positive", "negative", "neutral")
}

for (i in 1:5){
  names(liwc_twitter[[i]]) <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", "Prosocial", "number_tokens")
}




# Tweets 2020 orgafollowers
filename_5 <- "austriatweets_orgafollowers.tar.gz"

# unzip .tar.gz file
untar(filename_5, list = TRUE) 
untar(filename_5, exdir = "twitter_orgafollower_extracted")

# import AT files

filename_orga <- character(5)
follower_twitter <- vector(mode = "list", length = 5)

for (i in 1:5){
  # define imported file's name
  filename_orga[i] <- file.path("twitter_orgafollower_extracted", paste0("AT",
                                                                         i, "_followers"))
  follower_twitter[[i]] <- fread(filename_orga[i])
}


# save variable
# save(follower_twitter, file = "follower_twitter.RData")

# import orga data
filename_orga <- file.path("twitter_orgafollower_extracted", "AT_orga")
orga_twitter <- fread(filename_orga, col.names = c("id", "usercategory"),
                      stringsAsFactors = TRUE)
save(orga_twitter, file = "orga_twitter.RData")







# Tweets 2019 orgafollowers
filename_6 <- "austriatweets_2019_orgafollowers.tar.gz"
untar(filename_6, list = TRUE) 
untar(filename_6, exdir = "twitter_orgafollower_baseline_extracted")

#import
filename_follower_baseline <- file.path("twitter_orgafollower_baseline_extracted",
                                        "tweetdata.json_followers")

filename_orga_baseline <- file.path("twitter_orgafollower_baseline_extracted",
                                    "tweetdata.json_orga")

follower_twitter_baseline <- fread(filename_follower_baseline)

# use fread for int64 id's
orga_twitter_baseline <- fread(filename_orga_baseline, col.names = c("id", "usercategory"),
                               stringsAsFactors = TRUE)

# save variable
#save(follower_twitter_baseline, file = "follower_twitter_baseline.RData")
#save(orga_twitter_baseline, file = "orga_twitter_baseline.RData")



# import austriatweets_id_data_tr for 2020
filename <- "austriatweets_id_date.tar.gz"

# unzip .tar.gz file
untar(filename, list = TRUE) 
untar(filename, exdir = "date_id_extracted")

filename_date <- character(5)
filename_id <- character(5)
id_twitter_2020 <- vector(mode = "list", length = 5)
date_twitter_2020 <- vector(mode = "list", length = 5)

for (i in 1:5){
  # define imported file's name
  filename_date[i] <- file.path("date_id_extracted", paste0("AT",
                                                            i, "_date"))
  date_twitter_2020[[i]] <- fread(filename_date[i], sep = "", header = FALSE)
  filename_id[i] <- file.path("date_id_extracted", paste0("AT",
                                                          i, "_id"))
  id_twitter_2020[[i]] <- fread(filename_id[i])
}

# transform character vectors in list date_twitter_2020 into Date object

for (i in 1:5){
  date_twitter_2020[[i]] <- as.Date(date_twitter_2020[[i]]$V1, format = "%a %b %d %H:%M:%S +0000 %Y")
}



# Exclude organizations and posts of users with <100 or >5000 followers
# exlcude 2020 values when matching the exclusion criteria
subset_orga <- orga_twitter$usercategory == "organisational"
orga_id <- orga_twitter$id[subset_orga]

for (i in 1:5){
  subset_orga <- id_twitter_2020[[i]]$V1 %in% orga_id
  subset_exclude <- subset_orga | (follower_twitter[[i]]$V1 < 100) |
    (follower_twitter[[i]]$V1 > 5000)
  date_twitter_2020[[i]] <- date_twitter_2020[[i]][!subset_exclude]
  gs_twitter[[i]] <- gs_twitter[[i]][!subset_exclude, ]
  liwc_twitter[[i]] <- liwc_twitter[[i]][!subset_exclude, ]
  id_twitter_2020[[i]] <- id_twitter_2020[[i]]$V1[!subset_exclude]
}



# combine list elements to one data frame
gs_twitter <- do.call(rbind, gs_twitter)
liwc_twitter <- do.call(rbind, liwc_twitter)
date_twitter_2020 <- do.call(c, date_twitter_2020)
id_twitter_2020 <- do.call(c, id_twitter_2020)

# Dichtomize liwc_scores --> 0 -> 0; 1 or <1 -> 1
liwc_twitter <- as.data.frame(liwc_twitter)
liwc_twitter[, -8][liwc_twitter[, -8] > 0] <- 1
liwc_twitter <- as.data.table(liwc_twitter)

# save variables
# save(gs_twitter, file = "gs_twitter.RData")
# save(liwc_twitter, file = "liwc_twitter.RData")
# save(id_twitter_2020, file = "id_twitter_2020.RData")
# save(date_twitter_2020, file = "date_twitter_2020.RData")



# import dates and id for 2019
filename <- file.path("twitter_baseline_extracted", "tweetdata.json_date")
date_twitter_baseline <- fread(filename, sep = "", header = FALSE)

filename_2 <- file.path("twitter_baseline_extracted", "tweetdata.json_id")

id_twitter_baseline <- fread(filename_2)

# Tweets from 2019 for baseline correction
filename_4 <- "austriatweets_2019.tar.gz"

# unzip .tar.gz file
untar(filename_4, list = TRUE) # lists contents without directly extracting them
untar(filename_4, exdir = "twitter_baseline_extracted")


# gs files and liwc files not separated by months anymore; load both
# import liwc first
filename_base_liwc <- file.path("twitter_baseline_extracted", "tweetdata.json_text_liwc")
liwc_twitter_baseline <- fread(filename_base_liwc)

# change names
names(liwc_twitter_baseline) <- c("Posemo", "Negemo", "Anx", "Anger", "Sad", "Social", "Prosocial", "number_tokens")



# import gs baseline file
filename_base_gs <- file.path("twitter_baseline_extracted", "tweetdata.json_text_gs")
gs_twitter_baseline <- fread(filename_base_gs)
head(gs_twitter_baseline)

#change names
names(gs_twitter_baseline) <- c("positive", "negative", "neutral")

# change date_twitter_baseline data into a Date object
date_twitter_baseline <- as.Date(date_twitter_baseline$V1,
                                 format = "%a %b %d %H:%M:%S +0000 %Y")


# exclude 2018 and 2020 data in baseline variables
subset_2018 <- date_twitter_baseline < as.Date("2019-01-01")
subset_2020 <- date_twitter_baseline > as.Date("2019-12-31")
subset_exclude <- subset_2018 | subset_2020
id_twitter_baseline <- id_twitter_baseline$V1[!subset_exclude]
date_twitter_baseline <- date_twitter_baseline[!subset_exclude ]
gs_twitter_baseline <- gs_twitter_baseline[!subset_exclude, ]
liwc_twitter_baseline <- liwc_twitter_baseline[!subset_exclude, ]
follower_twitter_baseline <- follower_twitter_baseline$V1[!subset_exclude]

# exclude organizations as defined by Brandwatch and exclude posts of users with <100 or >5000 followers
# nrow(orga_twitter)
# nrow(orga_twitter_baseline)

# id_twitter_baseline matches in length with gs or liwc baseline scores BUT orga_twitter_baseline 
# does not match. Hence, match the id's (first column of orga_twitter_baseline) with 
# the id's in id_twitter_baseline and furthermore date_baseline and the sentiment scores

subset_orga <- orga_twitter_baseline$usercategory == "organisational"
orga_id <- orga_twitter_baseline$id[subset_orga]
subset_orga <- id_twitter_baseline %in% orga_id

# exclude baseline values categorized as originating from organizations or users 
# in the exclusion follower range
subset_exclude <- (subset_orga) | (follower_twitter_baseline < 100) |
  (follower_twitter_baseline > 5000)
date_twitter_baseline <- date_twitter_baseline[!subset_exclude]
gs_twitter_baseline <- gs_twitter_baseline[!subset_exclude, ]
liwc_twitter_baseline <- liwc_twitter_baseline[!subset_exclude, ]
id_twitter_baseline <- id_twitter_baseline[!subset_exclude]

# Dichtomize liwc_scores --> 0 -> 0; 1 or <1 -> 1
liwc_twitter_baseline <- as.data.frame(liwc_twitter_baseline)
liwc_twitter_baseline[, -8][liwc_twitter_baseline[, -8] > 0] <- 1
liwc_twitter_baseline <- as.data.table(liwc_twitter_baseline)

#save variables; 
# save(gs_twitter_baseline, file = "gs_twitter_baseline.RData")
# save(liwc_twitter_baseline, file = "liwc_twitter_baseline.RData")
# save(id_twitter_baseline, file = "id_twitter_baseline.RData")
# save(date_twitter_baseline, file = "date_twitter_baseline.RData")
# save(follower_twitter_baseline, file = "follower_twitter_baseline.RData")



# import id, gender, and federal state data of twitter posts 2019 & 2020


gender_region_twitter_2020 <- fread("tweetdata_2020_id_gender_region",
                                    na.strings = "",
                                    col.names = c("id", "gender", "federal_state"),
                                    stringsAsFactors = TRUE)

gender_region_twitter_2019 <- fread("tweetdata_2019_id_gender_region",
                                    na.strings = "",
                                    col.names = c("id", "gender", "federal_state"),
                                    stringsAsFactors = TRUE)

# import the 2020 November and December data
gender_region_twitter_2020_rest <- fread("tweetdata_2020_novdec_id_gender_region_orgafollowers",
                                         na.strings = "",
                                         col.names = c("id", "gender", "federal_state", "user_category"),
                                         stringsAsFactors = TRUE)
# save variables
# save(gender_region_twitter_2020_rest, file = "gender_region_twitter_2020_rest.RData")
# save(gender_region_twitter_2020, file = "gender_region_twitter_2020.RData")
# save(gender_region_twitter_2019, file = "gender_region_twitter_2019.RData")

###############################################################################
# load Data for further analyses
# set wd

# library(data.table)
# library(bit64)


# # load meta_data_stand_2019/2020
# load("meta_data_stand_19.RData")
# load("meta_data_stand_20.RData")
# 
# # load liwc_stand_2019/2020
# load("liwc_stand_19.RData")
# load("liwc_stand_20.RData")
# 
# # load gs_stand_2019/2020
# load("gs_stand_19.RData")
# load("gs_stand_20.RData")
# 
# # load tweets_liwc from 2020
# load("liwc_twitter.RData")
# 
# # load tweets_gs from 2020
# load("gs_twitter.RData")
# 
# # load tweets_liwc from 2019 - baseline
# load("liwc_twitter_baseline.RData")
# 
# # load tweets_gs from 2019 - baseline
# load("gs_twitter_baseline.RData")
# 
# # load twitter orgafollower data 2020
# load("follower_twitter.RData")
# load("orga_twitter.RData")
# 
# # load twitter orgafollower baseline data 2019
# load("follower_twitter_baseline.RData")
# load("orga_twitter_baseline.RData")
# 
# # load twitter dates & ids 2020
# load("id_twitter_2020.RData")
# load("date_twitter_2020.RData")
# 
# # load twitter dates & ids 2019/baseline 
# load("id_twitter_baseline.RData")
# load("date_twitter_baseline.RData")
# 
# # load id, gender, and federal state data of twitter posts 2019 & 2020
# load("gender_region_twitter_2019.RData")
# load("gender_region_twitter_2020.RData")
# load("gender_region_twitter_2020_rest.RData")
# 
# ###############################################################################




