# Brexit Explore V1 


# Exploratory Data Analysis 

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")


# Beg Aug
brexit_1 <- read.csv("brexit_tweets_08130814.csv")
brexit_2 <- read.csv("brexit_tweets_08110812.csv")
brexit_3 <- read.csv("brexit_tweets_08090811.csv")
brexit_4 <- read.csv("brexit_tweets_08070808.csv")
brexit_5 <- read.csv("brexit_tweets_08060807.csv")
brexit_6 <- read.csv("brexit_tweets_08050806.csv")

# End Aug
brexit_7 <- read.csv("brexit_tweets_08290830.csv")
brexit_8 <- read.csv("brexit_tweets_0828829.csv")
brexit_9 <- read.csv("brexit_tweets_0827828.csv")
brexit_10 <- read.csv("brexit_tweets_0826827.csv")
brexit_11 <- read.csv("brexit_tweets_0825826.csv")
brexit_12 <- read.csv("brexit_tweets_0824825.csv")
brexit_13 <- read.csv("brexit_tweets_0823824.csv")
brexit_14 <- read.csv("brexit_tweets_0822823.csv")
brexit_15 <- read.csv("brexit_tweets_0821822.csv")

# Sep
brexit_16 <- read.csv("brexit_tweets_09170918.csv")
brexit_17 <- read.csv("brexit_tweets_09160917.csv")
brexit_18 <- read.csv("brexit_tweets_09150916.csv")
brexit_19 <- read.csv("brexit_tweets_09140915.csv")
brexit_20 <- read.csv("brexit_tweets_09130914.csv")
brexit_21 <- read.csv("brexit_tweets_09120913.csv")
brexit_22 <- read.csv("brexit_tweets_09110912.csv")
brexit_23 <- read.csv("brexit_tweets_09100911.csv")
brexit_24 <- read.csv("brexit_tweets_09090910.csv")

brexit.labs <- read.csv("brexit_data_labelled_scale_full_300.csv")

library(dplyr)
library(tidyr)

brexit_all <- bind_rows(list(brexit_1,brexit_2,brexit_3,brexit_4,brexit_5,brexit_6,brexit_7,brexit_8,
                             brexit_9,brexit_10,brexit_11,brexit_12,brexit_13,brexit_14,brexit_15,brexit_16,
                             brexit_17,brexit_18,brexit_19,brexit_20,brexit_21,brexit_22,brexit_23,brexit_24))

brexit_all_d <- brexit_all %>% 
  distinct(user_id, status_id, .keep_all=T) %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x", remove = FALSE) %>% 
  filter(lang == "en")


# Total number of users 

## with and without distress 
master_person <- distinct(brexit_all_d, user_id, .keep_all = T) 
nrow(master_person) # 83,489

# Total number of posts 
master_post <- distinct(brexit_all_d, user_id, status_id, .keep_all = T)
nrow(master_post) # 1,475,947

# Mean number of posts per user 
# Variance in number of posts per user 

post_person <- brexit_all_d %>% 
  group_by(user_id) %>% 
  summarise(Count = n())

avg_day <- mean(post_person$Count, na.rm = T) # 1,153.99
avg_day_med <- median(post_person$Count, na.rm = T) # 811

var_day <- sd(post_person$Count, na.rm = T) # 981.55


# Mean number of posts per day per user 
# Variance in number of posts per day per user 

library(lubridate)
brexit_all_d$Created_Date <- ymd_hms(brexit_all_d$created_at)
brexit_all_d$Created_Date.d <- date(brexit_all_d$Created_Date)
brexit_all_d$Created_Date.month <- as.character(month(brexit_all_d$Created_Date))
brexit_all_d$Created_Date.day <- as.character(mday(brexit_all_d$Created_Date))
brexit_all_d$Created_Date.month_day <- paste0(brexit_all_d$Created_Date.month, 
                                              "-", brexit_all_d$Created_Date.day)

master.mean.day <- brexit_all_d %>% 
  group_by(Created_Date.month_day, user_id) %>% 
  summarise(Count = n())

master.mean.day.avg <- master.mean.day %>% 
  group_by(user_id) %>% 
  summarise(Count.d = n(), Mean.d = mean(Count, na.rm=T))

avg_posts_day <- mean(master.mean.day.avg$Mean.d, na.rm = T)
avg_posts_day_med <- median(master.mean.day.avg$Mean.d, na.rm = T)
var_posts_day <- sd(master.mean.day.avg$Mean.d, na.rm=T)
min_posts_day <- min(master.mean.day.avg$Mean.d, na.rm=T)
max_posts_day <- max(master.mean.day.avg$Mean.d, na.rm=T)
