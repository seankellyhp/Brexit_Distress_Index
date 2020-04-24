# Distress Wrangle v0.8


setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

# Merge all distress 
distress1 <- read.csv("tweets_history_distress.csv")
distress2 <- readr::read_csv("tweets_history_distress2.csv")
distress3 <- read.csv("tweets_history_distress3.csv")
distress4 <- read.csv("tweets_history_distress4.csv")
distress5 <- read.csv("tweets_history_distress5.csv")
distress6 <- read.csv("tweets_history_distress6.csv")
distress7 <- read.csv("tweets_history_distress7.csv")
distress8 <- read.csv("tweets_history_distress8.csv")

library(dplyr)
library(tidyr)
library(lubridate)

master_distress <- bind_rows(list(distress1, distress3, distress4, 
                                  distress5, distress6, distress7, distress8), .id = "id")



master_distress <- master_distress %>% 
  distinct(user_id, status_id, .keep_all = TRUE) %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x", remove = FALSE)

remove(distress1)
remove(distress2)
remove(distress3)
remove(distress4)
remove(distress5)
remove(distress6)
remove(distress7)
remove(distress8)

distress.test2 <- read.csv("distress_tweet_text_raw_labsv3.csv")

distress.test.cl <- distress.test2 %>% 
  filter(Label == 1) %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x")

distress.users <- unique(distress.test.cl$ID)

master_distress <- master_distress %>% 
  filter(ID %in% distress.users)

remove(distress.test.cl, distress.test2)

# Clean Distress Data 
# Only keep last 3 months 

# Filter to 'since last year'
master_distress$Created_Date <- ymd_hms(master_distress$created_at)
master_distress$Created_Date.d <- date(master_distress$Created_Date)

months_min3 <- max(master_distress$Created_Date.d) %m-% months(3)
months_min4 <- max(master_distress$Created_Date.d) %m-% months(4)

write.csv(master_distress, "master_distress_all.csv")


master_distress.cl <- master_distress %>% 
  filter(ifelse(id == "1",
                Created_Date.d >= months_min4, 
                Created_Date.d >= months_min3
  ))

# Filter with over 25 Tweets and in English
counts.user <- master_distress.cl %>%
  group_by(user_id) %>% 
  summarise(count=n()) %>% 
  filter(count >= 25) 

master_distress.cl <- master_distress.cl %>% 
  filter(user_id %in% counts.user$user_id) %>% 
  filter(lang == "en")

remove(master_distress)

master_distress.cl <- select(master_distress.cl, -ID, -id, -Prefix)

# Load Control Group 1

control1 <- read.csv("tweets_history_control1.csv")

# Only keep last 3 months 

# Filter to 'since last year'
control1$Created_Date <- ymd_hms(control1$created_at)
control1$Created_Date.d <- date(control1$Created_Date)

months_min3 <- max(control1$Created_Date.d) %m-% months(3)

control1.cl <- control1 %>% 
  filter(Created_Date.d >= months_min3)

# Filter with over 25 Tweets and in English
counts.user <- control1.cl %>%
  group_by(user_id) %>% 
  summarise(count=n()) %>% 
  filter(count >= 25) 

control1.cl <- control1.cl %>% 
  filter(user_id %in% counts.user$user_id) %>% 
  filter(lang == "en")

remove(control1)

# Load Control 2

control2 <- read.csv("tweets_history_control2.csv")

# Only keep last 3 months 

# Filter to 'since last year'
control2$Created_Date <- ymd_hms(control2$created_at)
control2$Created_Date.d <- date(control2$Created_Date)

months_min3 <- max(control2$Created_Date.d) %m-% months(3)

control2.cl <- control2 %>% 
  filter(Created_Date.d >= months_min3)

# Filter with over 25 Tweets and in English
counts.user <- control2.cl %>%
  group_by(user_id) %>% 
  summarise(count=n()) %>% 
  filter(count >= 25) 

control2.cl <- control2.cl %>% 
  filter(user_id %in% counts.user$user_id) %>% 
  filter(lang == "en")

remove(control2)

# Save copies of cleaned masters 

write.csv(master_distress.cl, "master_distress_cleaned.csv")
write.csv(control1.cl, "master_control1_cleaned.csv")
write.csv(control2.cl, "master_control2_cleaned.csv")

distress.users.cl <- unique(master_distress.cl$user_id)
control1.users.cl <- unique(control1.cl$user_id)
control2.users.cl <- unique(control2.cl$user_id)

# Join 
master_distress.cl <- data.frame(master_distress.cl)
control1.cl <- data.frame(control1.cl)
control2.cl <- data.frame(control2.cl)

master.all <- bind_rows(list(master_distress.cl, control1.cl, control2.cl), .id = "id")

write.csv(master, "master_distress_control1and2.csv")

remove(master.all)

# Sample

control.all <- bind_rows(list(control1.cl, control2.cl))

users.control <- unique(control.all$user_id)

set.seed(001)
control.sample <- sample(users.control, 640)

# Need to sample so that the depressed group is 20% of the total. Also need to use Sophie's more advanced filter. 

control.sample.even <- control.all %>% 
  filter(user_id %in% control.sample )

remove(control1.cl, control2.cl, controll.all)

# Join with exp data 

master.sample.even <- bind_rows(list(master_distress.cl, control.sample.even), .id = "id")

prop.table(table(master.sample.even$id))



write.csv(master.sample.even, "master_sample_even.csv")

