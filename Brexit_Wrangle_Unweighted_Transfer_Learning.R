
# Brexit Analysis End August

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

brexit_1 <- read.csv("brexit_tweets_08290830.csv")
brexit_2 <- read.csv("brexit_tweets_0828829.csv")
brexit_3 <- read.csv("brexit_tweets_0827828.csv")
brexit_4 <- read.csv("brexit_tweets_0826827.csv")
brexit_5 <- read.csv("brexit_tweets_0825826.csv")
brexit_6 <- read.csv("brexit_tweets_0824825.csv")
brexit_7 <- read.csv("brexit_tweets_0823824.csv")
brexit_8 <- read.csv("brexit_tweets_0822823.csv")
brexit_9 <- read.csv("brexit_tweets_0821822.csv")


library(dplyr)
library(tidyr)

brexit_all <- bind_rows(list(brexit_1,brexit_2,brexit_3,brexit_3,brexit_4,brexit_5,brexit_6, brexit_7, brexit_8, brexit_9))

brexit_all_d <- brexit_all %>% 
  distinct(user_id, status_id, .keep_all=T) %>% 
  separate(user_id, into = c("Prefix", "ID"), sep = "x", remove = FALSE) %>% 
  filter(lang == "en")


remove(brexit_1)
remove(brexit_2)
remove(brexit_3)
remove(brexit_4)
remove(brexit_5)
remove(brexit_6)
remove(brexit_7)
remove(brexit_8)
remove(brexit_9)

## Add IDs 

#install.packages("digest")
library(digest)

# Anonymize User

set.seed(003)
user_id <- unique(brexit_all_d$user_id)
UID <- unlist(lapply(user_id, digest, algo = "sha1"))

Salt <- vector("character", length(user_id))
for (i in 1:length(user_id)) {
  Salt[i] <- paste(floor(runif(5, min=0, max=9)), collapse = "")
} 

user.codes <- data.frame(user_id, UID, Salt)
user.codes$userID <- paste0(as.character(user.codes$UID), as.character(user.codes$Salt))

master.sample.cl.an <- merge(brexit_all_d, user.codes, by = "user_id")

# Anonymize Status # Removed bc aggregating
#remove(master.sample)

library(lubridate)

master.sample.cl.an$Created_Date <- ymd_hms(master.sample.cl.an$created_at)
master.sample.cl.an$Created_Date.d <- date(master.sample.cl.an$Created_Date)


# Drop IDs 
master.sample.select <- master.sample.cl.an %>% 
  select(userID, status_id, Created_Date, Created_Date.d, text, 
         display_text_width, is_retweet, is_quote, reply_to_user_id, favorite_count, 
         retweet_count, lang, country_code, location, followers_count, friends_count, 
         statuses_count, favourites_count, account_created_at) %>% 
  mutate(is_reply = ifelse(reply_to_user_id == "", 1, -1), Created_Hour = hour(Created_Date), 
         is_night = ifelse(Created_Hour >= 21 | Created_Hour < 6, 1, -1)) %>% ## Removed Labels - Distress_Lab = ifelse(id == 1, "Distress", "Control"
  select(-reply_to_user_id) 

remove(master.sample.cl.an)
remove(brexit_all)
remove(brexit_all_d)

# Shrink data to day 

master.sample.select$Created_Date.month <- as.character(month(master.sample.select$Created_Date))
master.sample.select$Created_Date.day <- as.character(mday(master.sample.select$Created_Date))
master.sample.select$Created_Date.month_day <- paste0(master.sample.select$Created_Date.month, 
                                                      "-", master.sample.select$Created_Date.day)

master.sample.select.day <- master.sample.select %>% 
  group_by(userID, Created_Date.month_day) %>% 
  summarise(Weekly_Text = paste0(text, collapse = " - "), Volume = n(), Avg_Length = mean(display_text_width), 
            Prop_Reply = sum(is_reply)/Volume, Prop_Retweet = sum(is_retweet)/Volume, Time_Index = sum(is_night)/Volume, 
            Daily_Retweets = sum(retweet_count), Daily_Favorites = sum(favorite_count), 
            Followers = max(followers_count),  Followees = max(friends_count),
            Total_Tweets = max(statuses_count), Total_Favorites = max(favourites_count))

remove(user.codes, master.sample.select, i, Salt, UID, user_id)

write.csv(master.sample.select.day, file = "brexit_sample_Aug_end_v0.2_no_labs.csv")

#
#
#
#### Tidy
#
#
#
#master.sample.select.day <- read.csv("brexit_sample_v0.1_no_labs.csv")

master.sample.select <- master.sample.select.day

# Replace References in Text - Remove HTTP
master.sample.select$Weekly_Text <- gsub("@\\w+ *","<span>$1</span>", master.sample.select$Weekly_Text)
master.sample.select$Weekly_Text <- gsub("http.*","",  master.sample.select$Weekly_Text)
master.sample.select$Weekly_Text <- gsub("https.*","", master.sample.select$Weekly_Text)

docs1 <- master.sample.select$Weekly_Text
docs2 <- textclean::replace_non_ascii(docs1, replacement = "")



library(tm)
#library(topicmodels)


# Create corpus from vector
docs<-VCorpus(VectorSource(docs2))

# Clean corpus - order matters
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs, removeNumbers)
docs<-tm_map(docs, removeWords, stopwords("english"))
docs<-tm_map(docs, stemDocument, language = "english")
docs<-tm_map(docs, removePunctuation)
docs<-tm_map(docs, stripWhitespace)

words.remove <- c("cent", "t", "s", "amp", "rt", "m", "ia")
docs<-tm_map(docs, removeWords, words.remove)

dtm<-DocumentTermMatrix(docs)

# Keep terms that cover 99% of doc - play with this parameter 
dtm2<-removeSparseTerms(dtm, 0.99)


# As Data Frame
m <- as.matrix(dtm2)

# Remove Memory intensive objects
#remove(sample.text.train, sample.text.test, docs, dtm, master.sample.cl, master.sample.cl.an)
#remove(index, index.df)
#remove(master.sample.word, master.sample)

data.cl <- data.frame(m)

names(data.cl) <- paste0("word.", names(data.cl))


#
#
# Join Data
#
#

master.sample.select.df <- data.frame(master.sample.select)
master.sample.feature <- master.sample.select.df[, -4]

master.sample.feature <- bind_cols(master.sample.feature, data.cl)


#
# Purge Memory
#

#remove(dtm, dtm2, m, master.sample.select.day, master.sample.select.df, docs2, words.remove, data.cl)

#
# Add to DF
#

tweets.words <- lapply(docs, as.character)
tweets.words <- unlist(tweets.words, use.names = F)

master.sample.select$text.cl <- tweets.words



# Clean text differently for Bag of Words Sentiment Analysis, as it is handled in house in SentimentAnalysis package

docs1 <- master.sample.select$Weekly_Text
docs2 <- textclean::replace_non_ascii(docs1, replacement = "")

# Create corpus from vector
docs.b<-VCorpus(VectorSource(docs2))

# Clean corpus - order matters
docs.b<-tm_map(docs.b,content_transformer(tolower))
docs.b<-tm_map(docs.b, removeNumbers)
#docs.b<-tm_map(docs.b, removeWords, stopwords("english"))
docs.b<-tm_map(docs.b, stemDocument, language = "english")
docs.b<-tm_map(docs.b, removePunctuation)
docs.b<-tm_map(docs.b, stripWhitespace)

# Test using the cleaned corpus
#sentiment3 <- analyzeSentiment(master.sample.select.cl$text.cl)

tweets.words2 <- lapply(docs.b, as.character)
tweets.words2 <- unlist(tweets.words2, use.names = F)

master.sample.select$text.cl2 <- tweets.words2

# Save a copy 

write.csv(master.sample.select, file = "master_brexit_end_aug_cl_no_labs.csv")

#

master.sample.select.df.sel <- master.sample.select.df %>% 
  filter(Created_Date.month_day == "8-28")

set.seed(008)
test_brexit_sample <- master.sample.select.df.sel[sample(1:nrow(master.sample.select.df.sel), 500,
                                                         replace=FALSE),]

#write.csv(test_brexit_sample[1:3], file = "master_brexit_end_aug_labs_sample.csv")


#


#
#
# Add Sentiment Analysis 
#
#
#


# turn off scientific notation
options(scipen=999)



#
#
#
#


# Export Text to load into LIWC 

master.sample.feature.sent <- master.sample.feature
#master.sample.feature.sent <- master.sample.feature.sent[,-1] # if data loaded from CSV remove rowlabs

# Load in Results

liwc.add <- read.csv("LIWC2015 Results (master_brexit_end_aug_cl_no_labs.csv).csv")
liwc.add.cl <- liwc.add[-1,]
liwc.add.cl.sel <- liwc.add.cl[c(2:3, 18:77)]

liwc.add.cl.sel <- liwc.add.cl.sel %>% 
  rename("userID" = 1, "Created_Date.month_day" = 2)

person.feature.liwc <- merge(master.sample.feature.sent, liwc.add.cl.sel, by = c("userID", "Created_Date.month_day"), all.x = T)

write.csv(person.feature.liwc, file = "master_brexit_end_aug_no_lab_features_no_scale.csv")

# Load in data 

#person.feature.liwc <- read.csv("master_brexit_no_lab_features_no_scale.csv") 


# Top Unigrams need to match those of the other training data

source_df <- read.csv("master_even_features_nosent_scale.csv")

names_source_df <- names(source_df)
names_target_df <- names(person.feature.liwc)

###

person.feature.liwc.sel <- person.feature.liwc[, which(names_target_df %in% names_source_df)]

person.feature.liwc.df <- data.frame(person.feature.liwc.sel)
person.feature.liwc.cl <- person.feature.liwc.df[complete.cases(person.feature.liwc.df), ]

person.feature.liwc.cl$Total_Tweets <- as.numeric(person.feature.liwc.cl$Total_Tweets)
person.feature.liwc.cl$Total_Favorites <- as.numeric(person.feature.liwc.cl$Total_Favorites)

#
#
#
# Purge memory
#
#

remove(data.cl, docs, docs.b, dtm, dtm2, liwc.add, liwc.add.cl, liwc.add.cl.sel, m, 
       master.sample.feature, master.sample.feature.sent, master.sample.select, 
       master.sample.select.day, master.samle.select.df, person.feature.liwc, 
       person.feature.liwc.sel)

#

person.label.full.scale2 <- apply(person.feature.liwc.cl[, - c(1:2)], 2, scale)
person.label.full.scale2 <- data.frame(person.label.full.scale2)

person.label.full.scale2$userID <- person.feature.liwc.cl$userID
#person.label.full.scale2$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
person.label.full.scale2$Created_Date.month_day <- person.feature.liwc.cl$Created_Date.month_day


person.feature.liwc.s <- person.label.full.scale2 %>% 
  group_by(userID, Created_Date.month_day) %>% 
  summarise_all(round, 3) %>% 
  select(-Prop_Retweet)


person.label.full <- person.feature.liwc.s

write.csv(person.label.full, file = "master_brexit_end_aug_no_lab_nosent_scale.csv")

#person.label.full <- read.csv("master_brexit_no_lab_nosent_scale.csv")

person.label.brexit <- person.label.full


#
# Load small sample of Distressed Tweets
#
#


#
#
# Label a small sample of Distressed Brexit Tweets
#
#

names_target_df2 <- names(person.label.brexit)

person.label.full <- source_df[, which(names_source_df %in% names_target_df2)]
person.label.full$Distress_Lab <- source_df$Distress_Lab

#
## Only significant predictors 

sig_labs <- read.csv("features_sig_tests_cl.csv")
sig_labs$sig_flag <- ifelse(sig_labs$sig_p_value <= .01, 1, 0)
sig_labs_cl2 <- sig_labs[which(sig_labs$sig_flag == 1),]
sig_labs_names2 <- as.character(sig_labs_cl2$feature_names)

person.label.full.names <- names(person.label.full)
person.label.full.sel <- person.label.full[, which(person.label.full.names %in% sig_labs_names2)]

#
#person.label.full.sel = Brexit data
brexit_df_cl <- person.label.full.sel

names_target_3 <- names(person.label.full.sel)

#
distress_df_cl <- source_df[, which(names_source_df %in% names_target_3)]
distress_df_cl$Distress_Lab <- source_df$Distress_Lab

# distress_df_cl = Full Distress Data 
# Add Ids 

distress_df_cl$userID <- source_df$userID
distress_df_cl$Created_Date.month_day <- source_df$Created_Date.month_day

brexit_df_cl$userID <- person.label.full$userID
brexit_df_cl$Created_Date.month_day <- person.label.full$Created_Date.month_day

##

write.csv(distress_df_cl, "master_distress_match1_no_ng.csv")
write.csv(brexit_df_cl, "master_brexit_match1_no_ng.csv")

remove(master.sample.select.df, master.sample.select.df.sel, person.feature.liwc.cl, person.feature.liwc.df, 
       person.feature.liwc.s, person.label.full, person.label.full.scale2, person.label.full.sel, 
       sig_labs, sig_labs_cl2, source_df, test_brexit_sample, docs1, docs2, tweets.words, tweets.words2)


# Pull in small sample of labelled Brexit data 


brexit_labs <- read.csv("master_brexit_end_aug_labs_v.1_sample.csv")

brexit_data_test <- merge(brexit_df_cl, brexit_labs, by = c("userID", "Created_Date.month_day"))

brexit_data_test$Distress_Lab <- as.character(brexit_data_test$Distress_Lab)

brexit_data_test <- brexit_data_test %>% 
  filter(Distress_Lab == "1" | Distress_Lab == "2") 

brexit_data_test$Weekly_Text <- as.character(brexit_data_test$Weekly_Text)

brexit_data_test.cl <- brexit_data_test %>% 
  filter(!grepl("Urgent -- Boris is suspending parliament to force through his #Brexit. Here's how we stop him! #BlockTheCoup", Weekly_Text))

brexit_data_test.cl <- data.frame(brexit_data_test.cl)
# Keep only sig names

brexit_data_test.names <- names(brexit_data_test.cl)
brexit_data_test.sel <- brexit_data_test.cl[, which(brexit_data_test.names %in% names(brexit_df_cl))]

brexit_data_test.sel$Distress_Lab <- brexit_data_test.cl$Distress_Lab
brexit_data_test.sel$Distress_Lab <- ifelse(brexit_data_test.sel$Distress_Lab == "1", "Distress", "Control")


brexit_test_labs <- brexit_data_test.sel

remove(brexit_data_test, brexit_data_test.cl, brexit_data_test.sel, brexit_labs)
#

library(caret)
set.seed(005)
folds <- caret::createDataPartition(distress_df_cl$Distress_Lab, p = .75, list = FALSE)

train.df <- distress_df_cl[folds, ]
test.df <- distress_df_cl[-folds, ]

trctrl <- trainControl(method = "cv", number = 3)

grid_best <- expand.grid(sigma = c(0, .01), C = c(0, 2)) 

# remove user name and date

train.df.c <- train.df[,-which(names(train.df) %in% c("userID","Created_Date.month_day"))]
test.df.c <- test.df[,-which(names(test.df) %in% c("userID","Created_Date.month_day"))]


# Model 

library(doParallel)

cores <- detectCores()

cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

# Remove row names if loaded in csv
#train.df <- train.df[-1]
#train.df <- train.df[-2]
#test.df <- test.df[-1]
#test.df <- test.df[-2]

svm_full_train <- train(factor(Distress_Lab) ~., data = train.df.c, method = "svmRadial",
                        trControl=trctrl,
                        tuneGrid = grid_best)

svm_full_train 

svm_full_train_pred <- predict(svm_full_train, newdata = test.df.c)

confusionMatrix(svm_full_train_pred, test.df.c$Distress_Lab)

save(svm_full_train, file="distress_model_lim_cols_v2.RData")

# Run it and see what happend - Failed because missing var --- Need to match column names 

model.svm.base.brexit <- predict(svm_full_train, newdata = brexit_test_labs)

confusionMatrix(model.svm.base.brexit, factor(brexit_test_labs$Distress_Lab))


### With Log Regression 

log_full_train <- train(factor(Distress_Lab) ~., data = train.df.c, method = "glm", family = "binomial",
                        trControl=trctrl)

log_full_train 

log_full_train_pred <- predict(log_full_train, newdata = test.df.c)

confusionMatrix(log_full_train_pred, test.df.c$Distress_Lab)

model.log.base.brexit <- predict(log_full_train, newdata = brexit_test_labs)

confusionMatrix(model.log.base.brexit, factor(brexit_test_labs$Distress_Lab))



#person.label.brexit.labs <- person.label.brexit

# Save source data 
write.csv(brexit_df_cl, file = "brexit_target_test_v1.csv")
write.csv(brexit_test_labs, file = "brexit_target_labs_test_v1.csv")

write.csv(test.df, file = "distress_df_test_v1.csv")
write.csv(train.df, file = "distress_df_train_v1.csv")

