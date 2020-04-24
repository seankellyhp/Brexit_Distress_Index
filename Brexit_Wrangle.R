# Brexit wrangle tidy beg sep

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

brexit_1 <- read.csv("brexit_tweets_09170918.csv")
brexit_2 <- read.csv("brexit_tweets_09160917.csv")
brexit_3 <- read.csv("brexit_tweets_09150916.csv")
brexit_4 <- read.csv("brexit_tweets_09140915.csv")
brexit_5 <- read.csv("brexit_tweets_09130914.csv")
brexit_6 <- read.csv("brexit_tweets_09120913.csv")
brexit_7 <- read.csv("brexit_tweets_09110912.csv")
brexit_8 <- read.csv("brexit_tweets_09100911.csv")
brexit_9 <- read.csv("brexit_tweets_09090910.csv")



library(dplyr)
library(tidyr)

brexit_all <- bind_rows(list(brexit_1,brexit_2,brexit_3,brexit_4,brexit_5,brexit_6,brexit_7,brexit_8,brexit_9))

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

#write.csv(master.sample.select.day, file = "brexit_sample_Aug_end_v0.2_no_labs.csv")

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

write.csv(master.sample.select, file = "master_brexit_beg_sep_cl_no_labs.csv")

#

#master.sample.select.df.sel <- master.sample.select.df %>% 
#  filter(Created_Date.month_day == "8-28")

#set.seed(008)
#test_brexit_sample <- master.sample.select.df.sel[sample(1:nrow(master.sample.select.df.sel), 500,
#                                                         replace=FALSE),]

#write.csv(test_brexit_sample[1:3], file = "master_brexit_end_aug_labs_sample.csv")


#


#
#
# Add Sentiment Analysis 
#
#
#



#library(SentimentAnalysis)


# Too Big Needs to be broken up 

#dfs2 <- split(master.sample.select$text.cl, rep(1:10, each = round(NROW(master.sample.select)/ 10)))


#res <- lapply(dfs2, function(x) analyzeSentiment(x))
#res.df <- bind_rows(res, .id = "column_label")

#sentiment2.bin <- apply(dplyr::select(res.df, SentimentGI, SentimentHE, SentimentLM, SentimentQDAP), MARGIN = 2, convertToDirection)
#sentiment2.bin2 <- data.frame(sentiment2.bin)
#sentiment2.bin2$WordCount <- res.df$WordCount

#master.sample.feature.sent <- bind_cols(master.sample.feature, sentiment2.bin2)

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

liwc.add <- read.csv("LIWC2015 Results (master_brexit_beg_sep_cl_no_labs.csv).csv")
liwc.add.cl <- liwc.add[-1,]
liwc.add.cl.sel <- liwc.add.cl[c(2:3, 18:77)]

liwc.add.cl.sel <- liwc.add.cl.sel %>% 
  rename("userID" = 1, "Created_Date.month_day" = 2)

person.feature.liwc <- merge(master.sample.feature.sent, liwc.add.cl.sel, by = c("userID", "Created_Date.month_day"), all.x = T)

write.csv(person.feature.liwc, file = "master_brexit_beg_sep_no_lab_features_no_scale.csv")