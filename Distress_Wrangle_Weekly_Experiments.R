# Weekly Analysis Distress

#
# Load Data and Packages
#


library(dplyr)
library(tidyr)
library(lubridate)

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")


#
# Pre-Process Big Data 
#

# Tidy
master.sample <- read.csv("master_sample_even.csv")

prop.table(table(master.sample$id))

# Anonymize Data 

#install.packages("digest")
library(digest)

# Anonymize User

set.seed(002)
user_id <- unique(master.sample$user_id)
UID <- unlist(lapply(user_id, digest, algo = "sha1"))

Salt <- vector("character", length(user_id))
for (i in 1:length(user_id)) {
  Salt[i] <- paste(floor(runif(5, min=0, max=9)), collapse = "")
} 

user.codes <- data.frame(user_id, UID, Salt)
user.codes$userID <- paste0(as.character(user.codes$UID), as.character(user.codes$Salt))

master.sample.cl.an <- merge(master.sample, user.codes, by = "user_id")

# Anonymize Status # Removed bc aggregating
remove(master.sample)

# Drop IDs 
master.sample.select <- master.sample.cl.an %>% 
  select(userID, status_id, id, Created_Date, Created_Date.d, text, 
         display_text_width, is_retweet, is_quote, reply_to_user_id, favorite_count, 
         retweet_count, lang, country_code, location, followers_count, friends_count, 
         statuses_count, favourites_count, account_created_at) %>% 
  mutate(is_reply = ifelse(reply_to_user_id == "", 1, -1), Created_Hour = hour(Created_Date), 
         is_night = ifelse(Created_Hour >= 21 | Created_Hour < 6, 1, -1), ### Fucked that up - maybe
         Distress_Lab = ifelse(id == 1, "Distress", "Control")) %>% 
  select(-reply_to_user_id) 

remove(master.sample.cl.an)

# Shrink Data to Week

master.sample.select$Created_Date.week <- as.character(week(master.sample.select$Created_Date))

master.sample.select.week <- master.sample.select %>% 
  group_by(Distress_Lab, userID, Created_Date.week) %>% 
  summarise(Weekly_Text = paste0(text, collapse = " - "), Volume = n(), Avg_Length = mean(display_text_width), 
            Prop_Reply = sum(is_reply)/Volume, Prop_Retweet = sum(is_retweet)/Volume, Time_Index = sum(is_night)/Volume, 
            Daily_Retweets = sum(retweet_count), Daily_Favorites = sum(favorite_count), 
            Followers = max(followers_count),  Followees = max(friends_count),
            Total_Tweets = max(statuses_count), Total_Favorites = max(favourites_count))


# Save backups

#write.csv(user.codes, "user_codes.csv")
write.csv(master.sample.select.week, "master_weekly_even.csv")
#write.csv(master.sample.select.day, "master_daily_even.csv")

#
# Purge Memory 
#

remove(user.codes, master.sample.select, master.sample.select, i, Salt, UID, user_id)


#
# 1. Keep Going or 2. Load in Smaller Data and Clean 
#

#master.sample.select <- read.csv("master_weekly_sample.csv")
master.sample.select <- master.sample.select.week

# Replace References in Text - Remove HTTP
master.sample.select$Weekly_Text <- gsub("@\\w+ *","<span>$1</span>", master.sample.select$Weekly_Text)
master.sample.select$Weekly_Text <- gsub("http.*","",  master.sample.select$Weekly_Text)
master.sample.select$Weekly_Text <- gsub("https.*","", master.sample.select$Weekly_Text)

docs1 <- master.sample.select$Weekly_Text
docs2 <- textclean::replace_non_ascii(docs1, replacement = "")

####

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

# Find Frequent Terms 

#dtm2 <- findFreqTerms(dtm, 5)

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

remove(dtm, dtm2, m, master.sample.select.week, master.sample.select.df, docs2, words.remove, data.cl)

#
# Add to DF
#

tweets.words <- lapply(docs, as.character)
tweets.words <- unlist(tweets.words, use.names = F)

master.sample.select$text.cl <- tweets.words


#
# Export Cleaned Text
#

#write.csv(master.sample.select.cl, file = "master_sample_cl.csv")


#
# Add Sentiment Scores 
#



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



#
# Export Cleaned Text
#

write.csv(master.sample.select, file = "master_weekly_even_cl.csv")


#
#
#
#
# Tidytext Word Count and Sentiment Analysis, Slow
#
#
#
#


#master.sample.word <- master.sample.select.cl %>%
#  select(userID, Created_Date.week, text.cl) %>%
#  tidytext::unnest_tokens(word, text.cl)

#master.sample.word.total <- master.sample.word %>% 
#  group_by(userID, Created_Date.week) %>% 
#  summarise(TotalWords = n())

#master.sample.word.total <- merge(master.sample.word, master.sample.word.total, 
#                                  by = c("userID", "Created_Date.week"), all.x = TRUE)

# Sentiment Bing - Other arent working correctly
#bing <- get_sentiments("bing")
#table(bing$sentiment)

#master.sample.sent.bing <- master.sample.word.total %>% 
#  inner_join(bing) 

#master.sample.sent.bing.ag <- master.sample.sent.bing %>% 
#  group_by(userID, Created_Date.week, TotalWords) %>% 
#  summarise(Positive = sum(ifelse(sentiment == "positive", 1, 0)), 
#            Negative = sum(ifelse(sentiment == "negative", 1, 0)), 
#            PosNeg = 1/(Positive-Negative), 
#            PosNeg.cl = ifelse(is.infinite(PosNeg), 0, PosNeg),
#            Emotional = (Positive + Negative)/max(TotalWords)) %>% 
#  select(-PosNeg, "PosNeg" = PosNeg.cl)

#master.sample.select.sent <- merge(master.sample.select, master.sample.sent.bing.ag, 
#                                                                 by = c("userID", "Created_Date.week"), all.x = TRUE)



#
#
#
# Sentiment Multiple - Binary HarvardVI, QDAP, Financial Dictionaries
#
#
#

#install.packages("SentimentAnalysis")



#library(SentimentAnalysis)


# Too Big Needs to be broken up 

#dfs2 <- split(master.sample.select$text.cl, rep(1:10, each = round(NROW(master.sample.select)/ 10)))


#res <- lapply(dfs2, function(x) analyzeSentiment(x))
#res.df <- bind_rows(res, .id = "column_label")

#sentiment2.bin <- apply(dplyr::select(res.df, SentimentGI, SentimentHE, SentimentLM, SentimentQDAP), MARGIN = 2, convertToDirection)
#sentiment2.bin2 <- data.frame(sentiment2.bin)
#sentiment2.bin2$WordCount <- res.df$WordCount

#master.sample.feature.sent <- bind_cols(master.sample.feature, sentiment2.bin2)


#
#
# Purge Memory 
#
#

remove(docs1, docs2, tweets.words, tweets.words2, docs, docs.b, master.sample.select.week, master.sample.select.df,
       sentiment2, sentiment2.bin, sentiment2.bin2, res.df, data.cl, dtm, dtm2, master.sample.select, m, words.remove)

#write.csv(master.sample.feature.sent, "master_features_even.csv")
#
#
# Add depression Language DeChoudhury - Doesnt match well
#
#

#depression.lang <- read.csv("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Masters Project/DeChoudhuryDepressionLexicon2.csv")

#docs.de<-VCorpus(VectorSource(depression.lang$Word))
#docs.de<-tm_map(docs.de,content_transformer(tolower))
#docs.de<-tm_map(docs.de, removeNumbers)
#docs.de<-tm_map(docs.de, removeWords, stopwords("english"))
#docs.de<-tm_map(docs.de, stemDocument, language = "english")
#docs.de<-tm_map(docs.de, removePunctuation)
#docs.de<-tm_map(docs.de, stripWhitespace)

#depress.words <- lapply(docs.de, as.character)
#depress.words <- unlist(depress.words, use.names = F)

#depression.lang$Word.cl <- depress.words
#depression.lang.cl <- select(depression.lang, Category, Word, Word.cl)

#allwords <- names(master.sample.feature)
#depress.words <- depression.lang$Word.cl

#ans <- vapply(allwords, function(allwords) allwords==depress.words, logical(111))
#res <- rowSums(ans)

#depress.words.df <- data.frame(depress.words, res) # Maybe Adjust Somehow to capture more 

# turn off scientific notation
options(scipen=999)

#
#
#
# Add Language Style, Positive Affect, and others added from LIWC
#
#
#

# Export Text to load into LIWC 

#write.csv(master.sample.select, "master_sample_select1000.csv")





#
#
# Weekly data without sentiment
#
#

master.sample.feature.sent <- master.sample.feature

# Load in Results

liwc.add <- read.csv("LIWC2015 Results (master_weekly_even_cl.csv).csv")
liwc.add.cl <- liwc.add[-1,]
liwc.add.cl.sel <- liwc.add.cl[c(2:4, 19:78)]

liwc.add.cl.sel <- liwc.add.cl.sel %>% 
  rename("Distress_Lab" = 1, "userID" = 2, "Created_Date.week" = 3)

# To aggregate at person level

# 
#
# Purge Memory
#
#

remove(liwc.add, liwc.add.cl, liwc.agg)


#
# Save backup 
#


# Merge all Numeric Data 

person.feature.liwc <- merge(master.sample.feature.sent, liwc.add.cl.sel, by = c("userID", "Distress_Lab", "Created_Date.week"), all.x = T)

#
# Purge Memory 
#
#
# Purge Memory
#

remove(dfs2, liwc.add.cl.sel, liwc.agg.cl, master.sample.feature.sent, master.sample.select, res)

#
#
# Load Data
#
#

#person.feature.liwc <- read.csv("master_even_features_no_scale.csv")

# May have to turn these into dummy variables 


#
#
#
# Supervised Feature selection using X^2 max score
#
#
#


#n <-  # Total number of observations 
#a <-  # yes feature yes category 
#b <-  # yes feature no category 
#c <-  # no feature yes category 
#d <-  # no feature no category

x_squared_distress_f <- function(x) {
  n <- as.numeric(nrow(person.feature.liwc))
  a <- sum(ifelse(x >= 1 & person.feature.liwc$Distress_Lab == "Distress", 1, 0))
  b <- sum(ifelse(x >= 1 & person.feature.liwc$Distress_Lab == "Control", 1, 0))
  c <- sum(ifelse(x == 0 & person.feature.liwc$Distress_Lab == "Distress", 1, 0))
  d <- sum(ifelse(x == 0 & person.feature.liwc$Distress_Lab == "Control", 1, 0))
  
  x_squared_distress <- (n * ((a * d) - (b * c)))/((a + b)*(a + c)*(b + d)*(c + d))
  return(x_squared_distress)
}

x_squared_distress_f(person.feature.liwc$word.sad)

x_squared_distress_val <- apply(dplyr::select(person.feature.liwc, contains("word.")), MARGIN = 2, x_squared_distress_f)
x_squared_distress_df <- data.frame(x_squared_distress_val)  
x_squared_distress_df$Words <- rownames(x_squared_distress_df)  

# Had a function for control but realized because it is binary it is just the opposite for each word 

x_squared_combine <- x_squared_distress_df %>% 
  mutate(x_squared_control_val = -x_squared_distress_val, 
         x_squared_max_val = ifelse(x_squared_distress_val > x_squared_control_val, x_squared_distress_val, x_squared_control_val))


x_squared_combine.cl <- x_squared_combine %>% 
  arrange(x_squared_max_val) %>% 
  top_n(150)

top_words <- x_squared_combine.cl$Words

person.feature.liwc.top.words <- person.feature.liwc %>% 
  select(top_words)

person.feature.liwc.no.words <- person.feature.liwc %>% 
  select(-contains("word."))

person.feature.liwc.cl <- bind_cols(person.feature.liwc.no.words, person.feature.liwc.top.words)


remove(person.feature.liwc, person.feature.liwc.top.words, person.feature.liwc.no.words, 
       x_squared_control_df, x_squared_distress_df)

person.feature.liwc <- person.feature.liwc.cl

# Scale Numeric Data 

# Remove Sentiment Analysis 

#person.feature.liwc.c <- select(person.feature.liwc, -SentimentGI, -SentimentHE, 
#                              -SentimentLM, -SentimentQDAP)

person.feature.liwc.df <- data.frame(person.feature.liwc)
person.feature.liwc.cl <- person.feature.liwc.df[complete.cases(person.feature.liwc.df), ]

person.feature.liwc.cl$Total_Tweets <- as.numeric(person.feature.liwc.cl$Total_Tweets)
person.feature.liwc.cl$Total_Favorites <- as.numeric(person.feature.liwc.cl$Total_Favorites)

#options(scipen=999)



person.label.full.scale2 <- apply(person.feature.liwc.cl[, - c(1:3)], 2, scale)
person.label.full.scale2 <- data.frame(person.label.full.scale2)

person.label.full.scale2$userID <- person.feature.liwc.cl$userID
person.label.full.scale2$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
person.label.full.scale2$Created_Date.week <- person.feature.liwc.cl$Created_Date.week


person.feature.liwc.s <- person.label.full.scale2 %>% 
  group_by(userID, Distress_Lab, Created_Date.week) %>% 
  summarise_all(round, 4)

#
# Purge Memory 
#

remove(person.feature.liwc.df, person.label.full.scale2, person.feature.liwc, person.feature.liwc.c, 
       person.feature.liwc.cl, master.sample.feature)


#
# Numeric Data is Cleaned and Scaled, add Word Data - Lost a few Observation bc missing values
#

# If Binary YN
#convert_count <- function(x) {
#  x <- ifelse(x > 0, "Yes", "No")
#}

#person.label.words.set2 <- apply(person.label.words.set[-1], MARGIN = 2, convert_count)
#person.label.words.set2 <- data.frame(person.label.words.set2)
#person.label.words.set2$userID <- person.label.words.set$userID
# 

#person.feature.liwc.words <- merge(person.feature.liwc.s, person.label.words.set, by = c("userID"))

person.label.full <- person.feature.liwc.s

write.csv(person.label.full, file = "master_even_features_week_nosent_scale.csv")


#
# Load in Scaled Dataset
#

#read.csv()



#
#
#
# Create Training and Test Data with Cross Validation 
#
#
#

library(caret)

person.label.full <- person.label.full[,-3]


set.seed(002)
folds <- caret::createDataPartition(person.label.full$Distress_Lab, p = .75, list = FALSE)

train <- person.label.full[folds, ]
test <- person.label.full[-folds, ]

remove(folds, person.feature.liwc.cl, person.feature.liwc.s)

# Create training and test data

#set.seed(42)
#nobs <- nrow(master.sample.feature)
#sample <- train <- sample(nrow(master.sample.feature), 0.7*nobs)
#validate = NULL
#test <- setdiff(setdiff(seq_len(nrow(master.sample.feature)), train), validate)


#
#
# Try Classifiers 
#
#
#

#
# SVM
#

library(e1071)

#htrain4 <- data.frame(htrain4)

#model.base <- svm(factor(Distress_Lab) ~ ., data=htrain4[-1], kernal = "radial basis", cost = 8) # Play with cost and gamma
#model.svm.base <- predict(model.base, select(htest4, -Distress_Lab)) 

#label <- htest4$Distress_Lab
#t.b <- table(model.svm.base, label)
#sens.base <- t.b[2,2]/(t.b[2,2]+t.b[1,2])
#spec.base <- t.b[1,1]/(t.b[1,1]+t.b[2,1])
#t.b

# RBF, Cost 8 = Sens = 92.8, Spec = 77 Test - 73.3, 51.9

#sens.base1 <- sens.base
#spec.base1 <- spec.base # Crap Results with small test data

#accuracy <- 

# Peek at outcomes
#summary(model.base)
#str(model.base)


# Use caret package 

library(doParallel)

cores <- detectCores()

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

library(caret)

grid_radial <- expand.grid(sigma = c(.001, .01, .05, .1, .5, .9), C = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16, 32)) 

set.seed(003)
trctrl <- trainControl(method = "cv", number = 5)


train <- data.frame(train)

svm_Radial_Grid <- train(factor(Distress_Lab) ~., data = train[-1], method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial)

svm_Radial_Grid

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)

confusionMatrix(test_pred_Radial, testing$V14 )

stopCluster(cl)
