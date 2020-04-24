### Brexit August-September Clean Join Pred

library(dplyr)
library(tidyr)

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")



# Load Brexit Data 
raw.brexit <- read.csv("master_brexit_end_aug_no_lab_features_no_scale.csv")
raw.brexit.beg <- read.csv("master_brexit_beg_aug_no_lab_features_no_scale.csv")
raw.brexit.sep <- read.csv("master_brexit_beg_sep_no_lab_features_no_scale.csv")


brexit_data_test <- read.csv("brexit_data_labelled_scale_full_300.csv")

raw.brexit <- raw.brexit[,-1]
raw.brexit.beg <- raw.brexit.beg[,-1]
raw.brexit.sep <- raw.brexit.sep[,-1]
brexit_data_test <- select(brexit_data_test, -X.1, -X)



#brexit_data <- read.csv("master_brexit_no_lab_nosent_scale.csv")
#brexit_labs <- read.csv("master_brexit_labs_sample_2.1.csv")

# Scale Brexit data 

raw.brexit.cl <- select(raw.brexit, -contains("Sentiment"))
raw.brexit.cl <- distinct(raw.brexit.cl)

raw.brexit.cl.beg <- select(raw.brexit.beg, -contains("Sentiment"))
raw.brexit.cl.beg <- distinct(raw.brexit.cl.beg)

raw.brexit.cl.sep <- select(raw.brexit.sep, -contains("Sentiment"))
raw.brexit.cl.sep <- distinct(raw.brexit.cl.sep)


# Join Aug

brexit.feature.liwc.n <- raw.brexit.cl[, which(names(raw.brexit.cl) %in% names(brexit_data_test))]
brexit.feature.liwc.n.beg <- raw.brexit.cl.beg[, which(names(raw.brexit.cl.beg) %in% names(brexit_data_test))]

brexit.feature.liwc.test <- brexit_data_test[, -which(names(brexit_data_test) %in% names(raw.brexit.cl.beg))]

# Handle missing Values - These words dont appear in the new data so they are correctly zero

brexit.feature.liwc.n.beg$word.absolut <- 0 
brexit.feature.liwc.n.beg$word.act <- 0
brexit.feature.liwc.n.beg$word.allow <- 0
brexit.feature.liwc.n.beg$word.everyth <- 0
brexit.feature.liwc.n.beg$word.play <- 0
brexit.feature.liwc.n.beg$word.sign <- 0
brexit.feature.liwc.n.beg$word.word <- 0
brexit.feature.liwc.n.beg$word.youa <- 0


#
#
#

# Join Sep

brexit.feature.liwc.n.sep <- raw.brexit.cl.sep[, which(names(raw.brexit.cl.sep) %in% names(brexit_data_test))]

brexit.feature.liwc.test2 <- brexit_data_test[, -which(names(brexit_data_test) %in% names(raw.brexit.cl.sep))]

brexit.feature.liwc.n.sep$word.act <- 0
brexit.feature.liwc.n.sep$word.everyth <- 0
brexit.feature.liwc.n.sep$word.head <- 0
brexit.feature.liwc.n.sep$word.pay <- 0
brexit.feature.liwc.n.sep$word.play <- 0
brexit.feature.liwc.n.sep$word.sign <- 0
brexit.feature.liwc.n.sep$word.two <- 0


# Join

brexit.feature.aug <- bind_rows(brexit.feature.liwc.n, brexit.feature.liwc.n.beg)
brexit.feature.all <- bind_rows(brexit.feature.aug, brexit.feature.liwc.n.sep)

# Scale Brexit 
brexit.feature.liwc.n.scale2 <- apply(brexit.feature.all[, - c(1:3)], 2, scale)
brexit.feature.liwc.n.scale2 <- data.frame(brexit.feature.liwc.n.scale2)

brexit.feature.liwc.n.scale2$userID <- brexit.feature.all$userID
brexit.feature.liwc.n.scale2$Weekly_Text <- brexit.feature.all$Weekly_Text
brexit.feature.liwc.n.scale2$Created_Date.month_day <- brexit.feature.all$Created_Date.month_day

write.csv(brexit.feature.liwc.n.scale2, "brexit_data_all_augsep_scale_full.csv")

#
#
#
# Transfer Learning
#
#
#

brexit.aug <- read.csv("brexit_data_all_augsep_scale_full.csv")
#brexit.aug <- brexit.feature.liwc.n.scale2

library(caret)

load("distress_model_log_uneven.RData")

brexit.model.full <- select(brexit.aug, -userID, -Created_Date.month_day, -X, -Weekly_Text)
brexit.model.aug <- select(brexit.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

target_pred_trans <- predict(log_uneven, newdata = brexit.model.aug)

brexit.aug.pred <- brexit.aug
brexit.aug.pred$Distress_Lab_Pred <- target_pred_trans

table(brexit.aug.pred$Distress_Lab_Pred)

write.csv(brexit.aug.pred, "brexit_pred_augsep.csv")


importance<- varImp(log_uneven)
plot(importance, 20)




