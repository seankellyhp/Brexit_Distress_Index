
#
#
#
# Transfer Learning - Trained Model Applied to Previous Distress Data
#
#
#

setwd("/Users/walden/Documents/BCU Backup/Masters Project/")

brexit.aug <- read.csv("Data/brexit_data_all_augsep_scale_full.csv")
#brexit.aug <- brexit.feature.liwc.n.scale2
distress.test

library(caret)
library(dplyr)
library(tidyr)

load("Models/distress_model_log_uneven.RData")

brexit.model.full <- select(brexit.aug, -userID, -Created_Date.month_day, -X, -Weekly_Text)
brexit.model.aug <- select(brexit.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

target_pred_trans <- predict(log_uneven, newdata = brexit.model.aug)

brexit.aug.pred <- brexit.aug
brexit.aug.pred$Distress_Lab_Pred <- target_pred_trans

table(brexit.aug.pred$Distress_Lab_Pred)

write.csv(brexit.aug.pred, "brexit_pred_augsep.csv")


importance<- varImp(log_uneven)
plot(importance, 20)


# Load Distress Data 

person.label.full.uneven <- read.csv("Data/distress_data_uneven.csv")

set.seed(010)
folds <- caret::createDataPartition(person.label.full.uneven$Distress_Lab, p = .75, list = FALSE)

train.df <- person.label.full.uneven[folds, ]
test.df <- person.label.full.uneven[-folds, ]

train.distress.model.u.full <- select(train.df, -userID, -Created_Date.month_day, -X)
test.distress.model.u.full <- select(test.df, -userID, -Created_Date.month_day, -X)

train.distress.model.u.b <- select(train.distress.model.u.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)
test.distress.model.u.b <- select(test.distress.model.u.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

train.distress.nolab <- select(train.distress.model.u.b, -Distress_Lab)
test.distress.nolab <- select(test.distress.model.u.b, -Distress_Lab)

remove(train.distress.model.u.full, test.distress.model.u.full)

distress_pred_trans <- predict(log_uneven, newdata = train.distress.nolab)

distress.pred <- train.df
distress.pred$Distress_Lab_Pred <- distress_pred_trans

table(distress.pred$Distress_Lab_Pred)

write.csv(distress.pred, "Data/distress_pred_validation.csv")


