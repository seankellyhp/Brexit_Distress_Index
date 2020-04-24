
# Brexit Weighted Transfer Learning Final

# Analysis for Weighted Models 

library(dplyr)
library(tidyr)

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

# Load Data 

# Distress
person.label.full.uneven <- read.csv("distress_data_uneven.csv")

distress.model.u.full <- select(person.label.full.uneven, -userID, -Created_Date.month_day, -X)
distress.model.u.b <- select(distress.model.u.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

# Brexit

brexit_data_test <- read.csv("brexit_data_labelled_scale_full_300.csv")

# Clean Data for model 
brexit.model.full <- select(brexit_data_test, -userID, -Created_Date.month_day, -X.1, -X,  -Weekly_Text)
brexit.model.b <- select(brexit.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)


#
#
# Rerun Models 
#
#



#Set Up Environment

library(e1071)


library(doParallel)

cores <- detectCores()

cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

library(caret)

# Test and Training Data 
remove(brexit.model.full, brexit.model.b, distress.model.u.full, distress.model.u.b)

set.seed(010)
folds <- caret::createDataPartition(person.label.full.uneven$Distress_Lab, p = .75, list = FALSE)

train.df <- person.label.full.uneven[folds, ]
test.df <- person.label.full.uneven[-folds, ]

train.distress.model.u.full <- select(train.df, -userID, -Created_Date.month_day, -X)
test.distress.model.u.full <- select(test.df, -userID, -Created_Date.month_day, -X)


train.distress.model.u.b <- select(train.distress.model.u.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)
test.distress.model.u.b <- select(test.distress.model.u.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)


# model 
trctrl <- trainControl(method = "cv", number = 5)

#
# Random Forest 
#

grid_rf <- expand.grid(.mtry = c(8, 15, 30, 60, 120, 229))


rf_uneven <- train(Distress_Lab~., 
                   data=train.distress.model.u.b, 
                   method='rf', 
                   metric='Kappa', 
                   trControl=trctrl,
                   tuneGrid = grid_rf)

save(rf_uneven, file="distress_model_rf_uneven.RData")


# Source 

importance<- varImp(rf_uneven)
plot(importance, 20)

test_pred_rf <- predict(rf_uneven, newdata = test.distress.model.u.b) # NI = .65, #Acc = .67
confusionMatrix(test_pred_rf, test.distress.model.u.b$Distress_Lab )


# Target 

target_pred_rf <- predict(rf_uneven, newdata = brexit.model.b)
confusionMatrix(target_pred_rf, brexit.model.b$Distress_Lab ) # Acc = .45, Sens = .63, Spec = .097

#
# Logistic Regression 
#

# LR 

log_uneven <- train(factor(Distress_Lab) ~., data = train.distress.model.u.b, method = "glm",
                        family = "binomial", trControl=trctrl)

importance<- varImp(log_uneven)
plot(importance, 20)

log_uneven 

save(log_uneven, file="distress_model_log_uneven.RData")

# Source 

test_pred_log <- predict(log_uneven, newdata = test.distress.model.u.b)
confusionMatrix(test_pred_log, test.distress.model.u.b$Distress_Lab)

# Target 

target_pred_log <- predict(log_uneven, newdata = brexit.model.b)
confusionMatrix(target_pred_log, brexit.model.b$Distress_Lab ) 


# 
# Support Vector Machine 
# 

grid_radial <- expand.grid(sigma = c(.001, .01, .05, .1, .5), C = c(0.25, 0.5, 1, 2, 4)) 


svm_uneven <- train(factor(Distress_Lab) ~., data = train.distress.model.u.b, method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial)

importance_svm <- varImp(svm_uneven)
plot(importance_svm, 50)

save(svm_uneven, file="distress_model_svm_uneven.RData")


# Source 

test_pred_svm <- predict(svm_uneven, newdata = test.distress.model.u.b)
confusionMatrix(test_pred_svm, test.distress.model.u.b$Distress_Lab)

# Target 

target_pred_svm <- predict(svm_uneven, newdata = brexit.model.b)
confusionMatrix(target_pred_svm, brexit.model.b$Distress_Lab ) 


