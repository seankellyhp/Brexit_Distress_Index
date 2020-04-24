# Brexit Wrangling, weighting, Transfer Learning Tests


library(dplyr)
library(tidyr)

setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")


person.label.full <- read.csv(file = "master_even_features_nosent_scale.csv")
person.label.full <- read.csv(file = "master_even_features_nosent_scale.csv")
person.feature.liwc <- read.csv("master_even_features_no_scale.csv")
brexit.feature.liwc <- read.csv(file = "master_brexit_end_aug_no_lab_features_no_scale.csv")


person.feature.liwc.cl <- select(person.feature.liwc, -contains("Sentiment"))
person.feature.liwc.cl <- distinct(person.feature.liwc.cl)

brexit.feature.liwc.cl <- select(brexit.feature.liwc, -contains("Sentiment"))
brexit.feature.liwc.cl <- distinct(brexit.feature.liwc.cl)



person.feature.liwc.n <- person.feature.liwc.cl[, which(names(person.feature.liwc.cl) %in% names(brexit.feature.liwc.cl))]
brexit.feature.liwc.n <- brexit.feature.liwc.cl[, which(names(brexit.feature.liwc.cl) %in% names(person.feature.liwc.n))]

#person.feature.liwc.n$Distress_Lab <- person.feature.liwc.cl$Distress_Lab


#mb <- mean(brexit.feature.liwc.n[, 4])
#mp <- mean(person.feature.liwc.n[, 4])

#x = mb/mp

#person.feature.liwc.n$Avg_Length.w = person.feature.liwc.n$Avg_Length * x


# Scale Brexit 
brexit.feature.liwc.n.scale2 <- apply(brexit.feature.liwc.n[, - c(1:3)], 2, scale)
brexit.feature.liwc.n.scale2 <- data.frame(brexit.feature.liwc.n.scale2)

brexit.feature.liwc.n.scale2$userID <- as.character(brexit.feature.liwc.n$userID)
#brexit.feature.liwc.n.scale2$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
brexit.feature.liwc.n.scale2$Created_Date.month_day <- brexit.feature.liwc.n$Created_Date.month_day


# Scale Distress 

person.feature.liwc.n.scale2 <- apply(person.feature.liwc.n[, - c(1:3)], 2, scale)
person.feature.liwc.n.scale2 <- data.frame(person.feature.liwc.n.scale2)

person.feature.liwc.n.scale2$userID <- as.character(person.feature.liwc.n$userID)
#brexit.feature.liwc.n.scale2$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
person.feature.liwc.n.scale2$Created_Date.month_day <- person.feature.liwc.n$Created_Date.month_day

# Rename 

brexit.feature.liwc.n <- brexit.feature.liwc.n.scale2
person.feature.liwc.n <- person.feature.liwc.n.scale2

brexit.weights <- list()
  
for(i in seq_along(1:ncol(brexit.feature.liwc.n))) {
  if(is.numeric(brexit.feature.liwc.n[, i])) { 
    mb <- mean(brexit.feature.liwc.n[, i])
    mp <- mean(person.feature.liwc.n[, i])
    x = mb/mp
    brexit.weights = append(brexit.weights, x)
  } else {
    print("Not Numeric")
  }
}


person.feature.liwc.weight <- select(person.feature.liwc.n, -userID, -Created_Date.month_day)

for(i in seq_along(1:ncol(person.feature.liwc.weight))) {
person.feature.liwc.weight[, i] <- person.feature.liwc.weight[, i] * brexit.weights[[i]]
}


#person.feature.liwc.weight.cl <- cbind(person.feature.liwc.n[,1:3], person.feature.liwc.weight)

mean(person.feature.liwc.weight[, 5])
mean(brexit.feature.liwc.n[, 5])

summary(person.feature.liwc.weight.cl[, 5])
summary(brexit.feature.liwc.n[, 5])

ks.test(person.feature.liwc.weight[, 4], brexit.feature.liwc.n[, 4])
ks.test(person.feature.liwc.n[, 4], brexit.feature.liwc.n[, 4])

ks.test(person.feature.liwc.weight[, 5], brexit.feature.liwc.n[, 5])
ks.test(person.feature.liwc.n[, 5], brexit.feature.liwc.n[, 5])

ks.test(person.feature.liwc.weight[, 6], brexit.feature.liwc.n[, 6])
ks.test(person.feature.liwc.n[, 7], brexit.feature.liwc.n[, 7])

ks.test(person.feature.liwc.weight[, 181], brexit.feature.liwc.n[, 181])
ks.test(person.feature.liwc.n[, 181], brexit.feature.liwc.n[, 181])


names(person.feature.liwc.weight)

person.feature.liwc.weight$userID <- person.feature.liwc.cl$userID
person.feature.liwc.weight$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
person.feature.liwc.weight$Created_Date.month_day <- person.feature.liwc.cl$Created_Date.month_day

write.csv(person.feature.liwc.weight, file = "master_distress_weighted_correct.csv")

# Correlation Analysis of All features 

#library(mlbench)
library(caret)
# load the data

# calculate correlation matrix


#correlationMatrix <- cor(person.label.full[,5:176])
person.feature.liwc.weight.nums <- select(person.feature.liwc.weight, -Prop_Retweet, -userID, -Distress_Lab, -Created_Date.month_day)

correlationMatrix2 <- cor(person.feature.liwc.weight.nums)

# summarize the correlation matrix
print(correlationMatrix2)
# find attributes that are highly corrected (ideally >0.75)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
highlyCorrelated2 <- findCorrelation(correlationMatrix2, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)



#
# Load in Scaled Dataset
#

person.label.full <- read.csv("master_distress_weighted_correct.csv")
person.label.full <- select(person.label.full, -Prop_Retweet, -X)
person.label.full <- na.omit(person.label.full)


#Set Up Environment

library(e1071)


library(doParallel)

cores <- detectCores()

cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

library(caret)




# Create Data 

#person.label.full <- person.feature.liwc.weight

set.seed(005)
folds <- caret::createDataPartition(person.label.full$Distress_Lab, p = .75, list = FALSE)

train.df <- person.label.full[folds, ]
test.df <- person.label.full[-folds, ]



#set.seed(006)
#train.b <- train.df[sample(1:nrow(train.df), 20000,
#                              replace=FALSE),]

#testing.b <- test.df[sample(1:nrow(test.df), 5000,
#                               replace=FALSE),]


#train.model <- select(train.b, -userID, -Created_Date.month_day)
#test.model <- select(testing.b, -userID, -Created_Date.month_day)

train.model.full <- select(train.df, -userID, -Created_Date.month_day)
test.model.full <- select(test.df, -userID, -Created_Date.month_day)


#remove(brexit.feature.liwc, brexit.feature.liwc.cl, brexit.feature.liwc.n, brexit.feature.liwc.n.scale2, 
#       person.feature.liwc, person.feature.liwc.cl, person.feature.liwc.n, person.feature.liwc.n.scale2, 
#       person.feature.liwc.weight, person.feature.liwc.weight.nums, brexit.weights)


#
# Model
#

trctrl <- trainControl(method = "cv", number = 5)

# SVM 

grid_radial <- expand.grid(sigma = c(.001, .01, .05, .1, .5), C = c(0.25, 0.5, 1, 2, 4)) 


svm_Radial_Grid <- train(factor(Distress_Lab) ~., data = train.model, method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial)


svm_Radial_Grid

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = test.model)

confusionMatrix(test_pred_Radial_Grid, testing.b$Distress_Lab )

# Random Forest 


set.seed(123)

default.mtry <- sqrt(ncol(train.model)-1)

grid_rf <- expand.grid(.mtry = c(8, 15, 30, 60, 120, 234))

rf_default <- train(Distress_Lab~., 
                    data=train.model, 
                    method='rf', 
                    metric='Kappa', 
                    trControl=trctrl,
                    tuneGrid = grid_rf)


rf_default

test_pred_rf <- predict(rf_default, newdata = test.model)

confusionMatrix(test_pred_rf, test.model$Distress_Lab )

importance <- varImp(rf_default)

save(rf_default, file="distress_model_rf_20000.RData")

# Target Data 

load("distress_model_rf_20000.RData")

rf_default

target_pred_rf_full <- predict(rf_default, newdata = brexit.model.full)
confusionMatrix(target_pred_rf_full, brexit.model.full$Distress_Lab ) 


#

# RF2 

grid_rf <- expand.grid(.mtry = c(8, 15, 30, 60, 120, 229))


train.model.b <- select(train.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)
test.model.b <- select(test.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

rf_default2 <- train(Distress_Lab~., 
                    data=train.model.b, 
                    method='rf', 
                    metric='Kappa', 
                    trControl=trctrl,
                    tuneGrid = grid_rf)

# Save the big model

save(rf_default2, file="distress_model_rf2_full.RData")



importance2 <- varImp(rf_default2)
plot(importance2, 20)

test_pred_rf2 <- predict(rf_default2, newdata = test.model.b)
confusionMatrix(test_pred_rf2, test.model.b$Distress_Lab )


# LR 

log_full_train <- train(factor(Distress_Lab) ~., data = train.model, method = "glm",
                        family = "binomial", trControl=trctrl)

log_full_train

log_full_train_pred <- predict(log_full_train, newdata = test.model)
confusionMatrix(log_full_train_pred, test.model$Distress_Lab)



#
#
# Test Transfer
#
#

# Load Brexit Data 
raw.brexit <- read.csv("master_brexit_end_aug_no_lab_features_no_scale.csv")
brexit_labs <- read.csv("master_brexit_end_aug_labs_v.1_sample.csv")



#brexit_data <- read.csv("master_brexit_no_lab_nosent_scale.csv")
#brexit_labs <- read.csv("master_brexit_labs_sample_2.1.csv")

# Scale Brexit data 

raw.brexit.cl <- select(raw.brexit, -contains("Sentiment"))
raw.brexit.cl <- distinct(raw.brexit.cl)

brexit.feature.liwc.n <- raw.brexit.cl[, which(names(raw.brexit.cl) %in% names(train.df))]

# Scale Brexit 
brexit.feature.liwc.n.scale2 <- apply(brexit.feature.liwc.n[, - c(1:2)], 2, scale)
brexit.feature.liwc.n.scale2 <- data.frame(brexit.feature.liwc.n.scale2)

brexit.feature.liwc.n.scale2$userID <- brexit.feature.liwc.n$userID
#brexit.feature.liwc.n.scale2$Distress_Lab <- person.feature.liwc.cl$Distress_Lab
brexit.feature.liwc.n.scale2$Created_Date.month_day <- brexit.feature.liwc.n$Created_Date.month_day


# Merge Labelled

brexit.names.test <- filter(brexit.feature.liwc.n.scale2, userID %in% brexit_labs$userID)

brexit_data_test <- merge(brexit.feature.liwc.n.scale2, brexit_labs, by = c("userID", "Created_Date.month_day"))

# Need to make Labs Even
brexit_data_test <- brexit_data_test %>% 
  filter(Distress_Lab == 1 | Distress_Lab == 2)

brexit_data_test$Distress_Lab <- ifelse(brexit_data_test$Distress_Lab == 1, "Distress", "Control")

write.csv(brexit_data_test, "brexit_data_labelled_scale_full_300.csv")

# Clean Data for model 
brexit.model.full <- select(brexit_data_test, -userID, -Created_Date.month_day, -X, -Weekly_Text)
brexit.model.b <- select(brexit.model.full, -Total_Favorites, -Followers, -Followees, -Total_Tweets)

# Transfer Learning 
rf_brexit_test_pred <- predict(rf_default2, newdata = brexit.model.b)
caret::confusionMatrix(rf_brexit_test_pred, factor(brexit.model.b$Distress_Lab))



#
# Limit the training data to the same proportions as Brexit data 
#

table(brexit_data_test$Distress_Lab) # Control = 197, Distress = 103, Prop = .34
table(person.label.full$Distress_Lab) # Control = 38355, Distress = 42954, Prop = .52

# 42954 - 20000 = 22954 

person.label.full.distress <- person.label.full %>% 
  filter(Distress_Lab == "Distress")

distress.users <- as.character(unique(person.label.full.distress$userID))

set.seed(008)
distress.user.sample <- sample(distress.users, 300)


person.label.full.distress.sample <- person.label.full.distress %>% 
  filter(userID %in% distress.user.sample)



person.label.full.control <- person.label.full %>% 
  filter(Distress_Lab == "Control")


person.label.full.uneven <- bind_rows(person.label.full.distress.sample, person.label.full.control)

table(person.label.full.uneven$Distress_Lab) # Control = 38355, Distress = 20166, Prop = .34


write.csv(person.label.full.uneven, "distress_data_uneven.csv")












#
#
#




model.base <- svm(factor(Distress_Lab) ~ ., data=htrain4[-1], kernal = "radial basis", cost = 8) # Play with cost and gamma
model.svm.base <- predict(model.base, select(htrain4, -Distress_Lab)) 

model.svm.base.test <- predict(model.base, select(htest4, -Distress_Lab))

label <- htrain4$Distress_Lab
t.b <- table(model.svm.base, label)
sens.base <- t.b[2,2]/(t.b[2,2]+t.b[1,2])
spec.base <- t.b[1,1]/(t.b[1,1]+t.b[2,1])
t.b

label.test <- htest4$Distress_Lab
t.b.t <- table(model.svm.base.test, label)
sens.base.t <- t.b.t[2,2]/(t.b.t[2,2]+t.b.t[1,2])
spec.base.t <- t.b.t[1,1]/(t.b.t[1,1]+t.b.t[2,1])
t.b.t




#train <- htrain4[1:10000,]
#testing <- htest4[1:10000,]

svm_Radial_Grid <- train(factor(Distress_Lab) ~., data = train.b, method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial)


svm_Radial_Grid

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing.b)

confusionMatrix(test_pred_Radial_Grid, testing.b$Distress_Lab )



#
#
# Other Methods
#
#


set.seed(001)
folds <- caret::createFolds(person.label.full$Distress_Lab, k = 5)

htrain1 <- person.label.full[-folds[[1]], ]
htest1 <- person.label.full[folds[[1]], ]

htrain2 <- person.label.full[-folds[[2]], ]
htest2 <- person.label.full[folds[[2]], ]

htrain3 <- person.label.full[-folds[[3]], ]
htest3 <- person.label.full[folds[[3]], ]

htrain4 <- person.label.full[-folds[[4]], ]
htest4 <- person.label.full[folds[[4]], ]

htrain5 <- person.label.full[-folds[[5]], ]
htest5 <- person.label.full[folds[[5]], ]


remove(folds, person.feature.liwc.cl, person.feature.liwc.s)


#
#
#
# Create Training and Test Data with Cross Validation 
#
#
#



#set.seed(002)
#folds <- caret::createDataPartition(person.label.full$Distress_Lab, p = .75, list = FALSE)

#train.df <- person.label.full[folds, ]
#test.df <- person.label.full[-folds, ]

#remove(folds, person.feature.liwc.cl, person.feature.liwc.s)

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




# RBF, Cost 8 = Sens = 92.8, Spec = 77 Test - 73.3, 51.9

#sens.base1 <- sens.base
#spec.base1 <- spec.base # Crap Results with small test data

#accuracy <- 

# Peek at outcomes
#summary(model.base)
#str(model.base)


# Use caret package 

# really small sample - Ran through a bunch of small samples and recorded - took a day. 

# limit the number of input features 

sig_labs <- read.csv("features_sig_tests_cl.csv")
sig_labs$sig_flag <- ifelse(sig_labs$sig_p_value <= .01, 1, 0)
sig_labs_cl2 <- sig_labs[which(sig_labs$sig_flag == 1),]
sig_labs_names2 <- as.character(sig_labs_cl2$feature_names)



#




# Training Data 2000 

htrain4.names <- names(htrain4)
htrain4.sel <- htrain4[, which(htrain4.names %in% sig_labs_names2)]

htrain4.sel$Distress_Lab <- htrain4$Distress_Lab

htest4.sel <- htest4[, which(htrain4.names %in% sig_labs_names2)]
htest4.sel$Distress_Lab <- htest4$Distress_Lab


# 




#save.image(file="svm_experiments_small.RData")

# Train the bst parameters on full dataset 

set.seed(005)
folds <- caret::createDataPartition(person.label.full$Distress_Lab, p = .75, list = FALSE)

train.df <- person.label.full[folds, ]
test.df <- person.label.full[-folds, ]


# Training Data Full

train.df.names <- names(train.df)
train.df.sel <- train.df[, which(train.df.names %in% sig_labs_names2)]

train.df.sel$Distress_Lab <- train.df$Distress_Lab

test.df.sel <- test.df[, which(htrain4.names %in% sig_labs_names2)]
test.df.sel$Distress_Lab <- test.df$Distress_Lab


trctrl <- trainControl(method = "cv", number = 3)

grid_best <- expand.grid(sigma = c(0, .01), C = c(0, 2)) 

svm_full_train <- train(factor(Distress_Lab) ~., data = train.df.sel, method = "svmRadial",
                            trControl=trctrl,
                            tuneGrid = grid_best)

svm_full_train

svm_full_train_pred <- predict(svm_full_train, newdata = test.df.sel)

confusionMatrix(svm_full_train_pred, test.df.sel$Distress_Lab)

save(svm_full_train, file="distress_model__noNgram_v1.RData")


##### No Ngram 

log_full_train <- train(factor(Distress_Lab) ~., data = train.df.sel, method = "glm",
                        family = "binomial", trControl=trctrl)

log_full_train

log_full_train_pred <- predict(log_full_train, newdata = test.df.sel)
confusionMatrix(log_full_train_pred, test.df.sel$Distress_Lab)




#####

summary(svm_full_train)

svm_full_train$coefnames
svmImp <- varImp(svm_full_train, scale = FALSE)
plot(svmImp, top = 50)

stopCluster(cl)


## Try Brexit Prediction # need to get rid of rewteets

load(file = "distress_model__noNgram_v1.RData")

brexit_data <- read.csv("master_brexit_no_lab_nosent_scale.csv")
brexit_labs <- read.csv("master_brexit_labs_sample_2.1.csv")

sig_labs <- read.csv("features_sig_tests_cl.csv")

sig_labs$sig_flag <- ifelse(sig_labs$sig_p_value <= .01, 1, 0)
sig_labs_cl2 <- sig_labs[which(sig_labs$sig_flag == 1),]
sig_labs_names2 <- as.character(sig_labs_cl2$feature_names)


brexit_data_test <- merge(brexit_data, brexit_labs, by = c("userID", "Created_Date.month_day"))

brexit_data_test <- brexit_data_test %>% 
  filter(Distress_Lab == 1 | Distress_Lab == 2)

# Keep only sig names

brexit_data_test.names <- names(brexit_data_test)
brexit_data_test.sel <- brexit_data_test[, which(brexit_data_test.names %in% sig_labs_names2)]

brexit_data_test.sel$Distress_Lab <- brexit_data_test$Distress_Lab
brexit_data_test.sel$Prop_Retweet <- 0
brexit_data_test.sel$WordCount <- 0
brexit_data_test.sel$Distress_Lab <- ifelse(brexit_data_test.sel$Distress_Lab == 1, "Distress", "Control")

svm_full_train$coefnames
names(brexit_data_test.sel)

svm_brexit_test_pred <- predict(svm_full_train, newdata = brexit_data_test.sel)
caret::confusionMatrix(svm_brexit_test_pred, factor(brexit_data_test.sel$Distress_Lab))


brexit_data_test.sel$Predicted <- svm_brexit_test_pred
brexit_data_test.sel$Text <- brexit_data_test$Weekly_Text


# Are they drawn from the same distributions - Kolmorogov Smirnoff Tests 


setwd("C:/Users/Sean/Documents/Masters Project/Data Examples/Twitter July1_19toJuly14_19/")

target_brexit <- read.csv("brexit_target_test_v1.csv")
target_lab_brexit <- read.csv("brexit_target_labs_test_v1.csv")
test_distress <- read.csv("distress_df_train_v1.csv")

library(tidyr)
library(dplyr)

# Did they come from the same distribution - significant = Yes 
ks.test(target_brexit$Avg_Length, test_distress$Avg_Length)
ks.test(target_brexit$Followers, test_distress$Followers)
ks.test(target_brexit$affect, test_distress$affect)
ks.test(target_brexit$sad, test_distress$sad)
ks.test(target_brexit$focuspresent, test_distress$focuspresent)


ks.test(target_lab_brexit$Avg_Length, test_distress$Avg_Length)
ks.test(target_lab_brexit$Followers, test_distress$Followers)
ks.test(target_lab_brexit$affect, test_distress$affect)
ks.test(target_lab_brexit$sad, test_distress$sad)
ks.test(target_lab_brexit$focuspresent, test_distress$focuspresent)
ks.test(target_lab_brexit$anger, test_distress$anger)
#ks.test(target_lab_brexit$Distress_Lab, test_distress$Distress_Lab)


options(scipen=999)

results.ks <- list()



all.vars <- names(target_brexit[-1])
all.vars.target

for(i in all.vars) {
  df1 <- select(target_lab_brexit, i)
  df2 <- select(test_distress, i)
  #print(names(df1))
  if(is.numeric(df1[,i])) {
  ks.all <- ks.test(df1[,i], df2[,i])
  results.ks <- append(results.ks, names(df1))
  results.ks <- append(results.ks, ks.all)
  } else {
    print("non-numeric")
  }
  results.ks2 <<- results.ks
}


all.tests <- lapply(master_features_noword[-1], function(x) t.test(x ~ master_features_noword$Distress_Lab))

all.tests.df <- data.frame(matrix(unlist(all.tests), nrow=length(all.tests), byrow=T))
all.tests.df.cl <- all.tests.df[,c(1:7)]



# 




