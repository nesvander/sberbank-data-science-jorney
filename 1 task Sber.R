setwd("~/Desktop/R/kaggle/Sberbank")

library(chron)
library(data.table)
library(dplyr)
library(ggplot2)

tran <- fread("transactions.csv") 
gender <- fread("customers_gender_train.csv")

#convert amounts to usual money
tran$amount <- round(tran$amount/(pi^exp(1)), digits = 2)

#split the time
m <- data.frame(matrix(unlist(strsplit(tran$tr_datetime, " ")), nrow=NROW(tran$amount), byrow=T))

#remove wrong specified time (like 13:34:60)
m$X2 <- gsub("\\d\\d$", "59", m$X2) 
m$X2 <- times(m$X2)

#merge all in one 
tran <- cbind(tran, m)
tran$tr_datetime <- NULL
rm(m)

#rename and factorize 
colnames(tran)[6] <- "day"
colnames(tran)[7] <- "time"
tran$mcc_code <- as.factor(tran$mcc_code)
tran$tr_type <- as.factor(tran$tr_type)

################
#generate mcc features (20 min)
customers <- unique(tran$customer_id)
matrix_mcc <- t(sapply(customers, 
                       function(x) table(tran[which(tran$customer_id == x),]$mcc_code)))
row.names(matrix_mcc) <- customers

################
#generate type features

matrix_tr_type <- t(sapply(customers, 
                       function(x) table(tran[which(tran$customer_id == x),]$tr_type)))
row.names(matrix_tr_type) <- customers

tran$exp <- ifelse(tran$amount < 0, tran$amount, NA)
tran$earn <- ifelse(tran$amount > 0, tran$amount, NA)

stat_amount <- tran %>%
                  group_by(customer_id) %>%
                  summarise(min = min(amount), #general amount stat
                            max = max(amount),
                            n_unique = n_distinct(amount), 
                            mean_earn = mean(earn, na.rm = T), #earnings stat
                            iqr_earn = IQR(earn, na.rm = T),
                            first_quart_earn = quantile(earn, 0.25, na.rm = T),
                            median_earn = median(earn, na.rm = T),
                            third_quart_earn = quantile(earn, 0.75, na.rm = T),
                            mean_exp = mean(exp, na.rm = T), #expenditures stat
                            iqr_exp = IQR(exp, na.rm = T),
                            first_quart_exp = quantile(exp, 0.25, na.rm = T),
                            median_exp = median(exp, na.rm = T),
                            third_quart_exp = quantile(exp, 0.75, na.rm = T))

#some customers don't have earnings or expenditures, alter the stat from NA to 0. 
stat_amount <- apply(stat_amount, 2, function(x) ifelse(is.na(x) == T, 0, x))
stat_amount <- as.data.frame(stat_amount)

########################
#generate time features
#separate day on 8 periods
tran$midnight <- ifelse(train3 == 0.00, 1, 0)
tran$night <- ifelse(train3 > 0.00 & train3 < 6.00, 1, 0)
tran$morning <- ifelse(train3 >= 6.00 & train3 < 9.00, 1, 0)
tran$early_work <- ifelse(train3 >= 9.00 & train3 < 12.00, 1, 0)
tran$lunch <- ifelse(train3 >= 12.00 & train3 < 14.00, 1, 0)
tran$late_work <- ifelse(train3 >= 14.00 & train3 < 17.00, 1, 0)
tran$evening <- ifelse(train3 >= 17.00 & train3 < 20.00, 1, 0)
tran$late_evening <- ifelse(train3 >= 20.00 & train3 < 24.00, 1, 0)
rm(train3)

stat_time <- tran %>%
      group_by(customer_id) %>%
      summarise(midnight = sum(midnight),
                night = sum(night),
                morning = sum(morning),
                early_work = sum(early_work),
                lunch = sum(lunch),
                late_work = sum(late_work),
                evening = sum(evening),
                late_evening = sum(late_evening))

##################
#generate day features
tran$day <- as.numeric(tran$day)
tran$days_ymd <- as.Date(x = tran$day, origin = "2014-11-01")
tran$weekday  <- weekdays(tran$days_ymd)

tran$mon <- ifelse(tran$weekday == "понедельник", 1, 0)
tran$tue <- ifelse(tran$weekday == "вторник", 1, 0)
tran$wed <- ifelse(tran$weekday == "среда", 1, 0)
tran$thu <- ifelse(tran$weekday == "четверг", 1, 0)
tran$fri <- ifelse(tran$weekday == "пятница", 1, 0)
tran$sat <- ifelse(tran$weekday == "суббота", 1, 0)
tran$sun <- ifelse(tran$weekday == "воскресенье", 1, 0)

stat_day <- tran %>%
  group_by(customer_id) %>%
  summarise(day1 = sum(mon),
            day2 = sum(tue),
            day3 = sum(wed),
            day4 = sum(thu),
            day5 = sum(fri),
            day6 = sum(sat),
            day7 = sum(sun))

#generate month features
tran$month <- format(tran$days_ymd, "%b")
tran$mon1 <- ifelse(tran$month == "янв", 1, 0)
tran$mon2 <- ifelse(tran$month == "фев", 1, 0)
tran$mon3 <- ifelse(tran$month == "мар", 1, 0)
tran$mon4 <- ifelse(tran$month == "апр", 1, 0)
tran$mon5 <- ifelse(tran$month == "май", 1, 0)
tran$mon6 <- ifelse(tran$month == "июн", 1, 0)
tran$mon7 <- ifelse(tran$month == "июл", 1, 0)
tran$mon8 <- ifelse(tran$month == "авг", 1, 0)
tran$mon9 <- ifelse(tran$month == "сен", 1, 0)
tran$mon10 <- ifelse(tran$month == "окт", 1, 0)
tran$mon11 <- ifelse(tran$month == "ноя", 1, 0)
tran$mon12 <- ifelse(tran$month == "дек", 1, 0)

stat_month <- tran %>%
              group_by(customer_id) %>%
                  summarise(mon1 = sum(mon1),
                            mon2 = sum(mon2),
                            mon3 = sum(mon3),
                            mon4 = sum(mon4),
                            mon5 = sum(mon5),
                            mon6 = sum(mon6),
                            mon7 = sum(mon7),
                            mon8 = sum(mon8),
                            mon9 = sum(mon9),
                            mon10 = sum(mon10),
                            mon11 = sum(mon11),
                            mon12 = sum(mon12))

matrix_final <- cbind(matrix_mcc, matrix_tr_type)
matrix_final <- as.data.frame(matrix_final)
matrix_final[,"customer_id"] <- customers
matrix_final <- inner_join(matrix_final, stat_amount, by = "customer_id")
matrix_final <- inner_join(matrix_final, stat_time, by = "customer_id")
matrix_final <- inner_join(matrix_final, stat_day, by = "customer_id")
matrix_final <- inner_join(matrix_final, stat_month, by = "customer_id")

######
##features_02.11.RData
#####

train_matrix <- matrix_final[which(matrix_final$customer_id %in% gender$customer_id), ]
test_matrix <- matrix_final[-which(matrix_final$customer_id %in% gender$customer_id), ]
rm(matrix_final)

train_matrix <- inner_join(train_matrix, gender, by = "customer_id")
customers_train <- train_matrix[,"customer_id"]
train_label <- train_matrix$gender
train_matrix[,"customer_id"] <- NULL

test_matrix[,"gender"] <- NA
customers_test <- test_matrix[,"customer_id"]
test_label <- c(rep(NA, 3000))
test_matrix[,"customer_id"] <- NULL
rm(tran)
rm(gender)

##Logit
#struggle singularity, use PCA
### I use xgboost importanceMatrix from the other simple model, sorry.    
train_pca <- prcomp(train_matrix[,importanceMatrix$Feature[-1]],
                    center = TRUE,
                    scale. = TRUE) 
#take 120 most important features 
logit_train <- train_pca$x[,1:120]
logit_train <- as.data.frame(cbind(logit_train, train_label))

logit_test <- as.data.frame(predict(train_pca, test_matrix[,importanceMatrix$Feature[-1]])[,1:120])
logit_test <- cbind(logit_test, test_label)

#train the model
model_logit <- lm(train_label ~., data = logit_train)
summary(model_logit) #coefficients are mainly significant 

logit_results_test <- predict(model_logit, logit_test[,-121])
logit_results_train <- predict(model_logit, logit_train[,-121])

#use logit-predictions as a feature
train_matrix <- cbind(train_matrix, logit_results_train)
test_matrix <- cbind(test_matrix, logit_results_test)
colnames(train_matrix)[303] <- "logit_results_test"


train_matrix <- as.data.frame(train_matrix)
test_matrix <- as.data.frame(test_matrix)

#best 120 features (100, 140, 160 work not so good )
#I use that importanceMatrix again.
train_matrix_120 <- as.matrix(train_matrix[,1:120])
test_matrix_120 <- as.matrix(test_matrix[,1:120])

rm(logit_test)
rm(logit_train)
rm(logit_results_train)
rm(logit_results_test)
rm(model_logit)
rm(train_pca)

##################

library(xgboost)
dtrain <- xgb.DMatrix(train_matrix_120,label=train_label, missing=NA)
dtest <- xgb.DMatrix(test_matrix_120,label=test_label,missing=NA)

###############
#tuned xgboost
param <- list(objective = "binary:logistic",
              eval_metric= "auc",
              eta=0.015,
              max_depth = 5,
              colsample_bytree = 0.7, 
              subsample = 0.8)

fit_cv <- xgb.cv(params=param,
                 data=dtrain,
                 nrounds=1500,
                 nfold=3,
                 early.stop.round = 100,
                 print.every.n = 5,
                 verbose=1)

model <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=1300,
                     verbose=1)
pred <- predict(model, dtest)

#feature importance 

names2 <- dimnames(test_matrix)
names2[[2]]
importanceMatrix2 <- xgb.importance(feature_names = names2[[2]], model = model)
importanceMatrix2$Feature
xgb.plot.importance(importanceMatrix[1:10,])

final <- arrange(data_frame(customer_id = customers_test, gender = pred), customer_id)

write.csv(x = final, file = "sber_1", row.names = F)


################
################
##Tuning and CV

library(mlr)

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
?makeLearner
xg_set <- makeLearner("classif.xgboost", 
                      eval_metric = "auc", 
                      predict.type = "prob",
                      par.vals = list(objective = "binary:logistic",
                                      eval_metric = "error",
                                      print.every.n = 2001,
                                      nrounds = 1500))

#define parameters for tuning
xg_ps <- makeParamSet(
    makeDiscreteParam("eta", values = c(0.015, 0.025, 0.05)),
    makeDiscreteParam("max_depth", values = c(5, 7, 9)),
    makeDiscreteParam("colsample_bytree", values = c(0.6, 0.7, 0.8)),
    makeDiscreteParam("subsample", values = c(0.7, 0.8, 0.9))
  )

#define search function
#rancontrol <- makeTuneControlRandom(maxit = 3L) #do 60 iterations
rancontrol <- makeTuneControlGrid()

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#tune parameters
?tuneParams
xgb_train <- as.data.frame(train_matrix_120)
colnames(xgb_train) <- sub("(^\\d)", "dd\\1", colnames(xgb_train))
xgb_train <- cbind(xgb_train, train_label)
xg_task <- makeClassifTask(data = xgb_train,
                           target = "train_label")

library(ROCR)

xg_tune <- tuneParams(learner = xg_set, 
                      task = xg_task, 
                      resampling = set_cv, 
                      measures = auc,
                      par.set = xg_ps,
                      control = rancontrol)

#set parameters
xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
model <- train(xg_new, trainTask)

#test model
predict.xg <- predict(xgmodel, testTask)