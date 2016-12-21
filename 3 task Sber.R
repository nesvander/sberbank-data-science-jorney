rm(list = ls())
gc()

# загрузка библиотек
library(data.table)
library(xgboost)
library(caret)
library(tidyr)
#library(FeatureHashing)

xgb_predict <- function(cust_id) {
  print(paste('Клиент',  which(cust_test == cust_id)))
  cust_grid <- as.data.table(expand.grid(customer_id = cust_id, mcc_code = unique(mcc$mcc_code), month_num = 0:15))
  cust_tran <- tran[customer_id == cust_id, .(volume = sum(amount)), by = c('customer_id', 'mcc_code', 'month_num')]
  cust_grid <- merge(cust_grid, cust_tran, by = c('customer_id', 'mcc_code', 'month_num'), all.x = T)
  cust_grid[is.na(volume), volume := 0]
  
  train_grid <- spread(cust_grid, month_num, volume, fill = 0, sep = '_')
  train_grid[, mcc_code := factor(mcc_code)]
  setkey(train_grid, mcc_code)
  
  res <- train_grid[, customer_id:mcc_code, with = F]
  dv <- dummyVars(~ 0 + ., train_grid)
  train_data <- as.data.table(predict(dv, train_grid))
  X <- train_data[, mcc_code.742:month_num_14, with = F]
  y <- train_data$month_num_15
  
  cust_grid[, month_num := month_num - 1]
  test_grid <- spread(cust_grid[month_num %in% 0:14], month_num, volume, fill = 0, sep = '_')
  test_grid[, mcc_code := factor(mcc_code)]
  setkey(test_grid, mcc_code)
  dv <- dummyVars(~ 0 + ., test_grid)
  X_pred <- as.data.table(predict(dv, test_grid))[, mcc_code.742:month_num_14, with = F]
  
  k <- 32
  param <- list( 
    eta = 0.2/k,
    max_depth = 5,
    colsample_bytree = 0.7,
    min_child_weight = 1,
    gamma = 0.6,
    subsample = 0.7,
    objective = 'reg:linear',
    eval_metric = "rmse"
  )
  model <- xgboost(data = as.matrix(X), label = y, params = param, nrounds = 100*k, print_every_n = 500, early_stopping_rounds = 100)
  res$volume <- predict(model, as.matrix(X_pred))
  
  return(res)
}

# загрузка данных
tran <- fread('transactions.csv')
cust <- fread('customers_gender_train.csv')
mcc <- fread('tr_mcc_codes.csv')
tr_type <- fread('tr_types.csv')
cust_test <- unique(tran$customer_id)
cust_test <- cust_test[!cust_test %in% cust$customer_id]

# трансформируем данные
#mcc$mcc_code <- paste('mcc', as.character(mcc$mcc_code), sep = '_')
tran[, c('day', 'time') := tstrsplit(tr_datetime, ' ', fixed = TRUE, type.convert = TRUE)]
tran[, tr_datetime := NULL]
tran <- tran[amount < 0]
tran[, day :=  day + 29 - max(day) %% 30]
tran[, month_num := day %/% 30]
tran[, c('time', 'term_id', 'tr_type', 'day') := NULL]
tran[, amount := - amount]
tran[, amount := log(amount + 1)]
#tran <- tran[customer_id %in% cust_test]


# > var2
gr_data <- tran[, .(volume = sum(amount)), by = c('customer_id', 'mcc_code', 'month_num')]
gr_data <- gr_data[month_num %in% 10:15]
train_grid <- spread(gr_data, month_num, volume, fill = 0, sep = '_')
train_grid[, mcc_code := factor(mcc_code)]
setkey(train_grid, customer_id, mcc_code)

dv <- dummyVars(~ 0 + ., train_grid)
train_data <- as.data.table(predict(dv, train_grid))
X <- train_data[, mcc_code.742:month_num_14, with = F]
y <- train_data$month_num_15

gr_data[, month_num := month_num - 1]
test_grid <- spread(gr_data[month_num %in% 10:14], month_num, volume, fill = 0, sep = '_')
test_grid[, mcc_code := factor(mcc_code)]
setkey(test_grid, customer_id, mcc_code)
res <- test_grid[, customer_id:mcc_code, with = F]
dv <- dummyVars(~ 0 + ., test_grid)
X_pred <- as.data.table(predict(dv, test_grid))[, mcc_code.742:month_num_14, with = F]

#scale
preProc <- preProcess(X, method=c("center", "scale"))
X <- predict(preProc, X)
X_pred <- predict(preProc, X_pred)

#caret
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 3, verboseIter = TRUE)
model <- train(X, y,
               method = 'lm',
               trControl = fitControl,
               metric = 'rmse'
)
model$results

res$volume <- predict(model, X_pred)
answer <- as.data.table(expand.grid(customer_id = cust_test, mcc_code = unique(mcc$mcc_code)))
answer[, mcc_code := factor(mcc_code)]
answer <- merge(answer, res, by = c('customer_id', 'mcc_code'), all.x = T)
answer[is.na(volume), volume := 0]
answer[, volume := exp(volume)]
answer[volume <= 1, volume := 0]
answer[volume == Inf, volume := 0]
answer[volume >= 1e6, volume := 0]
answer[, volume := round(volume, 2)]
write.csv(answer, 'task3_lm.csv', row.names = F, quote = F)