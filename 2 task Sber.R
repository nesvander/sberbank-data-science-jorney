#load file from 1 script
load("~/Desktop/R/kaggle/Sberbank/features_2.11.RData")
tran_mcc  <- fread("tr_mcc_codes.csv")

tran <- as.data.table(filter(tran, amount < 0))

volume <- tran %>%
  group_by(mcc_code, day) %>%
  summarise(amount = sum(abs(amount)))
volume$day <- as.factor(volume$day)

n <- spread(data = volume, day, value = amount, fill = 0)
n <- n[,-1]
m <- t(apply(n, 1, function(x) {
  mean <- mean(x)
  max <- max(x)
  first_quart = quantile(x, 0.25) + 0.001
  median = median(x) + 0.002
  third_quart = quantile(x, 0.75) + 0.003
  rbind(mean, max, first_quart, median, third_quart)
} ))





rm(list = ls())
gc()

# загрузка библиотек
library(data.table)
library(xgboost)
library(caret)

# загрузка данных
tran <- fread('transactions.csv')
cust <- fread('customers_gender_train.csv')
mcc <- fread('tr_mcc_codes.csv')
tr_type <- fread('tr_types.csv')

# трансформируем данные
mcc$mcc_code <- paste('mcc', as.character(mcc$mcc_code), sep = '_')
tran[, c('day', 'time') := tstrsplit(tr_datetime, ' ', fixed = TRUE, type.convert = TRUE)]
tran[, tr_datetime := NULL]
tran <- tran[amount < 0]

# готовим train_grid
group_data <- tran[, .(amount = sum(amount)), by = c('day', 'mcc_code')]
train_grid <- as.data.table(expand.grid(mcc_code = unique(tran$mcc_code), day = unique(tran$day)))
train_grid <- merge(train_grid, group_data, by = c('day', 'mcc_code'), all.x = T)
train_grid[is.na(amount), amount := 0]
print(sum(is.na(train_grid$amount)))
setkey(train_grid, day, mcc_code)

# готовим test_grid
test_grid <- as.data.table(expand.grid(unique(tran$mcc_code), 1:30 + max(tran$day)))
colnames(test_grid) <- c('mcc_code', 'day')
test_grid[, amount := NA]
setkey(test_grid, day, mcc_code)

# сольем и создадим фичи
all_data <- rbind(train_grid, test_grid)
lag_int <- length(unique(all_data$mcc_code))
for (i in 30:60) {
  all_data[, paste('day', i, sep = '_') := shift(all_data$amount, lag_int * i)]
}

#train_grid <- na.omit(train_grid)
all_data[, dw := day %% 7]
all_data[, dw := dw + 1]

all_data <- as.data.table(left_join(all_data, my_feature, by = "mcc_code"))

# делим обратно на трейн и тест
train_grid <- all_data[!is.na(amount)]
train_grid <- na.omit(train_grid)
test_grid <- all_data[is.na(amount)]

# готовим test_grid
#test_grid <- as.data.table(expand.grid(unique(tran$mcc_code), 1:30 + max(tran$day)))
#colnames(test_grid) <- c('mcc_code', 'day')
#for (i in 30:60) {
#  res <- unlist(apply(test_grid, 1, function(x)
#    ifelse(length(group_data[day == (x[2] - i) &
#                               mcc_code == x[1], amount]) == 0, 0, group_data[day == (x[2] - i) &
#                                                                                mcc_code == x[1], amount])))
#  test_grid[, paste('day', as.character(i), sep = '_')] <- res
#}
#test_grid <- as.data.table(test_grid)
#test_grid[, dw := day %% 7]
#test_grid[, dw := dw + 1]
#setkey(test_grid, day, mcc_code)

# запишем промежуточный результат
write.csv(train_grid, 't2_train_grid.csv', row.names = F, quote = F)
write.csv(test_grid, 't2_test_grid.csv', row.names = F, quote = F)
#train_grid <- fread('t2_train_grid.csv')
#test_grid <- fread('t2_test_grid.csv')

X <- train_grid[, -c('day', 'mcc_code', 'amount'), with = F]
dw <- as.data.frame(X)
dw <- dw[,32:37]
X <- log(-X + 1)
X[,32:37] <- as.data.table(dw)
#y <- train_grid$amount
y <- log(-train_grid$amount + 500)
X_pred <- test_grid[, -c('day', 'mcc_code', 'amount'), with = F]
dw <- as.data.frame(X_pred)
dw <- dw[,32:37]
X_pred <- log(-X_pred + 1)
X_pred[,32:37] <- as.data.table(dw)

#xgboost
xgbGrid <- expand.grid(
  eta = 0.2,
  nrounds = c(80, 100, 120) ,#OK
  max_depth = 5, #OK
  colsample_bytree = 0.2, #OK
  min_child_weight = 13, #OK
  gamma = 14 #OK
)
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 2, verboseIter = TRUE)

m1 <- train(X, y,
            method = 'xgbTree',
            trControl = fitControl,
            metric = 'rmse',
            tuneGrid = xgbGrid
)
m1$bestTune

k = 20
param <- list( 
  eta = 0.2/k,
  max_depth = 5,
  colsample_bytree = 0.2,
  min_child_weight = 13,
  gamma = 14,
  subsample = 0.7,
  objective = 'reg:linear',
  eval_metric = "rmse"
)

model <- xgboost(data = as.matrix(X), label = y, params = param,
                 nrounds = 80*k, print_every_n = 5, early_stopping_rounds = 100)
f_imp <- xgb.importance(feature_names = colnames(as.matrix(X)), model = model)
xgb.plot.importance(f_imp[Gain > 0.01])

res <- exp(predict(model, as.matrix(X_pred))) - 500
test_grid[, volume := res]
ans <- test_grid[, .(mcc_code, day, volume)]
write.csv(ans, 'sber_final.csv', row.names = F, quote = F)
