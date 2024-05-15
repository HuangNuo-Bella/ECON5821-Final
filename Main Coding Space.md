#### ECON5821-Final
### Data treatment
# import the file
install.packages("openxlsx")
library(openxlsx)
url <- "https://github.com/zhentaoshi/Econ5821/raw/main/data_example/US_PCE_training.xlsx"
data <- read.xlsx(url)
head(data)
# reformat the dataframe
df0 = t(data)
colnames(df0) = df0[1,]
df0 = df0[2:nrow(df0),]
df0 = apply(df0, 2, as.numeric)
df0 = df0[, c(2,1,3:206)]
head(df0)
# check if there is NA
any(is.na(df0))
#calculate the month inflation rate
IR = df0[,2]
IR[1] = 0
for (i in 2:nrow(df0)){
    IR[i] = (log(df0[i,2]) - log(df0[i-1,2])) * 12
    }
head(IR)
# data with x variables
df = cbind(df0, "Inflation rate of month t" = IR)
df = df[, c(1,207,2,3:206)]
head(df)
dim(df)
# transform the all variables into log difference
logdf = df0
logdf[1,2:ncol(logdf)] = 0
for (i in 2:nrow(df0)){
    logdf[i,2:ncol(df0)] = (log(df0[i,2:ncol(df0)]) - log(df0[i-1,2:ncol(df0)])) * 12
    }
# data with x variables of log difference
colnames(logdf)[2] = "Inflation rate of month t"
head(logdf)
dim(logdf)



### Feature selection
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)

# Delete the third column (PCE should be excluded now)
df <- df[, -3]
library(rpart)
library(stats)

target <- df[, 2] # IR index
predictors <- df[, -2] # Exclude the target variable

## Method 1: Decision tree model with method anova
tree_model <- rpart(target ~ ., data = predictors, method = "anova")

# Get and sort feature importance
importance <- tree_model$variable.importance
sorted_importance <- importance[order(importance, decreasing = TRUE)]

# Get top 25 features
top_25_features <- names(sorted_importance)[1:25]
print(top_25_features)

## Method 2: PCA (dimensional reduction method)
pca_result <- prcomp(predictors, scale. = TRUE)
# Get loadings
loadings <- pca_result$rotation

# Get the names of the original features
feature_names <- colnames(predictors)
top_feature_names <- character()

# Loop through each principal component
for (i in 1:25) {
  # Get the indices of the top 25 features for the current PC
  top_features_indices <- order(abs(loadings[, i]), decreasing = TRUE)[1:25]
  # Get the names of the top 25 features for the current PC
  top_feature_names <- c(top_feature_names, feature_names[top_features_indices])
}

# Print top 25 feature names
print(top_feature_names[1:25])






### Run LASSO
## Use features with decision tree 
library(dplyr)
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
df <- df[, -3]
data <- df[, top_25_features]
data <- cbind(data, IR)

# Using the first 731 rows for 1 month ahead
data_1m_LASSO <- data[1:731, c(top_25_features, "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m_LASSO <- data[1:729, c(top_25_features, "IR")]
# Using the first 720 rows for 12 months ahead
data_12m_LASSO <- data[1:720, c(top_25_features, "IR")]

# create function to fit model and calculate the MSE
library(glmnet)
fit_lasso_and_calculate_rmse <- function(data, forecast_horizon) {
  data <- data %>%
    mutate(lagged_IR = lag(IR, forecast_horizon)) %>%
    na.omit()
  X <- as.matrix(data[, top_25_features])
  y <- data$lagged_IR
  # Fit Lasso model using cross-validation
  set.seed(123)
  lasso_model <- cv.glmnet(X, y, alpha = 1, family = "gaussian")
  # Optimal lambda
  optimal_lambda <- lasso_model$lambda.min
  LASSO_final_model <- glmnet(X, y, alpha = 1, lambda = optimal_lambda)
  LASSO_predict <- predict(LASSO_final_model, newx = X, s = optimal_lambda, type = "response")
  rmse <- sqrt(mean((LASSO_predict - y)^2))
  return(rmse)
}

rmse_1m_LASSO <- fit_lasso_and_calculate_rmse(data_1m_LASSO, 1)
rmse_3m_LASSO <- fit_lasso_and_calculate_rmse(data_3m_LASSO, 3)
rmse_12m_LASSO <- fit_lasso_and_calculate_rmse(data_12m_LASSO, 12)


# Print RMSE values
print(paste("RMSE of LASSO method for 1 month ahead:", rmse_1m_LASSO))
print(paste("RMSE of LASSO method for 3 months ahead:", rmse_3m_LASSO))
print(paste("RMSE of LASSO method for 12 months ahead:", rmse_12m_LASSO))



## Use features with PCA (the results cannot converge, thus this method does not use)
library(dplyr)
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
df <- df[, -3]
data <- df[, top_feature_names[1:25]]
data <- cbind(data, IR)

# Using the first 731 rows for 1 month ahead
data_1m <- data[1:731, c(top_feature_names[1:25], "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m <- data[1:729, c(top_feature_names[1:25], "IR")]
# Using the first 720 rows for 12 months ahead
data_12m <- data[1:720, c(top_feature_names[1:25], "IR")]

# create function to fit model and calculate the MSE
library(glmnet)
fit_lasso_and_calculate_rmse <- function(data, forecast_horizon) {
  data <- data %>%
    mutate(lagged_IR = lag(IR, forecast_horizon)) %>%
    na.omit()
  X <- as.matrix(data[, top_feature_names[1:25]])
  y <- data$lagged_IR
  # Fit Lasso model using cross-validation
  set.seed(123)
  lasso_model <- cv.glmnet(X, y, alpha = 1, family = "gaussian")
  # Optimal lambda
  optimal_lambda <- lasso_model$lambda.min
  LASSO_final_model <- glmnet(X, y, alpha = 1, lambda = optimal_lambda)
  LASSO_predict <- predict(LASSO_final_model, newx = X, s = optimal_lambda, type = "response")
  #rmse <- sqrt(mean((LASSO_predict - y)^2))
  #return(rmse)
}

rmse_1m <- fit_lasso_and_calculate_rmse(data_1m, 1)
rmse_3m <- fit_lasso_and_calculate_rmse(data_3m, 3)
rmse_12m <- fit_lasso_and_calculate_rmse(data_12m, 12)

# Print RMSE values (the results cannot converge, thus, LASSO uses the decision tree feature selection)
print(paste("RMSE for 1 month ahead:", rmse_1m))
print(paste("RMSE for 3 months ahead:", rmse_3m))
print(paste("RMSE for 12 months ahead:", rmse_12m)) 



#AR model 
library(tseries)
plot(IR)
selected_IR <- as.data.frame(IR)
n <- 1
selected_df <- as.matrix(selected_IR[2:(nrow(selected_IR)-n),])
ts_df <- ts(selected_df)

#Run the AR(1) first
ar_model <- ar(ts_df, p <- 1)
print(ar_model)
#prediction for AR(1)
pre_IR <- predict(ar_model, n.ahead = 1)
#MSE for AR(1)
ar_mse <- mean((selected_IR[(nrow(selected_IR)-n+1):nrow(selected_IR), ]-pre_IR$pred)^2)

#Examine autocorrelation in data
acf_results <- acf(ts_df, plot = TRUE, lag.max = 20)
print(acf_results$acf)
pacf_results <- pacf(ts_df, plot = TRUE, lag.max = 20)
print(pacf_results$pacf)
#Choose p = 15 according to the PACF
p <- 15

#AR model function
ARmodel <- function(n, p, data) {
    data <- as.data.frame(data)
    selected_df <- as.matrix(data[2:(nrow(data)-n),])
    ts_df <- ts(selected_df)
    #Examine autocorrelation in data
    acf_results <- acf(ts_df, plot = FALSE)
    pacf_results <- pacf(ts_df, plot = FALSE)
    #AR model
    ar_model <- ar(ts_df, p=p)
    print(ar_model)
    #prediction
    pre_IR <- predict(ar_model, n.ahead = n)
    #MSE
    ar_mse <- mean((data[(nrow(data)-n+1):nrow(data), ]-pre_IR$pred)^2)
    return(ar_mse)
}
#forcast for y(t+1)
n1 <- 1
print(ARmodel(n1,p,IR))
#forcast for y(t+3)
n3 <- 3
print(ARmodel(n3,p,IR))
#forcast for y(t+12)
n12 <- 12
print(ARmodel(n12,p,IR))


#ARIMA model  
library(forecast)
#Stationary test
adf.test(ts_df, alternative = "stationary")
#Pure random test
Box.test(ts_df, lag = 9)
Box.test(ts_df, lag = 15)

#Run the ARIMA model
library(forecast)
arima_model <- auto.arima(ts_df, trace=TRUE)
print(arima_model)

#ARIMA model function
ARIMAmodel <- function(n,data){
    data <- as.data.frame(data)
    selected_df <- as.matrix(data[2:(nrow(data)-n),])
    ts_df <- ts(selected_df)
    #ARIMA model
    arima_model <- arima(ts_df, order = c(0,1,3))
    print(arima_model)
    #prediction
    pre_IR <- predict(arima_model, n.ahead = n)
    #MSE
    arima_mse <- mean((data[(nrow(data)-n+1):nrow(data), ]-pre_IR$pred)^2)
    return(arima_mse)
}
#forcast for y(t+1)
n1 <- 1
print(ARIMAmodel(n1,IR))
#forcast for y(t+3)
n3 <- 3
print(ARIMAmodel(n3,IR))
#forcast for y(t+12)
n12 <- 12
print(ARIMAmodel(n12,IR))

    




### XGBoost
## Use features with decision tree 
library(dplyr)
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
df <- df[, -3]
data <- df[, top_25_features]
data <- cbind(data, IR)

# Using the first 731 rows for 1 month ahead
data_1m_XGBoost <- data[1:731, c(top_25_features, "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m_XGBoost <- data[1:729, c(top_25_features, "IR")]
# Using the first 720 rows for 12 months ahead
data_12m_XGBoost <- data[1:720, c(top_25_features, "IR")]

# create function to fit model and calculate the MSE
library(readxl)
library(xgboost)
fit_XGBoost_and_calculate_rmse <- function(data, forecast_horizon) {
  data <- data %>%
    mutate(lagged_IR = lag(IR, forecast_horizon)) %>%
    na.omit()
  X <- as.matrix(data[, top_25_features])
  y <- data$lagged_IR
  dtrain <- xgb.DMatrix(data = X, label = y, missing = 0)
  # Define Parameters for XGBoost
  params <- list (
    objective = "reg:squarederror",  #Objective function for regression
    eval_metric = "rmse",             #Metric to evaluate the model
    max_depth = 5,                    #Control the depth of each tree
    eta = 0.05,                       #Learning rate
    subsample = 0.8,                  #Fraction of data per boosting round
    colspample_bytree = 0.3           #Fraction of features sampled per tree
  )
  #Train the XGBoost Model
  num_round <- 100                    #Number of boosting rounds
  
  XGBst <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = num_round,
    verbose = 1
  )
  #Predict the XGBoost
  XGBoost_predict <- predict(XGBst, dtrain)
  rmse <- sqrt(mean((XGBoost_predict - y)^2))
  return(rmse)
}

rmse_1m_XGBoost <- fit_XGBoost_and_calculate_rmse(data_1m_XGBoost, 1)
rmse_3m_XGBoost <- fit_XGBoost_and_calculate_rmse(data_3m_XGBoost, 3)
rmse_12m_XGBoost <- fit_XGBoost_and_calculate_rmse(data_12m_XGBoost, 12)

# Print RMSE values
print(paste("RMSE of XGBoost method for 1 month ahead:", rmse_1m_XGBoost))
print(paste("RMSE of XGBoost method for 3 months ahead:", rmse_3m_XGBoost))
print(paste("RMSE of XGBoost method for 12 months ahead:", rmse_12m_XGBoost))



## Use features with PCA (the performance of rmse is worse than decision trees one)
library(dplyr)
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
data <- df[, top_feature_names[1:25]]
data <- cbind(data, IR)

# Using the first 731 rows for 1 month ahead
data_1m <- data[1:731, c(top_feature_names[1:25], "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m <- data[1:729, c(top_feature_names[1:25], "IR")]
# Using the first 720 rows for 12 months ahead
data_12m <- data[1:720, c(top_feature_names[1:25], "IR")]

# create function to fit model and calculate the MSE
library(readxl)
library(xgboost)
fit_XGBoost_and_calculate_rmse <- function(data, forecast_horizon) {
  data <- data %>%
    mutate(lagged_IR = lag(IR, forecast_horizon)) %>%
    na.omit()
  X <- as.matrix(data[, top_feature_names[1:25]])
  y <- data$lagged_IR
  dtrain <- xgb.DMatrix(data = X, label = y, missing = 0)
  # Define Parameters for XGBoost
  params <- list (
    objective = "reg:squarederror",  #Objective function for regression
    eval_metric = "rmse",             #Metric to evaluate the model
    max_depth = 5,                    #Control the depth of each tree
    eta = 0.05,                       #Learning rate
    subsample = 0.8,                  #Fraction of data per boosting round
    colspample_bytree = 0.3           #Fraction of features sampled per tree
  )
  #Train the XGBoost Model
  num_round <- 100                    #Number of boosting rounds
  
  XGBst <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = num_round,
    verbose = 1
  )
  #Predict the XGBoost
  XGBoost_predict <- predict(XGBst, dtrain)
  rmse <- sqrt(mean((XGBoost_predict - y)^2))
  return(rmse)
}

rmse_1m <- fit_XGBoost_and_calculate_rmse(data_1m, 1)
rmse_3m <- fit_XGBoost_and_calculate_rmse(data_3m, 3)
rmse_12m <- fit_XGBoost_and_calculate_rmse(data_12m, 12)

# Print RMSE values
print(paste("RMSE for 1 month ahead:", rmse_1m))
print(paste("RMSE for 3 months ahead:", rmse_3m))
print(paste("RMSE for 12 months ahead:", rmse_12m))

### rf
IR[1] <- 0
df <- cbind(logdf, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
head(df)

# omit NA
df <- na.omit(df)  
head(df)

install.packages("randomForest")  
library(randomForest) 

# set x，y
y <- df$IR    
x <- df[, !names(df) %in% "IR", drop = FALSE] 

# Train a random forest model 
rf_model <- randomForest(x, y, importance = TRUE, ntree = 500)  
  
# Extract variable importance  
importance <- importance(rf_model)  
var_importance <- importance[, "IncNodePurity"]  # 或者使用 "MeanDecreaseAccuracy"  

# Sort variables based on importance 
var_order <- order(var_importance, decreasing = TRUE)  

if (length(var_importance) >= 25) {  
  # Get the names of the top 25 most important variables 
  top_25_var_names <- names(x)[var_order[1:25]]  
    
  # Extract rows for these 25 variables from the original dataframe df  
  df25 <- df[, top_25_var_names, drop = FALSE]  
    
  # Print the new dataframe df25  
  head(df25)  
} else {  
  print("数据集中没有足够的特征来获取前25个最重要的特征。")  
}
df25$IR <- df$IR
head(df25)

# Test set and Training set (split by each month)
# 1 month  
train_set1 <- df25[1:731, ]  
test_set1 <- df25[732:nrow(df), ]  
test_set1 <- na.omit(test_set1) 
head(train_set1)  
head(test_set1)

# 3 month  
train_set2 <- df25[1:729, ]  
test_set2 <- df25[730:nrow(df), ]  
head(train_set2)  
head(test_set2)

# 12 month  
train_set3 <- df25[1:720, ]  
test_set3 <- df25[721:nrow(df), ]   
head(train_set3)  
head(test_set3)

# rf month 1 
X_train1 <- train_set1[, -which(names(train_set1) %in% "IR")]   
y_train1 <- train_set1$IR  
X_test1 <- test_set1[, names(X_train1)] 
X_train1 <- na.omit(X_train1)  
y_train1 <- na.omit(y_train1)
rf_model1 <- randomForest(X_train1, y_train1, importance = TRUE, ntree = 500)
y_pred1 <- predict(rf_model1, X_test1)
mse1 <- mean((y_pred1 - test_set1$IR)^2) 

# rf month 3
X_train2 <- train_set2[, -which(names(train_set2) %in% "IR")]   
y_train2 <- train_set2$IR  
X_test2 <- test_set2[, names(X_train2)] 
X_train2 <- na.omit(X_train2)  
y_train2 <- na.omit(y_train2)
rf_model2 <- randomForest(X_train2, y_train2, importance = TRUE, ntree = 500)
y_pred2 <- predict(rf_model2, X_test2)
mse2 <- mean((y_pred2 - test_set2$IR)^2)

# rf month 12
X_train3 <- train_set3[, -which(names(train_set3) %in% "IR")] 
y_train3 <- train_set3$IR  
X_test3 <- test_set3[, names(X_train3)] 
X_train3 <- na.omit(X_train3)  
y_train3 <- na.omit(y_train3)
rf_model3 <- randomForest(X_train3, y_train3, importance = TRUE, ntree = 500)
y_pred3 <- predict(rf_model3, X_test3)
mse3 <- mean((y_pred3 - test_set3$IR)^2)  
mae3 <- mean(abs(y_pred3 - test_set3$IR))  

# Print RMSE values
cat("Mean Squared Error (MSE1):", mse1, "\n")  
cat("Mean Squared Error (MSE2):", mse2, "\n")  
cat("Mean Squared Error (MSE3):", mse3, "\n")  
