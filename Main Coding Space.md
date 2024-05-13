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



### training model with 3 horizons
library(dplyr)
IR[1] <- 0
df <- cbind(df0, IR)
df <- df[, c(1, 207, 2, 3:206)]
df <- as.data.frame(df)
df <- df[, -3]
data <- df[, top_25_features] #Decision tree
#data <- df[, top_feature_names[1:25]] if use the PCA
data <- cbind(data, IR)

# Using the first 731 rows for 1 month ahead
data_1m <- data[1:731, c(top_25_features, "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m <- data[1:729, c(top_25_features, "IR")]  
# Using the first 720 rows for 12 months ahead
data_12m <- data[1:720, c(top_25_features, "IR")] 





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
data_1m <- data[1:731, c(top_25_features, "IR")] 
# Using the first 729 rows for 3 months ahead
data_3m <- data[1:729, c(top_25_features, "IR")]
# Using the first 720 rows for 12 months ahead
data_12m <- data[1:720, c(top_25_features, "IR")]

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

rmse_1m <- fit_lasso_and_calculate_rmse(data_1m, 1)
rmse_3m <- fit_lasso_and_calculate_rmse(data_3m, 3)
rmse_12m <- fit_lasso_and_calculate_rmse(data_12m, 12)

# Print RMSE values
print(paste("RMSE for 1 month ahead:", rmse_1m))
print(paste("RMSE for 3 months ahead:", rmse_3m))
print(paste("RMSE for 12 months ahead:", rmse_12m))

## Use features with PCA
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
  rmse <- sqrt(mean((LASSO_predict - y)^2))
  return(rmse)
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


#ARMA model
#Stationary test
adf.test(ts_df, alternative = "stationary")

#Pure random test
Box.test(ts_df, lag = 6)
Box.test(ts_df, lag = 12)

#ARMA model
library(forecast)
arma_model <- auto.arima(ts_df, trace=TRUE)
print(arma_model)

#ARMA model function
ARMAmodel <- function(n,data){
    data <- as.data.frame(data)
    selected_df <- as.matrix(data[2:(nrow(data)-n),])
    ts_df <- ts(selected_df)
    #ARMA model
    arma_model <- arima(ts_df, order = c(0,1,3))
    print(arma_model)
    #prediction
    pre_IR <- predict(arma_model, n.ahead = n)
    #MSE
    arma_mse <- mean((data[(nrow(data)-n+1):nrow(data), ]-pre_IR$pred)^2)
    return(arma_mse)
}

#forcast for y(t+1)
n1 <- 1
print(ARMAmodel(n1,IR))
#forcast for y(t+3)
n3 <- 3
print(ARMAmodel(n3,IR))
#forcast for y(t+12)
n12 <- 12
print(ARMAmodel(n12,IR))
