#### ECON5821-Final
# 如果安装包运行不了就不运行这个
rm(list = ls())
ls()
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

# Print RMSE values
print(paste("RMSE for 1 month ahead:", rmse_1m))
print(paste("RMSE for 3 months ahead:", rmse_3m))
print(paste("RMSE for 12 months ahead:", rmse_12m))

