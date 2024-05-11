#### ECON5821-Final
# 如果安装包运行不了就不运行这个
rm(list = ls())
ls()
### 数据处理
#以下数据中data是初始数据，df0是初始数据调整格式后的数据以便于计算，df是log(y)&x，logdf是log(y)&log(x)的数据
# import a xlsx file from a URL
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
IR[1] = NA
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
logdf[1,2:ncol(logdf)] = NA
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

library(rpart)
library(stats)

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





### Run the LASSO Part
library(glmnet)

# Use PCA
selected_predictors <- predictors[, top_feature_names[1:25]]

# Run LASSO regression
lasso_model <- cv.glmnet(as.matrix(selected_predictors), target, alpha = 1)

# Plot cross-validated mean squared error (MSE) versus lambda
plot(lasso_model)

# Get the optimal lambda value
optimal_lambda <- lasso_model$lambda.min

# Refit with optimal lambda
lasso_model_final <- glmnet(as.matrix(selected_predictors), target, alpha = 1)
lasso_model_predict <- predict(lasso_model_final, s = optimal_lambda, type = "coefficients")[, 1]

# Print coefficients of the final LASSO model
print(lasso_model_predict)

# Calculate Mean Squared Error (MSE) on the training set
mse <- mean((lasso_model_predict - target)^2)

# Print MSE
print(paste("Final MSE on training set:", mse))

# Plot cross-validated mean squared error (MSE) versus lambda
plot(lasso_model_final)

# Plot the coefficients versus log(lambda)
plot(lasso_model_final, xvar = "lambda", label = TRUE)




# Use Decision tree
selected_predictors <- predictors[, top_25_features]

# Run LASSO regression
lasso_model <- cv.glmnet(as.matrix(selected_predictors), target, alpha = 1)

# Plot cross-validated mean squared error (MSE) versus lambda
plot(lasso_model)

# Get the optimal lambda value
optimal_lambda <- lasso_model$lambda.min

# Refit with optimal lambda
lasso_model_final <- glmnet(as.matrix(selected_predictors), target, alpha = 1)
lasso_model_predict <- predict(lasso_model_final, s = optimal_lambda, type = "coefficients")[, 1]

# Print coefficients of the final LASSO model
print(lasso_model_predict)

# Calculate Mean Squared Error (MSE) on the training set
mse <- mean((lasso_model_predict - target)^2)

# Print MSE
print(paste("Final MSE on training set:", mse))

# Plot cross-validated mean squared error (MSE) versus lambda
plot(lasso_model_final)

# Plot the coefficients versus log(lambda)
plot(lasso_model_final, xvar = "lambda", label = TRUE)
