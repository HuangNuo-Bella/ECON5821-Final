# ECON5821-Final
#如果安装包运行不了就不运行这个
rm(list = ls())
ls()
#以下数据中data是初始数据，df0是初始数据调整格式后的数据以便于计算，df是log(y)&x，logdf是log(y)&log(x)的数据
#import a xlsx file from a URL
install.packages("openxlsx")
library(openxlsx)
url <- "https://github.com/zhentaoshi/Econ5821/raw/main/data_example/US_PCE_training.xlsx"
data <- read.xlsx(url)
head(data)
#reformat the dataframe
df0 = t(data)
colnames(df0) = df0[1,]
df0 = df0[2:nrow(df0),]
df0 = apply(df0, 2, as.numeric)
df0 = df0[, c(2,1,3:206)]
head(df0)
#check if there is NA
any(is.na(df0))
#calculate the month inflation rate
IR = df0[,2]
IR[1] = NA
for (i in 2:nrow(df0)){
    IR[i] = (log(df0[i,2]) - log(df0[i-1,2])) * 12
    }
head(IR)
#data with x variables
df = cbind(df0, "Inflation rate of month t" = IR)
df = df[, c(1,207,2,3:206)]
head(df)
dim(df)
#transform the all variables into log difference
logdf = df0
logdf[1,2:ncol(logdf)] = NA
for (i in 2:nrow(df0)){
    logdf[i,2:ncol(df0)] = (log(df0[i,2:ncol(df0)]) - log(df0[i-1,2:ncol(df0)])) * 12
    }
#data with x variables of log difference
colnames(logdf)[2] = "Inflation rate of month t"
head(logdf)
dim(logdf)
