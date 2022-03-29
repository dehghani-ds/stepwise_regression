setwd("---/13")
library("moments")  #Moments, skewness, kurtosis and related tests 
library("MASS")     #Box-Cox Transformations for Linear Models
library("leaps")    #Regression Subset Selection
library("corrplot")
#Book: computer age statistical significance----
#why and how new algorithm born and explain algorithm
#show when came to problem choose correct solution

#step wise regression----

data = read.csv("CS_04.csv",header = T)
head(data)
str(data)
#data column that should change to factor:
#League,Division and NewLeague

col2change = c("League","Division","NewLeague")

data[,col2change] = lapply(data[,col2change], factor)

str(data)

summary(data)
#AtBat: Number of times at bat in 1986
#Hits:  Number of hits in 1986
#HmRun: Number of home runs in 1986
#Runs:  Number of runs in 1986
#RBI:   Number of runs batted in in 1986
#Walks: Number of walks in 1986
#Years: Number of years in the major leagues
#CAtBat: Number of times at bat during his career
#CHits:  Number of hits during his career
#CHmRun: Number of home runs during his career
#CRuns:  Number of runs during his career
#CRBI:   Number of runs batted in during his career
#CWalks: Number of walks during his career
#League: A factor with levels A and N indicating player's league at the end of 1986
#Division: A factor with levels E and W indicating player's division at the end of 1986
#PutOuts:  Number of put outs in 1986
#Assists:  Number of assists in 1986
#Errors:   Number of errors in 1986
#Salary:   1987 annual salary on opening day in thousands of dollars
#NewLeague: A factor with levels A and N indicating player's league at the beginning of 1987


sum(is.na(data))

#there is 59 missing value in data and at first we should 
#deal with them.before any activity
#all MV(missing values) are in salary column and its bad news
#that we have this much MV in response variable

#role of thumb for number of data is 30 data per features
#and if we want to choose complete data for modeling we have
#big problem. we cant use other method for imputation because
#the MV are in response variable , the column that we want to predict

#but we should analysis the MV's and find out if there is any
#pattern in them


#analysis pattern in MV ----


#remove record im MV ----

which(is.na(data$Salary))
data2 = data[-which(is.na(data$Salary)),]
summary(data2)

#remove identifire from data set

data2=data2[,-1]

head(data2)
dim(data2)#low in sample size and not ideal
summary(data2)
sum(is.na(data2))

#understanding data ----
dev.new()
par(mar = c(2, 2, 2, 2))
par(mfrow = c(3, 6))  # 3 rows and 6 columns
for (i in c(1:13,16:19)) {
  hist(data2[,i],probability = T,main = paste("hist of",colnames(data2)[i]))
}

#13-1 47:12













