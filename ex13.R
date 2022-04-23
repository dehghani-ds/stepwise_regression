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
dev.off()
par(mfrow=c(1,1))
boxplot(data2$Salary,main="distribution salary")

#identify outliers
# we haven't any data lower than range tukey so just higher outlier calc

tukey_ul = quantile(data2$Salary,probs = 0.75)+1.5*IQR(data2$Salary)
sum(data2$Salary>tukey_ul)
dim(data2)
#11/263=4% data are outliers

#correlation analysis

cor_table = round(cor(data2[,c(19,1:13,16:18)]),2)
#draw correlation graph 
corrplot(cor_table)

#we can see correlation between salary and most features 
#but another topic that attract our attention is two block
#of variable that have considerable correlation to each other
#its giving the vibe of multicollinearity problem

#correlation only show linear relationship between var
#but in scatter plot we can find out if there is any other
#kind of relation between response var and other var or not

#scatter plot response var with other variable

par(mfrow=c(3,6))
par(mar=c(2,2,2,2))
for(i in c(1:13,16:18)){
  plot(data2[,i],data2$Salary,main=paste(colnames(data2)[i]))
}

#categorical data
table(data2$League)
table(data2$Division)
table(data2$NewLeague)
dev.off()

#we have enough data in groups

#check influence categorical data on salary
tapply(data2$Salary,data2$League,mean)
tapply(data2$Salary,data2$Division,mean)
tapply(data2$Salary,data2$NewLeague,mean)

#we can run statistical test in this groups


#----divide data to train and test----
set.seed(1234)

train_case = sample(1:nrow(data2),size = nrow(data2)*0.8)
train = data2[train_case,]

test = data2[-train_case,]

summary(train)
summary(test)

#train data set with out outliers----
trimmed_train = train[-which(train$Salary>tukey_ul),]
dim(trimmed_train)

#model with simple linear regression----

lm_1 = lm(Salary~.,data = train)

summary(lm_1)

#p-value for F test is <0.05 so there is at least one features 
#that have linear relation with salary

#then take look at t test and know which features have good score

#only four of them :)

#don't rush to eliminate features from model lets look at 
#residual distribution and find out are they following
#normal distribution

hist(lm_1$residuals,breaks = 100,probability = T)
lines(density(lm_1$residuals),col="red")

qqnorm(lm_1$residuals)
qqline(lm_1$residuals,col="red")

jarque.test(lm_1$residuals)
anscombe.test(lm_1$residuals)

#both p-value under 0.05 and normality assumption rejected :(

plot(lm_1)
#the residual vs actual have pattern like cone
#qqplot has problem on each tale
#some data in Cooks distance on margin

#check multicollinearity 
car::vif(lm_1)
#sum features have very big vif and most of them are more than 10

#conclusion:
# severe violation of regression assumptions
# if select variables based on t-test results,
# limited number of variables will be selected. 
#but lets make a Bad model!
summary(lm_1)
lm_2 = lm(Salary~AtBat+Hits+Walks+Division+PutOuts,data = train)

summary(lm_2)

hist(lm_2$residuals,probability = T)
plot(lm_2)

car::vif(lm_2)

#lets predict with this bad model

pred_lm = predict(lm_2,test)
#calculate errors

abs_err_lm = abs(pred_lm-test$Salary)

models_comp <- data.frame("Mean of AbsErrors"   = mean(abs_err_lm),
                          "Median of AbsErrors" = median(abs_err_lm),
                          "SD of AbsErrors"  = sd(abs_err_lm),
                          "IQR of AbsErrors" = IQR(abs_err_lm),
                          "Min of AbsErrors" = min(abs_err_lm),
                          "Max of AbsErrors" = max(abs_err_lm), 
                          row.names = 'LM_t-test')

#actual vs predicted

plot(test$Salary,pred_lm,xlab = "Actual",ylab = "prediction")
abline(0,1,col="red",lwd=3)

#model not good at all

#box-cox transformation ----------

hist(data$Salary)
#because our response variable is skewed its give the hint to us
#that it may be help our modeling to box-cox transform our response var
#for better modeling
box_results = boxcox(train$Salary~.,data=train,lambda = seq(-5,5,0.1))
box_results=data.frame(box_results$x,box_results$y)
lambda=box_results[which(box_results$box_results.y==max(box_results$box_results.y)),1]
lambda
# we calculate lambda for box-cox transformation 0.1 but when we look at 
# the graph we can see that ZERO(0) is in confidence interval
# in this case we should transfer with log function

#log salary 
train$log_salary = log(train$Salary)
View(train)

#model on log salary 
lm_logresp_1 = lm(log_salary~.-Salary,data = train)

summary(lm_logresp_1)

#check assumption of regression

hist(lm_logresp_1$residuals,breaks = 100)
qqnorm(lm_logresp_1$residuals)
qqline(lm_logresp_1$residuals,col="red")

skewness(lm_logresp_1$residuals)
kurtosis(lm_logresp_1$residuals)

plot(lm_logresp_1)
car::vif(lm_logresp_1)

#model have multicollinearity problem and also a little 
# heteroscedasticity problem
# in conclusion predicting log salary can help us alittle in predicting 
#because heteroscedasticity problem a little solved

# and we are not using much of variables

#---- best subset selection ----
bestsub_1 = regsubsets(log_salary~.-Salary,nvmax = 19,data = train,method = "exhaustive")
summary(bestsub_1)

#model selection 
#r-squared
summary(bestsub_1)$rsq
#clearly r-squared isn't good tools for compare models which have 
#difference variables because model that have biggest variables always
#have bigger r-squared

#we choose adjusted r-squared first to choose best model
plot(summary(bestsub_1)$adjr2,
     type = "b",
     xlab = "# of Variables", 
     ylab = "AdjR2", 
     xaxt = 'n',
     xlim = c(1, 19)); grid()
axis(1, at = 1: 19, labels = 1: 19)

points(which.max(summary(bestsub_1)$adjr2), 
       summary(bestsub_1)$adjr2[which.max(summary(bestsub_1)$adjr2)],
       col = "red", cex = 2, pch = 20)
#plot show us that model with 11 variables is the best model

#we can also use cp criteria to choose best model
#Cp = 1/n * (SSE + 2 * d * sigma_hat ^ 2)
# as we know we like the SSE to be minimum same for cp 
plot(summary(bestsub_1)$cp)

plot(summary(bestsub_1)$cp,
     type = "b",
     xlab = "# of Variables", 
     ylab = "Cp", 
     xaxt = 'n',
     xlim = c(1, 19)); grid()
axis(1, at = 1: 19, labels = 1: 19)
points(which.min(summary(bestsub_1)$cp), 
       summary(bestsub_1)$cp[which.min(summary(bestsub_1)$cp)],
       col = "red", cex = 2, pch = 20)
#in this case model with 8 variables is the best

#or we can use BIC method
#BIC
#BIC (Bayesian Information Criterion ) =  -2 * LogLikelihood  + log(n) * d
# n: the number of samples 
# d: the number of predictors
plot(summary(bestsub_1)$bic,
     type = "b",
     xlab = "# of Variables", 
     ylab = "BIC", 
     xaxt = 'n',
     xlim = c(1, 19)); grid()
axis(1, at = 1: 19, labels = 1: 19)
points(which.min(summary(bestsub_1)$bic), 
       summary(bestsub_1)$bic[which.min(summary(bestsub_1)$bic)],
       col = "red", cex = 2, pch = 20)
# in this method 3variables model is the best but we know its not 
# acceptable for us (not enough variable)

#we choose result adjusted r-squared which offer us 11 variables
#how access to coeficient of this model
coef(bestsub_1,11)

#we can see which features selected 
#AtBat,Hits,Walks,Years,CAtBat,CHmRun,CRBI,CWalks,League,
#Division,PutOuts

bestsub_2 = lm(log_salary~AtBat+Hits+Walks+Years+CAtBat+
                 CHmRun+CRBI+CWalks+League+Division+PutOuts,data = train)
summary(bestsub_2)

plot(bestsub_2)

hist(bestsub_2$residuals)

#with best subset methodology, t-test result and multicollinearity 
#isn't important 
# t-test is not important because we are running all possible modeling
# and best model going to be choose
#also multicollinearity problem isn't matter because if we have this problem
# t-test result going under question.we don't use t-test result so
#multicollinearity problem is not important

#----test model with 11 variables, method best subset----
pred_bestsub = predict(bestsub_2,test)

#we are predicting log(salary)

pred_bestsub = exp(pred_bestsub)

#calculating error 

abs_err_bestsub = abs(pred_bestsub-test$Salary)
models_comp <- rbind(models_comp, 'BestSubset_AdjR2' = c(mean(abs_err_bestsub),
                                                         median(abs_err_bestsub),
                                                         sd(abs_err_bestsub),
                                                         IQR(abs_err_bestsub),
                                                         range(abs_err_bestsub)))
#actual vs prediction

plot(test$Salary, pred_bestsub, main = 'BestSubset_AdjR2',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#this methodology has a lot of calculating cost and not good for
#problem that have a lot of variables

#---- forward step wise regression ----

fwd_1 = regsubsets(log_salary~.-Salary,nvmax = 19,data = train,method = "forward")

which.max(summary(fwd_1)$adjr2)#12
which.min(summary(fwd_1)$cp)#8
which.min(summary(fwd_1)$bic)#3

#in forward selection we are using features from last step and add one 
#more features 
#its reduce calculating cost a lot
#----?----
coef(fwd_1,12)
coef(fwd_1,8)
coef(fwd_1,3)
#backward 

#reverse forward selection
bwd_1 <- regsubsets(log_salary ~ . - Salary, nvmax = 19, data = train, method = "backward")
summary(bwd_1)

which.max(summary(bwd_1)$adjr2)
which.min(summary(bwd_1)$cp)
which.min(summary(bwd_1)$bic)

#the important issue in this lecture we should know is that features 
#in features selection method may not be same as each other

#Inspect different models (Do it yourself)؟؟؟؟؟؟-----

#using k-fold cross validation instead of :adjusted r squared \ CP \BIC
#model 3: k-fold cross validation ----
k=10
set.seed(123)
folds = sample(1:10,nrow(train),rep=T)

#here we produce label for each row that show in which fold it should 
#be putted

#make error matrix that we can compare and say which model is better
cv_errors <- matrix(NA, k, 19, dimnames = list(NULL , paste(1: 19)))
cv_errors
#run 19 regression

#Prediction function
#using design matrix 
#---- Important----
predict_regsubsets = function(object, newdata, id) {
  reg_formula <- as.formula(object$call[[2]])
  mat    <- model.matrix(reg_formula, newdata)
  coef_i <- coef(object, id = id)
  mat[, names(coef_i)] %*% coef_i
}

set.seed(1234)
for(i in 1: 19){
  for(j in 1: k){
    best_fit <- regsubsets(log_salary ~ . - Salary, data = train[folds != j,], nvmax = 19, method = "exhaustive")
    pred <- predict_regsubsets(best_fit, newdata = train[folds == j,], id = i)
    cv_errors[j, i] <- mean((train$log_salary[folds == j] - pred) ^ 2)
  }
}

View(cv_errors)
mean_cv_erros <- apply(cv_errors, 2, mean)
mean_cv_erros 
plot(mean_cv_erros, type = "b")
which.min(mean_cv_erros)#8

#Coefficients of the best model
coef(bestsub_1, 8) #Model w/ 8 variables
bestsub_cv <- lm(log_salary ~ AtBat + Hits + HmRun + Walks + 
                   Years + CAtBat + League + Division, data = train)
summary(bestsub_cv)

#Test the Model----------------------------------
#Model: bestsub_cv
#Prediction
pred_bestsub_cv <- predict(bestsub_cv, test)
pred_bestsub_cv <- exp(pred_bestsub_cv)

#Absolute error mean, median, sd, max, min-------
abs_err_bestsub_cv <- abs(pred_bestsub_cv - test$Salary)
models_comp <- rbind(models_comp, 'BestSubset_CV' = c(mean(abs_err_bestsub_cv),
                                                      median(abs_err_bestsub_cv),
                                                      sd(abs_err_bestsub_cv),
                                                      IQR(abs_err_bestsub_cv),
                                                      range(abs_err_bestsub_cv)))
View(models_comp)
#Actual vs. Predicted
plot(test$Salary, pred_bestsub_cv, main = 'BestSubset_CV',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#Model 4: Best Sub Selection Using Trimmed Train and CV--------------
#Add Log Salary
trimmed_train$Log_Salary <- log(trimmed_train$Salary)
trimmed_bestsub_1 <- regsubsets(Log_Salary ~ . - Salary, nvmax = 19, data = trimmed_train, method = "exhaustive")

which.max(summary(trimmed_bestsub_1)$adjr2)
which.min(summary(trimmed_bestsub_1)$cp)
which.min(summary(trimmed_bestsub_1)$bic)

#Coefficients of the best model
coef(trimmed_bestsub_1, 10) #Model w/ 10 variables
trimmed_bestsub_2 <- lm(Log_Salary ~ AtBat + Hits + HmRun + Walks + 
                          Years + CAtBat + CHmRun + CRBI + CWalks + 
                          League, data = trimmed_train)
summary(trimmed_bestsub_2)
#Test the Model----------------------------------
#Prediction
pred_trimmed_bestsub  <- predict(trimmed_bestsub_2, test)
pred_trimmed_bestsub  <- exp(pred_trimmed_bestsub)
pred_trimmed_bestsub
#Absolute error mean, median, sd, max, min-------
abs_err_trimmed_bestsub <- abs(pred_trimmed_bestsub - test$Salary)
models_comp <- rbind(models_comp, 'BestSubset_Trimmed' = c(mean(abs_err_trimmed_bestsub),
                                                           median(abs_err_trimmed_bestsub),
                                                           sd(abs_err_trimmed_bestsub),
                                                           IQR(abs_err_trimmed_bestsub),
                                                           range(abs_err_trimmed_bestsub)))
View(models_comp)

#Actual vs. Predicted
plot(test$Salary, pred_trimmed_bestsub, main = 'BestSubset_Trimmed',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#Save the results--------------------------------
save(data2, train, test, models_comp, file = "case4_dataset_v1.R")
###End of Code###--------------------------------








































