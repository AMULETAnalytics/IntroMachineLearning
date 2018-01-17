# File-Name:       Chapter_7.R           
# Date:            2015-10-10                                
# Author:          Daniel D. Gutierrez (daniel@amuletanalytics.com)
# Purpose:         Machine Learning and Data Science: code for Chapter 7 - Evaluating Model Performance

# All source code is copyright (c) 2015, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# -------------------------------------------------------
# Overfitting
# -------------------------------------------------------

install.packages("kernlab")
library(kernlab)

data(spam)  # 4601 observations x 58 variables
head(spam)

# Take a small sample for this example, 25 observations
set.seed(333)
sampleIndex <- sample(dim(spam)[1], size=10)
sampleSpam <- spam[sampleIndex,]

# Create logical vector and coerce to numeric
spamSymbol <- (sampleSpam$type=="spam") + 1

# Symbol circle: pch=1 (nonspam), symbol triangle: pch=2 (spam)
# plot(sampleSpam$all, pch=spamSymbol)
plot(sampleSpam$capitalAve, pch=spamSymbol)
legend('topright', legend=c("nonspam", "spam"), pch=c(1,2))

# Print capitalAve values for sample
sampleSpam$capitalAve
#[1]  1.000 11.320  1.000  1.840  7.300  1.635  2.666  3.545  5.163  2.444

# Overfitted prediction algorithm 

alg1 <- function(x){
  pred <- rep(NA, length(x))
  
  pred[x>2.7] <- "spam"
  pred[x<2.4] <- "nonspam"
  # Additional rules result in overfitting
  pred[x<=2.45 & x>=2.4] <- "spam"
  pred[x<=2.7 & x>2.45] <- "nonspam"
  
  return(pred)
}

# Confusion matrix
table(alg1(sampleSpam$capitalAve),sampleSpam$type)
#        nonspam spam
#nonspam       5    0
#spam          0    5

# Another algorihm less closely to sample set

alg2 <- function(x){
  pred <- rep(NA, length(x))
  
  pred[x>2.8] <- "spam"
  pred[x<=2.8] <- "nonspam"
  
  
  return(pred)
}

# Confusion matrix
table(alg2(sampleSpam$capitalAve),sampleSpam$type)
#        nonspam spam
#nonspam       5    1
#spam          0    4

# Apply the overfit model to the complete data set

table(alg1(spam$capitalAve), spam$type)
#        nonspam spam
#nonspam    2141  588
#spam        647 1225

# Overfit algorithm errors > simpler algorithm for full spam dataset
sum(alg1(spam$capitalAve)!=spam$type)  # Number of errors
[1] 1235

# Apply the second model to the complete data set

table(alg2(spam$capitalAve), spam$type)
#        nonspam spam
#nonspam    2224  642
#spam        564 1171

# Non-overfit algorithm has fewer errors than overfit version
sum(alg2(spam$capitalAve)!=spam$type)  # Number of errors
[1] 1206


# -------------------------------------------------------
# Measuring Regression Performance
# -------------------------------------------------------

library("car")

data(Prestige)  # 102x6

# Calculate RMSE

rmse <- function(y_hat, y)
{
  return(sqrt(mean((y_hat-y)^2)))
}

Prestige_noNA <- na.omit(Prestige)

n <- nrow(Prestige_noNA)  # Number of observations = 102
ntrain <- round(n*0.7)    # 70% for training set
set.seed(333)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

prestige_train <- Prestige_noNA[tindex,]  # Create training set
prestige_test <- Prestige_noNA[-tindex,]  # Create test set

# Fitted model in lm1
lm1 <- lm(prestige~., data=prestige_train)

rmse_train <- rmse(predict(lm1),prestige_train$prestige)
rmse_train

rmse_test <- rmse(predict(lm1, newdata=prestige_test), prestige_test$prestige)
rmse_test


# Calculate R2
rsquared <- function(y_hat, y){
  mu <- mean(y)
  rse <- mean((y_hat - y)^2) / mean((mu - y)^2)
  rsquared <- (1 - rse) * 100
  return(rsquared)
}


# Calculate training set error metrics

y_hat <- lm1$fitted.values # y-hat
y <- prestige_train$prestige  # y
rsquared(y_hat, y)
#[1] 85.09975

rmse_train <- (mean((y_hat - y)^2))^0.5
rmse_train
#[1] 6.46309

mu <- mean(y)
rse <- mean((y_hat - y)^2) / mean((mu - y)^2)
rse
#[1] 0.1490025

rsquared <- 1 - rse
rsquared
#[1] 0.8509975

# Calculate test set error metris

y_hat <- predict(lm1, newdata=prestige_test) # y-hat
y <- prestige_test$prestige  # y
rsquared(y_hat, y)
#[1] 80.71466

rmse_test <- (mean((y_hat - y)^2))^0.5
rmse_test
#[1] 7.705871

mu <- mean(y)
rse <- mean((y_hat - y)^2) / mean((mu - y)^2)
rse
#[1] 0.1928534

rsquared <- 1 - rse
rsquared
#[1] 0.8071466


# -------------------------------------------------------
# Measuring Classification Performance
# -------------------------------------------------------

# Simple example using simulated data set

y_hat <- sample(0:2,50,replace=TRUE) # Predicted values from model
y <- sample(0:2,50,replace=TRUE)   # Actual values from data set

cm <- table(y_hat,y)  # Show confusion matrix
cm
#       y
#y_hat  0  1  2
#    0  6  4  4
#    1  4 11  5
#    2  8  4  4

misclassification_error_rate <- 1-sum(diag(cm))/sum(cm)
misclassification_error_rate
#[1] 0.58


# -----------------------------------------

library(randomForest)

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", "wine.csv")

df <- read.csv("wine.csv", sep=";", header=TRUE)
df$quality <- factor(df$quality)

n <- nrow(df)  # Number of observations = 102
ntrain <- round(n*0.7)    # 70% for training set
set.seed(333)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

wine_train <- df[tindex,]  # Create training set
wine_test <- df[-tindex,] 

rf <- randomForest(quality ~ ., data=wine_train, ntree=20, nodesize=5, mtry=9)

table(wine_test$quality, predict(rf, wine_test))
#    3   4   5   6   7   8
#3   0   1   2   2   0   0
#4   0   2  11   8   1   0
#5   0   2 159  35   7   0
#6   0   0  45 114  24   1
#7   0   0   5  20  34   1
#8   0   0   0   2   4   0

sum(wine_test$quality!=predict(rf, wine_test)) / nrow(wine_test)
#[1] 0.3604167

rf_pred_prob <- predict(rf, newdata=wine_test, type="prob")
rf_pred_prob

pred = prediction(rf_pred_prob, wine_test$quality) 


# -------------------------------------------------------
# Cross Validation
# -------------------------------------------------------

install.packages("ipred")
library(ipred)

set.seed(314)

# random forest algorithm
library(randomForest)
cv_error <- errorest(Species~., data=iris, model=randomForest)
cv_error$error   # class=cvclass
[1] 0.04666667

# Naive Bayes algorithm
library(e1071)
predict_nb <- function(object, newdata) {
  predict(object, newdata[,-1])
}

cv_error <- errorest(Species~., data=iris, model=naiveBayes, predict=predict_nb)
cv_error$error
#[1] 0.04666667

# k-nearest neighbors
library(class)
predict_knn <- function(object, newdata){
  predict.ipredknn(object, newdata, type="class")
}

cv_error <- errorest(Species~., data=iris, model=ipredknn, predict=predict_knn)
cv_error$error
#[1] 0.03333333

# Support vector machines
library(e1071)

cv_error <- errorest(Species~., data=iris, model=svm)
cv_error$error
#[1] 0.03333333


# LDA
library(MASS)
predict_lda <- function(object, newdata){
  predict(object, newdata)$class
}

cv_error <- errorest(Species~., data=iris, model=lda, predict=predict_lda)
cv_error$error
#[1] 0.02

set.seed(314)
cv_result <- replicate(25, errorest(Species~., data=iris, model=lda, predict=predict_lda)$error)
cv_result
sd(cv_result)
#[1] 0

# Confusion matrix
pred_species <- errorest(Species~., data=iris, model=lda, predict=predict_lda, est.para=control.errorest(predictions=TRUE))$predictions

table(iris$Species, pred_species)
#         pred_species
#           setosa versicolor virginica
#setosa         50          0         0
#versicolor      0         48         2
#virginica       0          1        49














































