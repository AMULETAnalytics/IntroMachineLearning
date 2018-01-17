# File-Name:       Chapter_6.R           
# Date:            2015-10-10                                
# Author:          Daniel D. Gutierrez (daniel@amuletanalytics.com)
# Purpose:         Machine Learning and Data Science: code for Chapter 6 - Classification

# All source code is copyright (c) 2015, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.


# -------------------------------------------------------------
# A simple example of classification
# -------------------------------------------------------------

install.packages("kernlab")
library(kernlab)
data(spam)
head(spam)

# Plot # times a specific frequency value "$" appears in email
plot(density(spam$charDollar[spam$type=="nonspam"]), lwd=0.5, main="", xlab="Frequency of '$' in E-mail")
lines(density(spam$charDollar[spam$type=="spam"]), lwd=3)

# > 0.125 -> spam, <= 0.125 -> non-spam
abline(v=0.125, col="black")
legend(1.5, 20,legend=c("Spam","Nonspam"), lwd=c(3,0.5), lty = 1)

# Classification
spam_classifier <- ifelse(spam$charDollar > 0.125, "spam", "nonspam")
table(spam_classifier, spam$type)/nrow(spam)
#spam_classifier   nonspam      spam
#nonspam         0.5911758 0.2279939
#spam            0.0147794 0.1660509

# -------------------------------------------------------------
# Logistic Regression
# -------------------------------------------------------------

data(iris)

n <- nrow(iris)  # Number of observations

ntrain <- round(n*0.6)  # 60% for training set

set.seed(333)    # Set seed for reproducible results

tindex <- sample(n, ntrain)   # Create an index

train_iris <- iris[tindex,]   # Create training set
test_iris <- iris[-tindex,]   # Create test set

newcol <- data.frame(isVersicolor=(train_iris$Species=="versicolor"))
train_iris <- cbind(train_iris, newcol)
head(train_iris)
#Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#71           5.9         3.2          4.8         1.8 versicolor
#13           4.8         3.0          1.4         0.1     setosa
#145          6.7         3.3          5.7         2.5  virginica
#84           6.0         2.7          5.1         1.6 versicolor
#3            4.7         3.2          1.3         0.2     setosa
#105          6.5         3.0          5.8         2.2  virginica
#isVersicolor
#71          TRUE
#13         FALSE
#145        FALSE
#84          TRUE
#3          FALSE
#105        FALSE


# Single predictor -----------------------------------------
glm1 <- glm(isVersicolor ~ Sepal.Width, data=train_iris, family=binomial)
glm1

summary(glm1)
#Call:
#  glm(formula = isVersicolor ~ Sepal.Width, family = binomial, 
#      data = train_iris)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.9933  -0.8609  -0.4757   0.9359   2.1143  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   9.1013     2.5534   3.564 0.000365 ***
#  Sepal.Width  -3.3010     0.8656  -3.813 0.000137 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 113.136  on 89  degrees of freedom
#Residual deviance:  90.326  on 88  degrees of freedom
#AIC: 94.326
#
#Number of Fisher Scoring iterations: 5

plot(train_iris$Sepal.Width, train_iris$isVersicolor)
curve(predict(glm1, data.frame(Sepal.Width=x), type="response"), add=TRUE) 

# Predict single new observation
newdata <- data.frame(Sepal.Width=2.4)
predict(glm1, newdata, type="response") 
#1 
#0.7647645 


# Multiple predictors ------------------------------------
formula <- isVersicolor ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

glm2 <- glm(formula, data=train_iris, family="binomial")

prob <- predict(glm2, newdata=test_iris, type="response")

round(prob,3)

#1     5     6     8     9    11    14    15    19    20    21 
#0.077 0.054 0.021 0.105 0.295 0.051 0.223 0.020 0.040 0.027 0.138 
#23    29    31    36    39    41    45    46    49    51    57 
#0.036 0.108 0.230 0.158 0.222 0.059 0.029 0.236 0.048 0.322 0.145 
#58    60    64    65    67    68    69    73    74    75    78 
#0.670 0.382 0.447 0.282 0.253 0.657 0.862 0.757 0.616 0.470 0.355 
#79    81    82    85    94    96    97    99   106   107   109 
#0.364 0.755 0.776 0.234 0.748 0.337 0.371 0.531 0.521 0.461 0.791 
#111   114   116   119   125   127   129   131   133   134   137 
#0.138 0.501 0.092 0.779 0.133 0.380 0.389 0.668 0.347 0.568 0.047 
#141   144   147   149   150 
#0.139 0.149 0.618 0.048 0.247

summary(glm2)
#Call:
#  glm(formula = formula, family = "binomial", data = train_iris)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0732  -0.7529  -0.4250   0.9386   2.2185  
#
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
#(Intercept)    5.9490     3.3423   1.780  0.07509 . 
#Sepal.Length   0.4966     0.8340   0.595  0.55156   
#Sepal.Width   -3.2680     1.0456  -3.125  0.00178 **
#Petal.Length   0.5930     0.7837   0.757  0.44920   
#Petal.Width   -1.7861     1.3396  -1.333  0.18241   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 113.136  on 89  degrees of freedom
#Residual deviance:  87.066  on 85  degrees of freedom
#AIC: 97.066
#
#Number of Fisher Scoring iterations: 5

coef(glm2)
#(Intercept) Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#5.9489966    0.4966006   -3.2679800    0.5930357   -1.7861451 

summary(glm2)$coef
#               Estimate Std. Error    z value    Pr(>|z|)
#(Intercept)   5.9489966  3.3422812  1.7799211 0.075088880
#Sepal.Length  0.4966006  0.8340266  0.5954254 0.551559180
#Sepal.Width  -3.2679800  1.0456208 -3.1253970 0.001775653
#Petal.Length  0.5930357  0.7836647  0.7567467 0.449201602
#Petal.Width  -1.7861451  1.3395827 -1.3333593 0.182413915

prob <- predict(glm2, newdata=test_iris, type="response")
round(prob,3)


# -------------------------------------------------------------
# Classification Trees
# -------------------------------------------------------------

install.packages("tree")
library(tree)

# Do some EDA on the training set
str(train_iris)
#'data.frame':  90 obs. of  5 variables:
#  $ Sepal.Length: num  5.9 4.8 6.7 6 4.7 6.5 6.3 5 4.9 5.8 ...
#$ Sepal.Width : num  3.2 3 3.3 2.7 3.2 3 2.3 3.5 3.1 2.7 ...
#$ Petal.Length: num  4.8 1.4 5.7 5.1 1.3 5.8 4.4 1.6 1.5 5.1 ...
#$ Petal.Width : num  1.8 0.1 2.5 1.6 0.2 2.2 1.3 0.6 0.1 1.9 ...
#$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 2 1 3 2 1 3 2 1 1 3 ...


# Train the decision tree
ct1 <- tree(Species~., data=train_iris)

# Plot the tree with labels
plot(ct1)
# Labels the current plot of the tree dendrogram with text
text(ct1)

# Display the classification tree object
ct1

#node), split, n, deviance, yval, (yprob)
#* denotes terminal node
#
#1) root 90 197.700 virginica ( 0.33333 0.32222 0.34444 )  
#2) Petal.Length < 2.7 30   0.000 setosa ( 1.00000 0.00000 0.00000 ) *
#  3) Petal.Length > 2.7 60  83.110 virginica ( 0.00000 0.48333 0.51667 )  
#6) Petal.Length < 4.85 28   8.628 versicolor ( 0.00000 0.96429 0.03571 )  
#12) Petal.Length < 4.55 21   0.000 versicolor ( 0.00000 1.00000 0.00000 ) *
#  13) Petal.Length > 4.55 7   5.742 versicolor ( 0.00000 0.85714 0.14286 ) *
#  7) Petal.Length > 4.85 32  14.960 virginica ( 0.00000 0.06250 0.93750 )  
#14) Petal.Width < 1.7 5   6.730 virginica ( 0.00000 0.40000 0.60000 ) *
#  15) Petal.Width > 1.7 27   0.000 virginica ( 0.00000 0.00000 1.00000 ) *


# Display summary metrics for the classification tree object
summary(ct1)
#Classification tree:
# tree(formula = Species ~ ., data = train_iris)
#Variables actually used in tree construction:
#  [1] "Petal.Length" "Petal.Width" 
#Number of terminal nodes:  5 
#Residual mean deviance:  0.1467 = 12.47 / 85 
#Misclassification error rate: 0.03333 = 3 / 90 

# Calculate vector of predicted responses from a fitted tree object.
prediction <- predict(ct1, newdata=test_iris, type='class')
prediction
#[1] setosa     setosa     setosa     setosa     setosa     setosa    
#[7] setosa     setosa     setosa     setosa     setosa     setosa    
#[13] setosa     setosa     setosa     setosa     setosa     setosa    
#[19] setosa     setosa     versicolor versicolor versicolor versicolor
#[25] versicolor versicolor versicolor versicolor versicolor virginica 
#[31] versicolor versicolor virginica  versicolor versicolor versicolor
#[37] versicolor versicolor versicolor versicolor versicolor virginica 
#[43] versicolor virginica  virginica  virginica  virginica  virginica 
#[49] virginica  versicolor virginica  virginica  virginica  virginica 
#[55] virginica  virginica  virginica  virginica  virginica  virginica 
#Levels: setosa versicolor virginica

#Use contingency table to see how accurate tree is
table(prediction, test_iris$Species)

#prediction   setosa versicolor virginica
#  setosa         20          0         0
#  versicolor      0         19         2
#  virginica       0          2        17

(20+19+17)/60
#[1] 0.9333333


# -------------------------------------------------------------
# Naive Bayes
# -------------------------------------------------------------

install.packages("e1071")
library(e1071)

nb1 <-naiveBayes(Species~., data=train_iris) 
nb1

#Naive Bayes Classifier for Discrete Predictors
#
#Call:
#  naiveBayes.default(x = X, y = Y, laplace = laplace)
#
#A-priori probabilities:
#Y
#   setosa versicolor  virginica 
#0.3333333  0.3222222  0.3444444 
#
#Conditional probabilities:
#            Sepal.Length
#Y                [,1]      [,2]
#  setosa     4.983333 0.3141308
#  versicolor 6.024138 0.4740819
#  virginica  6.654839 0.6297294
#
#            Sepal.Width
#Y                [,1]      [,2]
#  setosa     3.403333 0.4089375
#  versicolor 2.779310 0.3244548
#  virginica  3.009677 0.3279785
#
#            Petal.Length
#Y                [,1]      [,2]
#  setosa     1.480000 0.1349329
#  versicolor 4.306897 0.3890749
#  virginica  5.577419 0.5321088
#
#            Petal.Width
#Y                 [,1]      [,2]
#  setosa     0.2533333 0.1224276
#  versicolor 1.3379310 0.1859604
#  virginica  2.0129032 0.2883770


prediction <- predict(nb1, test_iris[,-5])
prediction

# Confusion matrix shows TP-true positive, FN-fales negative, 
# FP-false positive, and TN-true negative
xtab <- table(prediction, test_iris$Species)
xtab
#prediction   setosa versicolor virginica
#  setosa         20          0         0
#  versicolor      0         20         2
#  virginica       0          1        17

nb1$apriori
#Y
#    setosa versicolor  virginica 
#        30         29         31 

nb1$tables$Petal.Length
#            Petal.Length
#Y                [,1]      [,2]
#  setosa     1.480000 0.1349329
#  versicolor 4.306897 0.3890749
#  virginica  5.577419 0.5321088

# Using values from above in dnorm() density for normal distribution
plot(function(x) dnorm(x, 1.48, 0.1349329), 0, 8, lty=1, main="Petal length distribution by species")
curve(dnorm(x, 4.306897, 0.3890749), add=TRUE, lty=2)
curve(dnorm(x, 5.577419, 0.5321088 ), add=TRUE, lty=5)
legend('topright', legend=c("setosa", "versicolor", "verginica"), lty=c(1,2,5), bty='o')


# -------------------------------------------------------------
# K-nearest Neighbors
# -------------------------------------------------------------

# Plot 2-dim to demonstrate KNN
plot(train_iris$Petal.Length, train_iris$Petal.Width, pch=c(train_iris$Species))
legend('topleft', legend=c("setosa", "versicolor", "verginica"), pch=c(1,2,3), bty='o')

install.packages("class")
library(class)
train_x <- train_iris[,-5]
train_y <- train_iris[,5]
test_x <- test_iris[,-5]
test_y <- test_iris[,5]
prediction <- knn(train_x, test_x, train_y, k=5)

xtab <- table(prediction, test_iris$Species)
xtab
#prediction   setosa versicolor virginica
#  setosa         20          0         0
#  versicolor      0         21         1
#  virginica       0          0        18

(20+21+18)/nrow(test_iris)
#[1] 0.9833333

sum(prediction != test_y)
#[1] 1

length(test_y)
#[1] 60


# -------------------------------------------------------------
# Support Vector Machines
# -------------------------------------------------------------

library(e1071)

svm1 <- svm(Species~., data=train_iris, type="C-classification", kernal="radial", gamma=0.1, cost=10)
summary(svm1)

#Call:
#  svm(formula = Species ~ ., data = train_iris, type = "C-classification", 
#      kernal = "radial", gamma = 0.1, cost = 10)
#
#
#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  10 
#gamma:  0.1 
#
#Number of Support Vectors:  22
#
#( 10 3 9 )
#
#
#Number of Classes:  3 
#
#Levels: 
#  setosa versicolor virginica

svm1$SV   #Show support vectors
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
#71   0.006532661  0.30579724    0.5624153   0.7747208
#84   0.124120557 -0.84094241    0.7315157   0.5125972
#88   0.476884245 -1.75833413    0.3369481   0.1194119
#86   0.124120557  0.76449310    0.3933149   0.5125972
#53   1.182411621  0.07644931    0.6187821   0.3815354
# ...

plot(svm1, train_iris, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))

prediction <- predict(svm1, test_iris)

xtab <- table(test_iris$Species, prediction)
xtab
#          prediction
#           setosa versicolor virginica
#setosa         20          0         0
#versicolor      0         20         1
#virginica       0          0        19

(20+20+19)/nrow(test_iris)
#[1] 0.9833333

sum(prediction != test_y)  # Number of misclassifications
#[1] 1


# -------------------------------------------------------------
# Neural Nets
# -------------------------------------------------------------

install.packages("neuralnet")
library(neuralnet)

nn1_iristrain <- train_iris

# Binarize categorical output
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == "setosa")
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == "versicolor")
nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species == "virginica")

names(nn1_iristrain)[6] <- "setosa"
names(nn1_iristrain)[7] <- "versicolor"
names(nn1_iristrain)[8] <- "virginica"

head(nn1_iristrain[,5:8])
#       Species setosa versicolor virginica
#71  versicolor  FALSE       TRUE     FALSE
#13      setosa   TRUE      FALSE     FALSE
#145  virginica  FALSE      FALSE      TRUE
#84  versicolor  FALSE       TRUE     FALSE
#3       setosa   TRUE      FALSE     FALSE
#105  virginica  FALSE      FALSE      TRUE

# Train the neural network model
nn1 <- neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=nn1_iristrain, hidden=c(4))

print(nn1)
#Call: neuralnet(formula = setosa + versicolor + virginica ~ Sepal.Length +     Sepal.Width + Petal.Length + Petal.Width, data = nn1_iristrain,     hidden = c(4))
#
#1 repetition was calculated.
#
#Error Reached Threshold Steps
#1 0.9297848198    0.009291227594 20533

nn1$net.result
nn1$weights
nn1$generalized.weights
nn1$result.matrix
nn1$startweights
nn1$covariate
nn1$response
nn1$data

# Plot the neural network object
plot(nn1)

# Use neural network to make classifications
prediction <- compute(nn1, test_iris[-5])
prediction <- prediction$net.result

pred_idx <- function(x) {return(which(x==max(x)))}

# Apply over rows of prediction matrix using function maxidx
idx <- apply(prediction, c(1), pred_idx)

prediction_nn <- c('setosa', 'versicolor', 'virginica')[idx]

xtab <- table(prediction_nn, test_iris$Species)
xtab
#prediction_nn setosa versicolor virginica
#   setosa         20          0         0
#   versicolor      0         18         2
#   virginica       0          3        17


# -------------------------------------------------------------
# Random Forests
# -------------------------------------------------------------

install.packages("randomForest")
library(randomForest)

rf <- randomForest(Species~., data=train_iris, ntree=500, mtry=2, importance=TRUE)

prediction <- predict(rf, newdata=test_iris, type="class")

table(prediction, test_iris$Species)
#prediction   setosa versicolor virginica
#  setosa         20          0         0
#  versicolor      0         20         2
#  virginica       0          1        17

importance(rf)
#                 setosa   versicolor   virginica MeanDecreaseAccuracy
#Sepal.Length 0.04522895  0.021642010 0.071957664          0.046288212
#Sepal.Width  0.00523258 -0.003301299 0.006098693          0.002920681
#Petal.Length 0.31637557  0.290288009 0.354026892          0.316221928
#Petal.Width  0.30126501  0.249734399 0.286229751          0.275835179
#             MeanDecreaseGini
#Sepal.Length         6.852732
#Sepal.Width          1.407675
#Petal.Length        27.888180
#Petal.Width         23.093445

print(rf)
#Call:
#  randomForest(formula = Species ~ ., data = train_iris, ntree = 500, mtry = 2, importance = TRUE) 
#                Type of random forest: classification
#                      Number of trees: 500
#No. of variables tried at each split: 2
#
#        OOB estimate of  error rate: 6.67%
#Confusion matrix:
#           setosa versicolor virginica class.error
#setosa         30          0         0  0.00000000
#versicolor      0         26         3  0.10344828
#virginica       0          3        28  0.09677419

varImpPlot(rf)

varUsed(rf, by.tree=FALSE, count=TRUE)
#[1]  528  414 1010  903

# -------------------------------------------------------------
# Gradient Boosted Machines
# -------------------------------------------------------------

install.packages("gbm")
library(gbm)

data(iris)

n <- nrow(iris)  # Number of observations

ntrain <- round(n*0.6)  # 60% for training set

set.seed(333)    # Set seed for reproducible results

tindex <- sample(n, ntrain)   # Create an index

train_iris <- iris[tindex,]   # Create training set
test_iris <- iris[-tindex,]   # Create test set

gbm1 <- gbm(Species ~ ., distribution="multinomial", data=train_iris,
                n.trees=2000, shrinkage=0.01)
gbm1
#gbm(formula = Species ~ ., distribution = "multinomial", data = train_iris, 
#    n.trees = 2000, shrinkage = 0.01)
#A gradient boosted model with multinomial loss function.
#2000 iterations were performed.
#There were 4 predictors of which 4 had non-zero influence.

prediction <- predict.gbm(gbm1, test_iris, type="response", n.trees=1000)

summary.gbm(gbm1)
#                      var   rel.inf
#Petal.Length Petal.Length 69.845854
#Petal.Width   Petal.Width 21.582802
#Sepal.Length Sepal.Length  4.440778
#Sepal.Width   Sepal.Width  4.130565

