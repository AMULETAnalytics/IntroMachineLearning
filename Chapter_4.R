# File-Name:       Chapter_4.R           
# Date:            2015-10-10                                
# Author:          Daniel D. Gutierrez (daniel@amuletanalytics.com)
# Purpose:         Machine Learning and Data Science: code for Chapter 4 - EDA

# All source code is copyright (c) 2015, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

data(ToothGrowth)
data(airquality)
data(iris)

# ---------------------------------------------------------------
# Numeric Summaries
# ---------------------------------------------------------------

# Show unique values found for a variable
unique(airquality$Month)
#[1] 5 6 7 8 9

# Count unique values for a variable

library(sqldf)
sqldf("select count(Ozone) from airquality where Ozone=11")
#  count(Ozone)
#1            3


# Summary
summary(airquality)

#Ozone           Solar.R           Wind       
#Min.   :  1.00   Min.   :  7.0   Min.   : 1.700  
#1st Qu.: 18.00   1st Qu.:115.8   1st Qu.: 7.400  
#Median : 31.50   Median :205.0   Median : 9.700  
#Mean   : 42.13   Mean   :185.9   Mean   : 9.958  
#3rd Qu.: 63.25   3rd Qu.:258.8   3rd Qu.:11.500  
#Max.   :168.00   Max.   :334.0   Max.   :20.700  
#NA's   :37       NA's   :7                       
#Temp           Month            Day      
#Min.   :56.00   Min.   :5.000   Min.   : 1.0  
#1st Qu.:72.00   1st Qu.:6.000   1st Qu.: 8.0  
#Median :79.00   Median :7.000   Median :16.0  
#Mean   :77.88   Mean   :6.993   Mean   :15.8  
#3rd Qu.:85.00   3rd Qu.:8.000   3rd Qu.:23.0  
#Max.   :97.00   Max.   :9.000   Max.   :31.0  

# Mean, Min, Max, Range, Quantile, Fivenum
mean(airquality$Ozone, na.rm=TRUE)
#[1] 42.12931

min(airquality$Wind)
#[1] 1.7

max(airquality$Solar.R, na.rm=TRUE)
#[1] 334

range(airquality$Month)
#[1] 5 9

quantile(airquality$Ozone, na.rm=TRUE)
#  0%    25%    50%    75%   100% 
#1.00  18.00  31.50  63.25 168.00 


var(airquality$Temp)
# [1] 89.59133

var(airquality$Ozone, na.rm=TRUE)
# [1] 1088.201

head(airquality)
#Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
#3    12     149 12.6   74     5   3
#4    18     313 11.5   62     5   4
#5    NA      NA 14.3   56     5   5
#6    28      NA 14.9   66     5   6

# Discover levels of a factor variable
levels(ToothGrowth$supp)
#[1] "OJ" "VC"


# 3 ways to find the number of non-missing values of Ozone
length(airquality$Ozone[is.na(airquality$Ozone) == FALSE])
length(airquality$Ozone[!is.na(airquality$Ozone)])
sum(!is.na(airquality$Ozone))
# [1] 116


# ---------------------------------------------------------------
# Histograms
# ---------------------------------------------------------------

# Single variable frequency plot
hist(iris$Sepal.Length, col="blue")

# Frequency distribution plot
hist(iris$Sepal.Length, probability=TRUE, breaks=10, col="blue")
lines(density(iris$Sepal.Length))


# ---------------------------------------------------------------
# Boxplots
# ---------------------------------------------------------------

# For a quantitative variable, goal: view distribution of data
boxplot(airquality$Ozone, col="blue")

# Show len broken down by supp variable
boxplot(ToothGrowth$len ~ as.factor(ToothGrowth$supp), col="blue")


boxplot(ToothGrowth$len ~ as.factor(ToothGrowth$supp), col=c("blue","orange"), varwidth=TRUE)


# ---------------------------------------------------------------
# Barplots
# ---------------------------------------------------------------

table(airquality$Temp)

#56 57 58 59 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 
#1  3  2  2  3  2  1  2  2  3  4  4  3  1  3  3  5  4  4  9  7  6  6  5 11  9  4  5 
#85 86 87 88 89 90 91 92 93 94 96 97 
#5  7  5  3  2  3  2  5  3  2  1  1 

barplot(table(airquality$Temp), col="blue")


# ---------------------------------------------------------------
# Density Plots
# ---------------------------------------------------------------

temp_dens <- density(airquality$Temp)
plot(temp_dens, lwd=3, col="blue")

# Overlay filtered density
len_dens <- density(ToothGrowth$len)
plot(len_dens, lwd=3, col="blue")

VC_dens <- density(ToothGrowth$len[which(ToothGrowth$supp=="VC")])
lines(VC_dens, lwd=3, col="orange")


# ---------------------------------------------------------------
# Scatterplots
# ---------------------------------------------------------------

plot(ToothGrowth$len, ToothGrowth$dose, pch=19, col="blue")

plot(ToothGrowth$len, ToothGrowth$dose, pch=ifelse(ToothGrowth$supp=="VC",0,1), col="blue")

# 3D scatter plots
library(scatterplot3d)

scatterplot3d(airquality$Solar.R, airquality$Wind, airquality$Temp, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue",main="Air Quality Data Set", pch=20, xlab="Solar Radiation", ylab="Wind", zlab="Temp")

# Correlation matrix
cor(iris[,c(1,2,3,4)], method="pearson")
#             Sepal.Length Sepal.Width Petal.Length Petal.Width
#Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
#Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
#Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
#Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# Scatterplot matrix
pairs(iris[,c(1,2,3,4)])

# Big Data techniques

# Plotting too many points
x <- rnorm(1e5)
y <- rnorm(1e5)
plot(x,y, pch=16)

# Random sampling

sampledSubset <- sample(1:1e5, size=1000, replace=FALSE)
plot(x[sampledSubset], y[sampledSubset], pch=16)

# Plot a large number of point. Show outliers
smoothScatter(x,y)

# Count bins
install.packages("hexbin")
library(hexbin)

hbins <- hexbin(x,y)
plot(hbins)


# ---------------------------------------------------------------
# QQ-plots
# ---------------------------------------------------------------

x <- rnorm(50)
y <- rnorm(50)
qqplot(x,y)
abline(c(0,1))


# ---------------------------------------------------------------
# Heatmaps
# ---------------------------------------------------------------

image(1:150, 1:4, as.matrix(iris[1:150, 1:4]))

# Transpose data matrix to be more intuitive
transMatrix <- as.matrix(iris[1:150, 1:4])
transMatrix <- t(transMatrix)[,nrow(transMatrix):1]
image(1:4, 1:150, transMatrix)

# Boxplot for missing values

boxplot(airquality$Temp ~ is.na(airquality$Solar.R))


# ---------------------------------------------------------------
# Expository Plots
# ---------------------------------------------------------------

par(mfrow=c(1,2))
hist(airquality$Ozone, xlab="Ozone (ppb)", col="blue", main="Ozone Frequencies")

plot(airquality$Ozone, airquality$Temp, pch=16, col="blue", cex=1.25, xlab="Ozone (ppb)", ylab="Temperature (degrees F)", main="Air Quality - Ozone vs. Temp", cex.axis=1.5)
legend(125,60,legend="May-Sep 1973", col="blue", pch=16, cex=1.0)






