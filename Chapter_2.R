# File-Name:       Chapter_2.R           
# Date:            2015-10-10                                
# Author:          Daniel D. Gutierrez (daniel@amuletanalytics.com)
# Purpose:         Machine Learning and Data Science: code for Chapter 2 - Data Access

# All source code is copyright (c) 2015, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# --------------------------------------------------------------
# Set the working directory
# --------------------------------------------------------------

setwd("./MYPROJECT")      # Relative path
getwd()
#[1] "C:/Users/Dan/MYPROJECT"

setwd("/Users/dan/MYPROJECT")   # Absolute path
getwd()
#[1] "C:/Users/dan/MYPROJECT"

# Path name for Windows
setwd("C:\\Users\\Dan\\MYPROJECT")
getwd()
# [1] "C:/Users/Dan/MYPROJECT"


# --------------------------------------------------------------
# Download a file from the web
# --------------------------------------------------------------

fileUrl <- "https://data.sfgov.org/api/views/7egw-qt89/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/SFParkingMeters.csv")

list.files("./data")
# [1] "SFParkingMeters.csv"

SFParkingMeters <- read.table("./data/SFParkingMeters.csv",sep=",", header=TRUE)
head(SFParkingMeters)

#POST_ID MS_ID MS_SPACEID CAP_COLOR METER_TYPE SMART_METE ACTIVESENS
#1 354-20160     -        0.0      Grey         SS          Y          Y
#2 354-21030     -        0.0     Green         SS          Y          Y
#3 354-21160     -        0.0    Yellow         SS          Y          Y
#4 363-05250     -        0.0      Grey         SS          N          N
#5 363-05270     -        0.0      Grey         SS          N          N
#6 464-04120     -        0.0      Grey         SS          Y          Y
#JURISDICTI ON_OFF_STR OSP_ID STREET_NUM   STREETNAME STREET_SEG RATEAREA
#1      SFMTA         ON    0.0     2016.0  CHESTNUT ST  3977000.0   Area 5
#2      SFMTA         ON    0.0     2103.0  CHESTNUT ST  3979000.0   Area 5
#3      SFMTA         ON    0.0     2116.0  CHESTNUT ST  3979000.0   Area 5
#4      SFMTA         ON    0.0      525.0 COLUMBUS AVE  4295000.0   Area 3
#5      SFMTA         ON    0.0      527.0 COLUMBUS AVE  4295000.0   Area 3
#6      SFMTA         ON    0.0      412.0     HAYES ST  6816000.0   Area 5
#SFPARKAREA                 LOCATION
#1       Marina  (37.800798, -122.43687)
#2       Marina (37.800522, -122.438067)
#3       Marina (37.800589, -122.438525)
#4              (37.800053, -122.409985)
#5              (37.800088, -122.410035)
#6 Civic Center (37.776878, -122.423512)

# --------------------------------------------------------------
# Reading CSV files
# --------------------------------------------------------------

SFParkingMeters <- read.csv("./data/SFParkingMeters.csv")

# Pick a file from your local file system
SFParkingMeters <- read.csv(file.choose())

# --------------------------------------------------------------
# Reading EXCEL files
# --------------------------------------------------------------

install.packages("xlsx")
library(xlsx)

fileUrl <- "https://data.sfgov.org/api/views/7egw-qt89/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/SFParkingMeters.xlsx", mode="wb")

SFParkingMeters <- read.xlsx2("./data/SFParkingMeters.xlsx", sheetIndex=1)

# --------------------------------------------------------------
# Using File Connections
# --------------------------------------------------------------

con <- file("./data/SFParkingMeters.csv", "r")

SFParkingMeters <- read.csv(con)

close(con)

# Now read a web page
con <- url("http://radicaldatascience.wordpress.com/", "r")

RDS <- readLines(con, n=20)

close(con)

head(RDS)

# [1] "<!DOCTYPE html>"               "<!--[if IE 7]>"               
# [3] "<html id=\"ie7\" lang=\"en\">" "<![endif]-->"                 
# [5] "<!--[if IE 8]>"                "<html id=\"ie8\" lang=\"en\">"

class(RDS)

# [1] "character"


# --------------------------------------------------------------
# Reading JSON Files
# --------------------------------------------------------------

install.packages("RJSONIO")
library(RJSONIO)

fileURL <- "https://data.sfgov.org/api/views/7egw-qt89/rows.json?accessType=DOWNLOAD"

# fromJSON returns a nested list with: meta, data
parkdata <- fromJSON(fileURL)[[2]]  # Only use the data portion

# Extract variables into df: CAP_COLOR, METER_TYPE, STREETNAME

park_df = data.frame(
  CAP_COLOR = sapply(parkdata, function(x) x[[12]]),
  METER_TYPE = sapply(parkdata, function(x) x[[13]]),
  STREETNAME = sapply(parkdata, function(x) x[[20]])    
)

head(park_df)
#  CAP_COLOR METER_TYPE   STREETNAME
#1      Grey         SS  CHESTNUT ST
#2     Green         SS  CHESTNUT ST
#3    Yellow         SS  CHESTNUT ST
#4      Grey         SS COLUMBUS AVE
#5      Grey         SS COLUMBUS AVE
#6      Grey         SS     HAYES ST


# --------------------------------------------------------------
# Scraping data from websites
# --------------------------------------------------------------

install.packages("XML")
install.packages("reshape2")
library(XML)
library(reshape2)

webdata <- readHTMLTable('http://www.ism.ws/ISMReport/content.cfm?ItemNumber=10752')
df <- data.frame(webdata[[1]])
names(df)[1] <- 'Year'
head(df)
#  Year  JAN  FEB  MAR  APR  MAY  JUN  JUL  AUG  SEP  OCT  NOV  DEC
#1 2014 51.3                                                       
#2 2013 52.3 53.1 51.5 50.0 50.0 52.5 54.9 56.3 56.0 56.6 57.0 56.5
#3 2012 52.8 52.4 53.0 53.7 53.2 51.0 50.6 51.1 52.2 51.2 49.5 50.4
#4 2011 59.0 59.3 59.1 58.9 53.7 56.6 52.9 53.0 52.8 51.8 52.1 53.1
#5 2010 57.2 55.8 58.8 58.1 58.3 56.4 56.4 58.0 56.3 57.7 57.6 57.5
#6 2009 34.9 35.5 36.0 39.5 41.7 45.8 49.9 53.5 54.4 56.0 54.4 55.3

# Reshape the data
df <- melt(df,id.vars='Year')

names(df) <- c('Year','Month','PMI')
df$PMI <- as.numeric(df$PMI)
df <- na.omit(PMI)


# --------------------------------------------------------------
# SQL Databases
# --------------------------------------------------------------

# Read from SQL Server Express

install.packages("RODBC")
library(RODBC)

con <- odbcConnect("Heritage", uid="dan")

df <- sqlQuery(con, "SELECT TOP 1000 [MemberID]
      ,[ProviderID]
      ,[Vendor]
                      ,[PCP]
                      ,[Year]
                      ,[Specialty]
                      ,[PlaceSvc]
                      ,[PayDelay]
                      ,[LengthOfStay]
                      ,[DSFS]
                      ,[PrimaryConditionGroup]
                      ,[CharlsonIndex]
                      ,[ProcedureGroup]
                      ,[SupLOS]
                      ,[dsfsI]
                      ,[CharlsonIndexI]
                      ,[LengthOfStayI]
                      ,[PayDelayI]
                      FROM [Heritage].[dbo].[Claims]")

odbcClose(con)

names(df)

# [1] "MemberID"              "ProviderID"            "Vendor"               
# [4] "PCP"                   "Year"                  "Specialty"            
# [7] "PlaceSvc"              "PayDelay"              "LengthOfStay"         
# [10] "DSFS"                  "PrimaryConditionGroup" "CharlsonIndex"        
# [13] "ProcedureGroup"        "SupLOS"                "dsfsI"                
# [16] "CharlsonIndexI"        "LengthOfStayI"         "PayDelayI"   

mean(df$PayDelayI)

# [1] 42.944


install.packages("sqldf")

library (sqldf)

# SQL queries using sqldf

orders <- data.frame(order_no=c("10021","10022","10023","10024","10025"), prod_id=c("AC-01","AC-01","AD-11","AE-21","AM-19"), qty=c(1,1,2,3,1))

product <- data.frame(prod_id=c("AC-01","AD-11","AE-21","AM-19","AG-40"), desc=c("Widget A","Widget B","Widget C","Widget D", "Widget E"), price=c(123.50,25,55,17.95,45.33))

sqldf("SELECT o.*, p.price FROM orders o INNER JOIN product p ON o.prod_id = p.prod_id;")

#order_no prod_id qty  price
#1    10021   AC-01   1 123.50
#2    10022   AC-01   1 123.50
#3    10023   AD-11   2  25.00
#4    10024   AE-21   3  55.00
#5    10025   AM-19   1  17.95


# --------------------------------------------------------------
# SQL Equivalents in R
# --------------------------------------------------------------

data(CO2)

head(CO2)
#  Plant   Type  Treatment conc uptake
#1   Qn1 Quebec nonchilled   95   16.0
#2   Qn1 Quebec nonchilled  175   30.4
#3   Qn1 Quebec nonchilled  250   34.8
#4   Qn1 Quebec nonchilled  350   37.2
#5   Qn1 Quebec nonchilled  500   35.3
#6   Qn1 Quebec nonchilled  675   39.2

# SELECT * FROM CO2 WHERE conc>400 AND uptake>40

CO2_subset <- CO2[CO2$conc>400 & CO2$uptake>40,]
head(CO2_subset)
#   Plant   Type  Treatment conc uptake
#12   Qn2 Quebec nonchilled  500   40.6
#13   Qn2 Quebec nonchilled  675   41.4
#14   Qn2 Quebec nonchilled 1000   44.3
#19   Qn3 Quebec nonchilled  500   42.9
#20   Qn3 Quebec nonchilled  675   43.9
#21   Qn3 Quebec nonchilled 1000   45.5

dim(CO2_subset)
#[1] 8 5

# SELECT * FROM CO2 ORDER BY conc, uptake DESC

CO2[order(CO2$conc, -CO2$uptake),][1:20,]

#   Plant        Type  Treatment conc uptake
#15   Qn3      Quebec nonchilled   95   16.2
#1    Qn1      Quebec nonchilled   95   16.0
#36   Qc3      Quebec    chilled   95   15.1
#22   Qc1      Quebec    chilled   95   14.2
#8    Qn2      Quebec nonchilled   95   13.6
#50   Mn2 Mississippi nonchilled   95   12.0
#57   Mn3 Mississippi nonchilled   95   11.3
#43   Mn1 Mississippi nonchilled   95   10.6
#78   Mc3 Mississippi    chilled   95   10.6
#64   Mc1 Mississippi    chilled   95   10.5
#29   Qc2      Quebec    chilled   95    9.3
#71   Mc2 Mississippi    chilled   95    7.7
#16   Qn3      Quebec nonchilled  175   32.4
#2    Qn1      Quebec nonchilled  175   30.4
#9    Qn2      Quebec nonchilled  175   27.3
#30   Qc2      Quebec    chilled  175   27.3
#23   Qc1      Quebec    chilled  175   24.1
#51   Mn2 Mississippi nonchilled  175   22.0
#37   Qc3      Quebec    chilled  175   21.0
#58   Mn3 Mississippi nonchilled  175   19.4


# SELECT Plant, AVG(uptake) FROM CO2 GROUP BY Plant

aggregate(x=CO2[,c("uptake")], by=data.frame(CO2$Plant), FUN="mean")

#   CO2.Plant        x
#1        Qn1 33.22857
#2        Qn2 35.15714
#3        Qn3 37.61429
#4        Qc1 29.97143
#5        Qc3 32.58571
#6        Qc2 32.70000
#7        Mn3 24.11429
#8        Mn2 27.34286
#9        Mn1 26.40000
#10       Mc2 12.14286
#11       Mc3 17.30000
#12       Mc1 18.00000


# LEFT JOIN based on Type
head(CO2)

#  Plant   Type  Treatment conc uptake
#1   Qn1 Quebec nonchilled   95   16.0
#2   Qn1 Quebec nonchilled  175   30.4
#3   Qn1 Quebec nonchilled  250   34.8
#4   Qn1 Quebec nonchilled  350   37.2
#5   Qn1 Quebec nonchilled  500   35.3
#6   Qn1 Quebec nonchilled  675   39.2

stateprov <- c("Mississippi", "California", "Victoria", "New South Wales", "Quebec", "Ontario")
country <- c("United States", "United States", "Australia", "Australia", "Canada", "Canada")

geo_map <- data.frame(country=country, stateprov=stateprov)
geo_map

#        country            Type
#1 United States     Mississippi
#2 United States      California
#3     Australia        Victoria
#4     Australia New South Wales
#5        Canada          Quebec
#6        Canada         Ontario

colnames(geo_map) <- c("country", "Type")
joinCO2 <- merge(CO2, geo_map, by=c("Type"))
head(joinCO2)

#         Type Plant  Treatment conc uptake       country
#1 Mississippi   Mn1 nonchilled   95   10.6 United States
#2 Mississippi   Mn1 nonchilled  175   19.2 United States
#3 Mississippi   Mn1 nonchilled  250   26.2 United States
#4 Mississippi   Mn1 nonchilled  350   30.0 United States
#5 Mississippi   Mn1 nonchilled  500   30.9 United States
#6 Mississippi   Mn1 nonchilled  675   32.4 United States


# --------------------------------------------------------------
# Reading Twitter Data
# --------------------------------------------------------------

install.packages("twitteR")   # Contains ROAuth

library(twitteR)
library(ROAuth)

# Windows users need to get this cert file
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "OE9PrQA0YNJ0C1CQRCuBw"
consumerSecret <- "QP6KbRn2vncJhS6VszMTpbXX9jESRQIs4vAHq83Tc"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)

Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
#9999999

save(Cred, file="twitter authentication.Rdata")
registerTwitterOAuth(Cred)
#[1] TRUE

# For future use
load("twitter authentication.Rdata")
registerTwitterOAuth(Cred)

# Read Tweets for hashtag #MLB  

MLB.list <- searchTwitter('#MLB', n=499, cainfo="cacert.pem")  
MLB.df = twListToDF(MLB.list)  
write.csv(MLB.df, file='MLBTweets.csv', row.names=F)


# --------------------------------------------------------------
# Reading Data from Google Analytics
# --------------------------------------------------------------

library(xlsx)

GA <- read.xlsx2("./data/Analytics_All_Traffic.xlsx", sheetIndex=2)
head(GA)

#Source_Medium Visits        Pages_Visit Avg_Visit_Duration
#1               google / organic 1349.0  5.638991845811712 151.54558932542625
#2              (direct) / (none)  562.0  4.119217081850533 114.98220640569394
#3 fashiondistrict.org / referral  242.0 3.9214876033057853 140.30578512396696
#4                yahoo / organic   73.0 3.6301369863013697  66.53424657534246
#5                 bing / organic   71.0  5.549295774647887 104.14084507042253
#6       oohlaluxe.net / referral   36.0  4.361111111111111 116.80555555555556
#Percent_New_Visits         Bounce_Rate
#1  0.614529280948851  0.5181616011860638
#2  0.597864768683274  0.5747330960854092
#3 0.8099173553719008 0.45041322314049587
#4  0.684931506849315   0.410958904109589
#5  0.647887323943662 0.29577464788732394
#6 0.8888888888888888  0.3611111111111111


# Now use the RGoogleAnalytics API

install.packages("RGoogleAnalytics")
install.packages("RCurl")  
install.packages("RJSON")  

# Loading the RGoogleAnalytics library
library(RGoogleAnalytics) 
library(RCurl)
library(RJSON)

# 1. Authorize your account and paste the accesstoken 
query <- QueryBuilder()

# Running the following line will open a browser window requiring login
# to Google, will then generate a token that will be pasted in the 
# R console. 
access_token <- query$authorize()                                                

# 2.  Create a new Google Analytics API object
ga <- RGoogleAnalytics()
ga.profiles <- ga$GetProfileData(access_token)

# List the GA profiles 
ga.profiles

# 3. Build the query string, use the profile by setting its index value 
query$Init(start.date = "2013-07-01",
           end.date = "2013-07-01",
           dimensions = "ga:date,ga:pagePath",
           metrics = "ga:visits,ga:pageviews,ga:timeOnPage",
           sort = "ga:visits",
           #filters="",
           #segment="",
           max.results = 99,
           table.id = paste("ga:",ga.profiles$id[1],sep="",collapse=","),
           access_token=access_token)

# 4. Make a request to get the data from the API
ga.data <- ga$GetReportData(query)

# [1] "Your query matched 88 results that are stored to dataframe ga.data"

# 5. Look at the returned data
head(ga.data)

# date                                                      pagePath
# 1 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=127&CategoryID=0
# 2 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=130&CategoryID=0
# 3 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=175&CategoryID=0
# 4 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=181&CategoryID=0
# 5 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=184&CategoryID=0
# 6 20130701 /SearchResult.asp?Type=ALL&String=&CompanyID=186&CategoryID=0
# visits pageviews timeOnPage
# 1      0         3         11
# 2      0         4         20
# 3      0         2         13
# 4      0         1          1
# 5      0         2         11
# 6      0         3          6

# --------------------------------------------------------------
# Writing Data
# --------------------------------------------------------------

tempDF <- SFParkingMeters[,-1]   # Remove POST_ID variable

write.table(tempDF, file="./data/newSFParkingMeters.csv", sep=",")

newSFParkingMeters <- read.table("./data/newSFParkingMeters.csv", sep=",")

head(newSFParkingMeters)

#MS_ID MS_SPACEID CAP_COLOR METER_TYPE SMART_METE ACTIVESENS JURISDICTI ON_OFF_STR
#1     -          0      Grey         SS          Y          Y      SFMTA         ON
#2     -          0     Green         SS          Y          Y      SFMTA         ON
#3     -          0    Yellow         SS          Y          Y      SFMTA         ON
#4     -          0      Grey         SS          N          N      SFMTA         ON
#5     -          0      Grey         SS          N          N      SFMTA         ON
#6     -          0      Grey         SS          Y          Y      SFMTA         ON
#OSP_ID STREET_NUM   STREETNAME STREET_SEG RATEAREA   SFPARKAREA
#1      0       2016  CHESTNUT ST    3977000   Area 5       Marina
#2      0       2103  CHESTNUT ST    3979000   Area 5       Marina
#3      0       2116  CHESTNUT ST    3979000   Area 5       Marina
#4      0        525 COLUMBUS AVE    4295000   Area 3             
#5      0        527 COLUMBUS AVE    4295000   Area 3             
#6      0        412     HAYES ST    6816000   Area 5 Civic Center
#LOCATION
#1  (37.800798, -122.43687)
#2 (37.800522, -122.438067)
#3 (37.800589, -122.438525)
#4 (37.800053, -122.409985)
#5 (37.800088, -122.410035)
#6 (37.776878, -122.423512)







