############################
# Getting and Cleaning Data
#############################
install.packages('xlsx')

# Since swirl is an R package, you can easily install it by entering a single comman
install.packages("swirl")

# If you've installed swirl in the past make sure you have version 2.2.21 or later
packageVersion("swirl")

# Every time you want to use swirl, you need to first load the package
library(swirl)

# Install the Getting and Cleaning Data course
install_from_swirl("Getting and Cleaning Data")

# Start swirl and complete the lessons
swirl()

#first QUiz
setwd("D:/Developer Room/CourseraDataScienceUsingR/Getting and Cleaning Data")


# WEEK 1
########################

#create the data sub-directory if it doesn't exist
if(!file.exists("data")){
  dir.create("data")
}

# create the file url link
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

#download the csv file and save it as propertyData.csv into the data folder
download.file(fileUrl, destfile = "./data/propertyData.csv")
#download.file(dataUrlLINK,destfile = "./data/propertyData.csv", method = "curl")

#set the downloaded date
dateDownloaded <-date()

#list files in the data folder
list.files("./data")

# reading CSV DATA
###################################

# 1. using read.table
propertyTABLEData <- read.table("./data/propertyData.csv", sep = ",", header = TRUE)

# 2. using read.csv
propertyCSVData <- read.csv("./data/propertyData.csv")

library(data.table)
#pass the csv data into a datatable
DT = data.table(propertyCSVData)

#see the tables in memory
tables()

#view top data
head(DT)

#properties are wort is variable VAL , and number 24 represents $1000000+
DT[DT$VAL==24]

#How many properties are worth $1,000,000 or more?
nrow(DT[DT$VAL==24])
nrow(DT[DT$VAL>13])

#the vriable FES does not represent one data per column

# reading EXCEL DATA
######################
fileUrl_2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl_2, destfile = "./data/naturalGasAqProg.xlsx" , method = "curl")

library(xlsx)
natGas <- read.xlsx("./data/naturalGasAqProg.xlsx",1) #load sheet1

#Read rows 18-23 and columns 7-15 into R and assign the result to a variable called dat
colIndex_7_15 <- 7:15
rowIndex_18_23 <- 18:23
dat <- read.xlsx("./data/naturalGasAqProg.xlsx", sheetIndex = 1, colIndex = colIndex_7_15, rowIndex = rowIndex_18_23)

sum(dat$Zip*dat$Ext,na.rm=T)


# reading XML DATA
######################
fileUrl_3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"  #set the url link
download.file(fileUrl_3, destfile = "./data/getdata_data_restaurants.xml")  #download/save xml to file to data folder

#to load the xml
library(XML)
xmlData <- xmlTreeParse("./data/getdata_data_restaurants.xml", useInternalNodes = TRUE)
BaktimoreRestaurant <- xmlRoot(xmlData) #gives content of root
BaktimoreRestaurant

#looking at the data
class(BaktimoreRestaurant) #gives the class
xmlName(BaktimoreRestaurant) #give name of node, response
xmlSize(BaktimoreRestaurant) #how many children in node, 19
xmlName(BaktimoreRestaurant[[1]]) #name of root's children
xmlName(BaktimoreRestaurant[[1]][[1]])
xmlName(BaktimoreRestaurant[[1]][[1]][[1]])
xmlName(BaktimoreRestaurant[[1]][[1]][[2]])

zipcodes <- xpathSApply(BaktimoreRestaurant, '//zipcode', xmlValue)
zipcodes_1 <- as.data.frame (zipcodes)

library(data.table)
zipcodes_2 = data.table(zipcodes)
#How many restaurants have zipcode 21231? 
nrow(zipcodes_2[zipcodes_2$zipcodes==21231])

fileUrl_4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl_4, destfile = "./data/AmericanCommunityHousingSurveyData.csv")

DT<- read.csv("./data/AmericanCommunityHousingSurveyData.csv", header = TRUE, sep = ",")

library(data.table)
DT <- fread("./data/AmericanCommunityHousingSurveyData.csv")

DT$pwgtp15

#One can time the evaluation of an R expression using system.time
system.time(DT$pwgtp15)

sapply(split(DT$pwgtp15,DT$SEX),mean)

system.time( sapply(split(DT$pwgtp15,DT$SEX),mean) )
system.time( DT[,mean(pwgtp15),by=SEX] )
system.time((DT$pwgtp15,by=DT$SEX))
system.time(apply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15))
system.time(rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2])


mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
DT[,mean(pwgtp15),by=SEX]
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
tapply(DT$pwgtp15,DT$SEX,mean)
mean(DT$pwgtp15,by=DT$SEX)
sapply(split(DT$pwgtp15,DT$SEX),mean)

#other tests
xpathSApply(BaktimoreRestaurant, '//row/row/name', xmlValue)
xpathSApply(BaktimoreRestaurant, '/row/row/name', xmlValue)
xpathSApply(BaktimoreRestaurant, '/row/row/zipcode', xmlValue)

xmlName(BaktimoreRestaurant)
#name(BaktimoreRestaurant)

BaktimoreRestaurant[[1]][[1]]
BaktimoreRestaurant[[1]][[1]][["zipcode"]]
xmlSApply(BaktimoreRestaurant, xmlValue)
xpathSApply(BaktimoreRestaurant,"//zipcode", xmlValue)
xpathSApply(BaktimoreRestaurant,"//zipcode[contains(text(), '21231')]", xmlValue)




xpathSApply(BaktimoreRestaurant,"//response[count(.//zipcode)=21231]", xmlValue)
xpathSApply(BaktimoreRestaurant,"//zipcode[contains(text(), '21231')]", xmlValue)
test <- BaktimoreRestaurant %>% htmlTreeParse(xpath= '//zipcode')
xpathSApply(BaktimoreRestaurant,"//zipcode[count(.//zipcode)=21231]", xmlValue)
xpathSApply(BaktimoreRestaurant,"//count(.//zipcode)=21231", xmlValue)


library(dplyr)

# WEEK 2
#############
install.packages("RMySQL")

library(RMySQL)
ucscDB <- dbConnect(MySQL(), user="genome", host="genome-mysql.soe.ucsc.edu")
result <-dbGetQuery(ucscDB,"show databases;") 
dbDisconnect(ucscDB)

#focus on hg19
hg19 <- dbConnect(MySQL(), user="genome",db="hg19", host="genome-mysql.soe.ucsc.edu")
allTables <-dbListTables(hg19)
length(allTables)
allTables[1:5]  #first five(5) tables
dbListFields(hg19,"affyU133Plus2") #show the fields or column names in the SQL table affyU133Plus2
dbGetQuery(hg19,"select count(*) from affyU133Plus2") #count number of observations/rows in the SQL table

#to fetch a table out from SQL
affyData <- dbReadTable(hg19,"affyU133Plus2")  #will return the extracted table data into a data frame with all the table fields
head(affyData)

#to select certain specific fields to reduce memory use
query <- dbSendQuery(hg19,"select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query)
quantile(affyMis$misMatches)

affyMisSmall <- fetch(query, n=10) #don't want to fetch a gigantic table but just a small top 10 records
dbClearResult(query) #clear the sent query to the DB, if successful will return TRUE
dbDisconnect(hg19)

dim(affyMisSmall) #check the dimension of the dataframe table. 10 observation by 22 variables/columns


## Reading Data from HDF5
#first need to install it from biocLite
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install

BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))  #To install core packages
BiocManager::install(c("rhdf5"))  #To install rhdf5

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")

library(rhdf5)  #load the library

