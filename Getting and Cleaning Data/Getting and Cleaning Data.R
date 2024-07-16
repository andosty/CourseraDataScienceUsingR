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

#American Community Survey data
fileUrl_4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl_4, destfile = "./data/AmericanCommunityHousingSurveyData.csv")

DT<- read.csv("./data/AmericanCommunityHousingSurveyData.csv", header = TRUE, sep = ",")

library(data.table)
acs <- fread("./data/AmericanCommunityHousingSurveyData.csv")
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
created = h5createFile("example.h5")
created

#accessing git hub api
##############################
#Question 1
#install.packages("jsonlite")
#install.packages("httpuv")
#install.packages("httr")

library(jsonlite)
library(httpuv)
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on your appname, key, and secret 
myapp <- oauth_app(appname = "courseraApiTest",
                   key = "Ov23ctpfagge19DnIvKE",
                   secret = "f90d61648c7f56386796acc7a96439b147c8948a")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 
# Answer: 
# 2013-11-07T13:25:07Z

#next question
#############

detach("package:RMySQL", unload=TRUE) #must unload RMYSQL for sqldf to work
library(sqldf)
rm(acs,acs2)
acs <- read.csv.sql("./data/AmericanCommunityHousingSurveyData.csv", sql = "select * from file", header = TRUE, sep = ",") 
summary(acs)
names(acs)

pwgtp1_less50  <- read.csv.sql("./data/AmericanCommunityHousingSurveyData.csv", 
                      sql = "select pwgtp1 from acs where AGEP < 50")

query1 <- sqldf("select pwgtp1 from acs where AGEP < 50")
             
sqldf("select * from acs where AGEP < 50")
sqldf("select pwgtp1 from acs where AGEP < 50")

#equivalent function to unique(acs$AGEP
sqldf("select distinct AGEP from acs")

con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode=readLines(con)
close(con)
htmlCode
c(nchar(htmlCode[10]), nchar(htmlCode[20]), nchar(htmlCode[30]), nchar(htmlCode[100]))


#Read this data set into R and report the sum of the numbers in the fourth of the nine columns.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
lines <- readLines(url, n = 10)
w <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)
colNames <- c("filler", "week", "filler", "sstNino12", "filler", "sstaNino12", 
              "filler", "sstNino3", "filler", "sstaNino3", "filler", "sstNino34", "filler", 
              "sstaNino34", "filler", "sstNino4", "filler", "sstaNino4")
d <- read.fwf(url, w, header = FALSE, skip = 4, col.names = colNames)
d <- d[, grep("^[^filler]", names(d))]
sum(d[, 4])

#Week2
#################
#Subsetting quick review
set.seed(13435)
x <-data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
x <- x[sample(1:5),]; x$var2[c(1,3)]=NA
x

x[,1] #open 1st column in the dataframe using column index number
x[,"var1"] #open 1st column in the dataframe using column name
x[1:2,"var2"] #open first two rows of X on the 2nd column in the dataframe using column name

x[(x$var1 <=3 & x$var3 >11),] #show all the rows of x, where var1<=3 and var3>11
x[(x$var1 <=3 | x$var3 >15),] #show all the rows of x, where var1<=3 or var3>15

x[(x$var1 <=3 | x$var3 >15),"var1"] #show all the rows of x , where var1<=3 or var3>15, but show only colname var1
x[(x$var1 <=3 | x$var3 >15),c("var1","var2")] #show all the rows of x , where var1<=3 or var3>15, but show only colname var1 & colname var2

#dealing with missing values
x[(x$var2 > 8),] #This won't deal with NA results
x[which(x$var2 > 8),]  #This will deal with missing values

#sorting
x
sort(x$var1)                    #sort ascending order
sort(x$var1, decreasing = TRUE) #sort descending order
sort(x$var2, na.last = TRUE)  # sort but put all missing values at the last

# Ordering
#this will order the dataframe output
x[order(x$var1), ]          #order the dataframe in ascending order of var1
x[order(x$var1, x$var3), ]  #order the dataframe in ascending order of var1 (1st) and var3(2nd)


#using plyr package to order dataframes
library(dplyr)
arrange(x,var1)  #order the dataframe in ascending order of var1
arrange(x,desc(var1))  #order the dataframe in descending order of var1

x$var4 <- rnorm(5) #create a var 4 and assign it to the x dataframe
Y <- cbind(x, rnorm(5)) #create a var and bind it column-wise to the x dataframe, and call the new dataframe as Y
#can use rbind to bind it row-wise, at the button of the data frame

#summarsing data
summary(restData)  #will give min vals
str(restData) #will add data types
quantile(restData$counilDistrict, na.rm=TRUE)  #show quantiles
quantile(restData$counilDistrict, probs=c(0.5,0.75,0.9)) #show stated percentiles
table(restData$zipcode, useNA = "ifany")  #to make sure missing values are shown

table(restData$counilDistrict, restData$zipcode) #show a two dimensional table


#check for missing values
sum(is.na(restData$counilDistrict)) #count number of missing values
any(is.na(restData$counilDistrict)) #check if there are any variables that is na. if none, will return false
all(restData$counilDistrict > 0) #check if all zipcodes are >0. if some are not, it will return false

colSums(is.na(restData)) #check all columns in dataset if it has na values. if 0, there are no na values for that column
all(colSums(is.na(restData))==0) #will return true if there are no missing vaklues in the dataset

#find values with specific characteristics
table(restData$zipcode %in% c("21212")) #are there any values of zipcodes with the value 21212. will return count if true if found
table(restData$zipcode %in% c("21212","21213")) #are there any values of zipcodes with the value 21212 or 21213. will return count if true if found

restData[restData$zipcode %in% c("21212","21213"),] #subset the dataset for restaurants with zipcode as 21212 or 21213. as row subsetting

#cross tabulation of the data set
rm(list=)
data("UCBAdmissions") #load one of the classic data sets in R
DF = as.data.frame(UCBAdmissions)

summary(DF)
str(DF)
all(colSums(is.na(DF))==0)

xt <- xtabs(Freq~Gender + Admit, data=DF) #the first colname eg Freq, which is on the left of the swung-dash or tilde  is the one that will actually be in the table
xt

xt <- xtabs(Freq~Gender + Admit + Dept, data=DF) #multi-layered table in compact form
ftable(xt)

#check the size of a dataset
fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units="Mb")

#Export tables to xlsx files
#########################################
install.packages("openxlsx")
install.packages("expss")

library(expss)
library(openxlsx)
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

mtcars_table = mtcars %>% 
  cross_cpct(
    cell_vars = list(cyl, gear),
    col_vars = list(total(), am, vs)
  ) %>% 
  set_caption("Table 1")

mtcars_table

#Then we create workbook and add worksheet to it.
wb = createWorkbook()
sh = addWorksheet(wb, "Tables_mtCars")

#Export - we should specify workbook and worksheet.
xl_write(mtcars_table, wb, sh)

#And, finally, we save workbook with table to the xlsx file.
saveWorkbook(wb, "table1.xlsx", overwrite = TRUE)




