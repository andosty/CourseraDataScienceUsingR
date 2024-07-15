https://rstudio-pubs-static.s3.amazonaws.com/220397_d07534a9d3de4d0d87d7df9036602296.html

# specdata <-"C:/Users/andos/Desktop/datascience coursera quiz/specdata"


rm(list = ls())
setwd("C:/Users/andos/Desktop/datascience coursera quiz")

#function1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  means <- c()
  
  for(monitor in id)
  {
    location<-paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    data <- read.csv(location)
    monitorPollutant <- data[pollutant]
    means <- c(means, monitorPollutant[!is.na(monitorPollutant)])
  }
  mean(means)
}

#function2
complete <- function(directory, id = 1:332){
  
  outputData <- data.frame(id=numeric(0), nobs=numeric(0))
  
  for(monitor in id){
    location<-paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    data <- read.csv(location)
    monitorPollutant <- data[(!is.na(data$sulfate)), ]
    monitorPollutantOkay <- monitorPollutant[(!is.na(monitorPollutant$nitrate)), ]
    nobs <- nrow(monitorPollutantOkay)
    outputData <- rbind(outputData, data.frame(id=monitor, nobs=nobs))
  }
  outputData
}


corr <- function(directory, threshold = 0){
  if(nrow(complete_cases)>0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      #print(path)
      monitor_data <- read.csv(path)
      #print(monitor_data)
      interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
      interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
      sulfate_data <- interested_data["sulfate"]
      nitrate_data <- interested_data["nitrate"]
      cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
    }
  }
  cor_results
}

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)