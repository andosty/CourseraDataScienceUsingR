#hospital compare
setwd("C:/Users/andos/Desktop/datascience coursera quiz/rprog-data-ProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)


#read.csv("hospital-data.csv")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

library(data.table)
library(dplyr)
library(ggplot2)
library(janitor)

## 1. Plot the 30-day mortality rates for heart attack
###################################################
# reading data
outcome <- data.table::fread("outcome-of-care-measures.csv", colClasses = "character")

# preprocessing data for histogram
histogram_data <- outcome %>% 
  rename(death_rate_30_HA = 11) %>%
  mutate(death_rate_30_HA = suppressWarnings(as.numeric(death_rate_30_HA))) %>%
  select(death_rate_30_HA) %>%
  unlist()

# plot histogram
hist(histogram_data, 
     main = "Hospital 30-day Death (Mortality) Rates from Heart Attacks",
     xlab = "Deaths", 
     col = "red")

## 2. Finding the best hospital in a state
###################################################
best <- function(state, outcome) {
  # Read outcome data
  dt <- data.table::fread("outcome-of-care-measures.csv")
  
  # change outcome to lowercase
  outcome <- tolower(outcome)
  
  # change variable name to prevent confusion
  chosen_state <- state
  
  # Check state and outcome are valid, if not return warning message
  if (!chosen_state %in% unique(dt[["State"]])) {
    stop("Invalid state")
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid outcome")
  }
  
  dt <- dt %>% 
    rename_with(~ tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", .x))) %>%
    filter(state == chosen_state) %>%
    mutate(rate = suppressWarnings(as.numeric(get(outcome)))) %>%
    clean_names() %>%
    select(hospital_name, state, rate) %>%
    filter(complete.cases(.)) %>%
    arrange(rate, hospital_name) %>%
    mutate(rank = row_number())  
  
  unlist(dt[1,1])
}
#test the best function
best("TX", "heart attack")

## 3. Ranking hospitals by outcome in a state
###################################################
rankHospital <- function(state, outcome, num="best") {
  # Read outcome data
  dt <- data.table::fread("outcome-of-care-measures.csv")
  
  # change outcome to lowercase
  outcome <- tolower(outcome)
  
  # change variable name to prevent confusion
  chosen_state <- state
  
  # Check state and outcome are valid, if not return warning message
  if (!chosen_state %in% unique(dt[["State"]])) {
    stop("Invalid state")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid outcome")
  }
  
  dt <- dt %>% 
    rename_with(~ tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", .x))) %>%
    filter(state == chosen_state) %>%
    mutate(rate = suppressWarnings(as.numeric(get(outcome)))) %>%
    clean_names() %>%
    select(hospital_name, state, rate) %>%
    filter(complete.cases(.)) %>%
    arrange(rate, hospital_name) %>%
    mutate(rank = row_number())  
  
  if (num == "best") {
    unlist(head(dt[[1]], 1))
  }
  
  else if (num == "worst") {
    unlist(tail(dt[[1]], 1))
  }
  
  else {
    dt %>% 
      slice(num) %>%
      select(hospital_name) %>%
      unlist()
  }
}
#test the rank function
rankHospital("TX", "heart failure", "best")

## 4. Ranking hospitals in all states
###################################################
rankAll <- function(outcome, num = "best") {
  # Read outcome data
  dt <- data.table::fread("outcome-of-care-measures.csv")
  
  # change outcome to lowercase
  outcome <- tolower(outcome)
  
  # check if outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  dt <- dt %>% 
    rename_with(~ tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", .x))) %>%
    mutate(rate = suppressWarnings(as.numeric(get(outcome)))) %>%
    clean_names() %>%
    select(hospital_name, state, rate) %>%
    filter(complete.cases(.)) %>%
    group_by(state) %>%
    arrange(rate, hospital_name, .by_groups=TRUE) %>% 
    arrange(state) %>%
    mutate(rank = row_number()) 
  
  if (num == "best") {
    dt %>% 
      filter(rank == 1) %>%
      select(hospital_name, state)
  }
  
  else if (num == "worst") {
    dt %>%
      group_by(state) %>%
      filter(rank == max(rank)) %>%
      select(hospital_name, state)
  }
  
  else {
    dt %>%
      group_by(state) %>%
      filter(rank == num) %>%
      select(hospital_name, state)
  }
}
#test the rankAll function
tail(rankAll("heart failure"), 10)



r <- rankAll("heart attack", 4)
as.character(subset(r, state == "HI")$hospital_name)


r <- rankAll("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital_name)

r <- rankAll("heart failure", 10)
as.character(subset(r, state == "NV")$hospital_name)