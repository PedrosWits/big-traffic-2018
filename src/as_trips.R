#!/usr/bin/env Rscript
suppressWarnings(suppressMessages(library(tidyverse)))

args = commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
  stop("One argument only must be supplied: threshold value", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  threshold = as.integer(args[1])
}

start.time = Sys.time()

cat("Threshold = ", threshold, "\n")

cat("\nLoading data ..", "\n")

tmpenv <- new.env()

load(file = "data/np_data.RData", envir = tmpenv)

attach(tmpenv)

#print(head(np_data))

as.trips = function(rawdata, threshold) {
  start.time.1 = Sys.time()
  
  tmp = 
    rawdata %>%
    group_by(Tag) %>%
    mutate(JourneyTime = difftime(Timestamp, lag(Timestamp, 1)))
    
  cat("Journey Time - elapsed time:", format(difftime(Sys.time(), start.time.1)), "\n", sep = " ")
  
  start.time.2 = Sys.time()
  
  tmp = 
    tmp %>%
    mutate(Trip = ifelse(JourneyTime > threshold | is.na(JourneyTime), TRUE, FALSE))
  
  cat("Trip (ifelse) - elapsed time:", format(difftime(Sys.time(), start.time.2)), "\n", sep = " ")
  
  start.time.3 = Sys.time()
  
  tmp = 
    tmp %>%
    mutate(Trip = cumsum(Trip))
  
 # print(head(tmp))
  
 # print(nrow(tmp))
  
  cat("Trip (cumsum) - elapsed time:", format(difftime(Sys.time(), start.time.3)), "\n", sep = " ")
  
  start.time.4 = Sys.time()
  
  tmp = 
    tmp %>%
    group_by(Tag, Trip) %>%
    mutate(isDuplicate = (CameraId == lag(CameraId))) %>%
    mutate(isDuplicate = ifelse(is.na(isDuplicate), FALSE, isDuplicate))  
    
  cat("Group, 2 x mutate isDuplicate - elapsed time:", format(difftime(Sys.time(), start.time.4)), "\n", sep = " ")
  
  start.time.5 = Sys.time()
  
  tmp = 
    tmp %>%
    filter(!isDuplicate) %>%
    select(-isDuplicate) %>%
    mutate(Length = n())
    
  cat("Filter duplicates and Length - elapsed time:", format(difftime(Sys.time(), start.time.5)), "\n", sep = " ")
  
  cat("Total elapsed time:", format(difftime(Sys.time(), start.time.1)), "\n", sep = " ")
  return(tmp)
}

cat("\nComputing trips ..", "\n")

# Trips
trips =
  np_data %>%
  as.trips(threshold = threshold)

cat("\nSaving trips ..", "\n")

# print(head(trips))

print(nrow(trips))

save(trips, file = paste("data/trips", threshold, "RData", sep="."))

cat("Total elapsed time:", format(difftime(Sys.time(), start.time)), "\n", sep = " ")

system("say This is the end, my friend!")