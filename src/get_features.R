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

load(file = paste("data/trips", threshold, "RData", sep ="."), envir = tmpenv)

attach(tmpenv)

print(head(trips))


as.features = function(rawdata, threshold) {
  # ---
  # ---
  # 1
  # ---
  # ---
  
  cat("\nComputing features (step 1 - mutate)", "\n")
  start.time.1 = Sys.time()
  
  features =
    rawdata %>%
    group_by(Tag, Trip) %>%
    mutate(Origin = CameraId[1]) %>%
    mutate(Destination = ifelse(Length > 1, CameraId[Length], NA)) %>%
    summarise(
      Day = lubridate::day(Timestamp[1]),
      Origin = Origin[1],
      Destination = Destination[1],
      StartTime = Timestamp[1],
      EndTime = Timestamp[Length[1]],
      Length = Length[1],
      Route = paste(CameraId, collapse = "-"),
      OD = paste(Origin[1], Destination[1], collapse = "-")
      )
  #unite("OD", c(Origin, Destination), remove = FALSE) %>%
  
  cat("Elapsed time:", format(difftime(Sys.time(), start.time.1)), "\n", sep = " ")
  
  # ---
  # ---
  # 2
  # ---
  # ---
  
  cat("\nComputing features (step 2 - mutate RestTime)", "\n")
  start.time.2 = Sys.time()
  
  features =
    features %>%
    group_by(Tag, Day) %>%
    mutate(RestTime = as.numeric(difftime(StartTime, lag(EndTime, 1), units = "hours")))
  
  cat("Elapsed time:", format(difftime(Sys.time(), start.time.2)), "\n", sep = " ")
  

  # ---
  # ---
  # 3
  # ---
  # ---
  
  cat("\nComputing features (step 3 - summarise by vehicle and day)", "\n")
  start.time.3 = Sys.time()
  
  features =
    features %>%
    summarise(
      Trips = n(),
      MedianLength = median(Length),
      Sightings = sum(Length),
      DistinctOrigins = n_distinct(Origin),
      DistinctDestinations = n_distinct(Destination, na.rm = TRUE),
      DistinctRoutes = n_distinct(Route),
      FirstTime = min(StartTime),
      LastTime = max(EndTime),
      #DistinctCameras = n_distinct(),
      TotalRestTime = sum(RestTime, na.rm = TRUE)
    )
  
  cat("Elapsed time:", format(difftime(Sys.time(), start.time.3)), "\n", sep = " ")
  
  # ---
  # ---
  # 4
  # ---
  # ---
  
  cat("\nComputing features (step 4 - summarise by vehicle)", "\n")
  start.time.4 = Sys.time()
  
  features =
    features %>%
    group_by(Tag) %>%
    summarise(
      TotalTrips = sum(Trips),
      AverageTrips = mean(Trips),
      AverageLength = mean(MedianLength),
      AverageSightings = mean(Sightings),
      AverageDistinctOrigins = mean(DistinctOrigins),
      AverageDistinctDestinations = mean(DistinctDestinations),
      AverageDistinctRoutes = mean(DistinctRoutes),
      AverageFirstHour = mean(lubridate::hour(FirstTime)),
      AverageLastHour = mean(lubridate::hour(LastTime)),
      AverageDifferenceTimeHours = mean(as.numeric(difftime(LastTime, FirstTime, units = "hours"))),
      AverageRestTimeHours = mean(TotalRestTime)
    )
  
  cat("Elapsed time:", format(difftime(Sys.time(), start.time.4)), "\n", sep = " ")
  
  return(features)
}

# ---
# ---
# END
# ---
# ---

save(features, file = paste("data/features", threshold, "RData", sep="."))

cat("Total elapsed time:", format(difftime(Sys.time(), start.time)), "\n", sep = " ")

system("say This is the end, my friend!")
