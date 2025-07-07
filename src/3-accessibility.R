# Title     : Accessibility to various destinations by public transit
# Objective : Calculate PT access to various destinations by county
# Created by: Yuan Liao
# Created on: 2025-07-01

options(java.parameters = "-Xmx52G")

library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(interp)
library(dplyr)

# system.file returns the directory with example data inside the r5r package
# set data path to directory containing your own data if not using the examples
tp_path <- "dbs/accessibility" # system.file("extdata/poa", package = "r5r")
amenities <- c('sl', 'en', 'hs', 'ed')

# Indicate the path where OSM and GTFS data are stored
data_path <- paste0(tp_path, "/data")
r5r_core <- setup_r5(data_path = data_path)

ct.process <- function(amenity, hour, mode=c("WALK", "TRANSIT"), fn='', time_threshold=15) {
  output_path <- paste0(tp_path, "/access_", time_threshold, "_", amenity, "_", fn, "_", hour, ".csv")
  origins <- fread(file.path(data_path, paste0("origins_", hour, ".csv")))
  destinations <- fread(file.path(data_path, paste0("destinations_", amenity, ".csv")))
  # set departure datetime input
  departure_datetime <- as.POSIXct(paste0("07-07-2025 ", hour, ":00:00"),
                                   format = "%d-%m-%Y %H:%M:%S")
  # calculate accessibility
  access <- accessibility(r5r_core = r5r_core,
                          origins = origins,
                          destinations = destinations,
                          opportunities_colnames = "job", # This is constantly 1
                          mode = mode,
                          departure_datetime = departure_datetime,
                          decay_function = "step",
                          cutoffs = time_threshold,
                          max_trip_duration = time_threshold,
                          verbose = FALSE,
                          progress = TRUE)
  data.table::fwrite(access, file = output_path, row.names = FALSE)
}

# pt access
for (amenity in amenities){
  for (hr in seq(0, 23)) {
    print(paste("Processing amenity:", amenity, "at hour:", hr))
    ct.process(amenity = amenity, hour = hr, fn ='pt', mode = c("WALK", "TRANSIT"), time_threshold = 30)
  }
}

# car access
mode <- "CAR"
for (amenity in amenities){
  for (hr in seq(0, 23)) {
    print(paste("Processing amenity:", amenity, "at hour:", hr))
    ct.process(amenity = amenity, hour = hr, fn ='car', mode = mode, time_threshold = 30)
  }
}