# Title     : OD travel time by public transit / car
# Objective : Compute travel time from each origin to each destination (per amenity) by hour
# Created by: Yuan Liao
# Updated   : 2025-08-16

# Start a fresh R session first!

# (A) Make sure nothing has started the JVM yet
# Set heap + GC options EARLY
Sys.unsetenv("_JAVA_OPTIONS")
Sys.unsetenv("JAVA_TOOL_OPTIONS")
Sys.setenv(RJAVA_JVM_ARGS = "-Xms48g -Xmx53g -XX:+UseG1GC -XX:+UseStringDeduplication")
options(java.parameters = c("-Xms48g","-Xmx53g","-XX:+UseG1GC","-XX:+UseStringDeduplication"))

# (B) Confirm 64-bit Java
system("java -version")     # Should say: 64-Bit Server VM

# (C) Start the JVM and query heap
library(rJava)              # loads JVM with the args above
rJava::.jinit()
rt <- rJava::.jcall("java/lang/Runtime","Ljava/lang/Runtime;","getRuntime")
maxB   <- rJava::.jcall(rt,"J","maxMemory")
totalB <- rJava::.jcall(rt,"J","totalMemory")
freeB  <- rJava::.jcall(rt,"J","freeMemory")
cat(sprintf("max=%.1f GB  total=%.1f GB  free=%.1f GB\n", maxB/2^30, totalB/2^30, freeB/2^30))

library(r5r)
library(data.table)
library(dplyr)

tp_path   <- "dbs/sp_accessibility_r"

data_path <- file.path(tp_path, "data")
r5r_core  <- setup_r5(data_path = data_path)

# ---------- helper: compute OD travel time matrix ----------
# mode: c("WALK","TRANSIT") or "CAR"
# max_dur_min: cap computations at 90 minutes (change as needed)
tt.process <- function(hour, mode = c("WALK","TRANSIT"), fn = "", max_dur_min = 15,
                       percentiles = 50L) {

  output_path <- file.path(tp_path, sprintf("tt_wk_%s_%02d_%02d.csv", fn, hour, max_dur_min))
  origins <- fread(file.path(data_path, paste0("origins_", max_dur_min, ".csv")))
  destinations <- fread(file.path(data_path, sprintf("destinations.csv")))

  # Expect columns: id, lon, lat in both origins/destinations
  # If names differ, rename here:
  # setnames(origins, c("origin_id","origin_lon","origin_lat"), c("id","lon","lat"))
  # setnames(destinations, c("dest_id","dest_lon","dest_lat"), c("id","lon","lat"))

  departure_datetime <- as.POSIXct(sprintf("07-07-2025 %02d:00:00", hour),
                                   format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

  # Compute OD travel times (in minutes). For TRANSIT, percentiles summarize schedule variability.
  tt <- travel_time_matrix(
    r5r_core           = r5r_core,
    origins            = origins,
    destinations       = destinations,
    mode               = mode,
    departure_datetime = departure_datetime,
    max_trip_duration  = max_dur_min,
    percentiles        = percentiles,   # e.g., 50 for median; or c(5,50,95)
    progress           = TRUE,
    verbose            = FALSE
  )

  # r5r returns 'travel_time_pXX' columns for each percentile (or 'travel_time' for CAR/no percentiles).
  # Drop unreachable pairs (NA travel times)
  tt <- tt %>% mutate(across(starts_with("travel_time"), as.numeric)) %>% 
               filter(if_any(starts_with("travel_time"), ~ !is.na(.)))

  fwrite(tt, file = output_path)
}

# ---------- Public transit (WALK + TRANSIT), 90-min cap ----------

hr <- 17
dur_list <- c(15, 30, 45, 60, 75, 90)
message(sprintf("PT: hour=%02d", hr))
situation <- 2
for (max_dur_min in c(15, 30, 45, 60, 75, 90)) {
  message(sprintf("Processing situation %d with max_dur_min=%d", situation, max_dur_min))
  tt.process(hour    = hr,
              fn      = "pt",
              mode    = c("WALK", "TRANSIT"),
              max_dur_min = max_dur_min,
              percentiles = 50L)  # keep multiple percentiles if useful
}


# ---------- Car, 90-min cap ----------
hr <- 17
situation <- 2
message(sprintf("CAR: hour=%02d", hr))
for (max_dur_min in dur_list) {
  message(sprintf("Processing situation %d with max_dur_min=%d", situation, max_dur_min))
  tt.process(hour    = hr,
              fn      = "car",
              mode    = "CAR",
              max_dur_min = max_dur_min,
              percentiles = 50L)  # keep multiple percentiles if useful
}