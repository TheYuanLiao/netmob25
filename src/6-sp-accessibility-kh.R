# Title     : OD travel time by public transit / car
# Objective : Compute travel time from each origin to each destination (per amenity) by hour
# Created by: Yuan Liao
# Updated   : 2025-08-16

# Start a fresh R session first!

# (A) Make sure nothing has started the JVM yet
Sys.unsetenv("_JAVA_OPTIONS")
Sys.unsetenv("JAVA_TOOL_OPTIONS")
Sys.setenv(RJAVA_JVM_ARGS = "-Xms48g -Xmx53g -XX:+UseG1GC -XX:+UseStringDeduplication")
options(java.parameters = c("-Xms48g","-Xmx53g","-XX:+UseG1GC","-XX:+UseStringDeduplication"))

# (B) Confirm 64-bit Java
system("java -version")     # Should say: 64-Bit Server VM

# (C) Start the JVM and query heap
library(rJava)
rJava::.jinit()
rt <- rJava::.jcall("java/lang/Runtime","Ljava/lang/Runtime;","getRuntime")
maxB   <- rJava::.jcall(rt,"J","maxMemory")
totalB <- rJava::.jcall(rt,"J","totalMemory")
freeB  <- rJava::.jcall(rt,"J","freeMemory")
cat(sprintf("max=%.1f GB  total=%.1f GB  free=%.1f GB\n", maxB/2^30, totalB/2^30, freeB/2^30))

library(r5r)
library(data.table)
library(dplyr)
library(magrittr)   # for %>%

tp_path   <- "dbs/sp_accessibility_r"
data_path <- file.path(tp_path, "data")
r5r_core  <- setup_r5(data_path = data_path)

clean_after_run <- function(objects_to_rm = c("origins","destinations","tt"),
                            force_sleep = 0.5) {
  for (nm in objects_to_rm) {
    if (exists(nm, inherits = FALSE)) rm(list = nm, envir = parent.frame())
  }
  invisible(gc(full = TRUE, reset = TRUE))

  try(rJava::.jcall("java/lang/System", "V", "gc"), silent = TRUE)
  try(rJava::.jcall("java/lang/System", "V", "runFinalization"), silent = TRUE)
  try(rJava::.jcall("java/lang/System", "V", "gc"), silent = TRUE)

  if (!is.null(force_sleep) && force_sleep > 0) Sys.sleep(force_sleep)
}

# ---------- helper: compute OD travel time matrix ----------
tt.process <- function(hour,
                       mode = c("WALK","TRANSIT"),
                       fn = "",
                       max_dur_min = 15,
                       percentiles = 50L,
                       part = NULL) {

  if (is.null(part)) {
    origins_file <- file.path(data_path, sprintf("origins_%s_%d.csv", fn, max_dur_min))
    out_file     <- file.path(tp_path, sprintf("tt_kh_%s_%02d_%02d.csv", fn, hour, max_dur_min))
  } else {
    origins_file <- file.path(data_path, sprintf("origins_%s_%d_part%d.csv", fn, max_dur_min, part))
    out_file     <- file.path(tp_path, sprintf("tt_kh_%s_%02d_%02d_part%d.csv", fn, hour, max_dur_min, part))
  }

  destinations_file <- file.path(data_path, sprintf("destinations_%s.csv", fn))

  if (!file.exists(origins_file)) stop("Origins file not found: ", origins_file)
  if (!file.exists(destinations_file)) stop("Destinations file not found: ", destinations_file)

  origins      <- data.table::fread(origins_file)
  destinations <- data.table::fread(destinations_file)

  departure_datetime <- as.POSIXct(sprintf("07-07-2025 %02d:00:00", hour),
                                   format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

  tt <- r5r::travel_time_matrix(
    r5r_core           = r5r_core,
    origins            = origins,
    destinations       = destinations,
    mode               = mode,
    departure_datetime = departure_datetime,
    max_trip_duration  = max_dur_min,
    percentiles        = percentiles,
    progress           = TRUE,
    verbose            = FALSE
  )

  # Convert travel_time* cols to numeric
  time_cols <- grep("^travel_time", names(tt), value = TRUE)
  for (col in time_cols) tt[[col]] <- as.numeric(tt[[col]])

  # Keep rows with at least one non-NA travel_time
  tt <- tt[rowSums(!is.na(tt[, ..time_cols])) > 0, ]

  data.table::fwrite(tt, file = out_file)
  clean_after_run(objects_to_rm = c("origins", "destinations", "tt"))
}

run_for_all_parts <- function(hour, fn, max_dur_min, mode, percentiles = 50) {
  pat <- sprintf("^origins_%s_%d_part(\\d+)\\.csv$", fn, max_dur_min)
  files <- list.files(data_path, pattern = pat)
  parts <- sort(as.integer(sub(pat, "\\1", files)))

  if (length(parts) == 0) {
    tt.process(hour = hour, fn = fn, mode = mode,
               max_dur_min = max_dur_min, percentiles = percentiles, part = NULL)
  } else {
    for (p in parts) {
      message(sprintf("Running %s max=%d part=%d", fn, max_dur_min, p))
      tt.process(hour = hour, fn = fn, mode = mode,
                 max_dur_min = max_dur_min, percentiles = percentiles, part = p)
    }
  }
}

# ---------- Public transit (WALK + TRANSIT), 90-min cap ----------

hr <- 17
#for (situation in 1:3) {
for (max_dur_min in c(15, 30, 45, 60, 75, 90)) {
  run_for_all_parts(hour = hr,
                    fn   = "pt",
                    max_dur_min = max_dur_min,
                    mode = c("WALK", "TRANSIT"),
                    percentiles = 50)
}

# ---------- Car, 90-min cap ----------
hr <- 17
#for (situation in 1:3) {
for (max_dur_min in c(15, 30, 45, 60, 75, 90)) {
  run_for_all_parts(hour = hr,
                    fn   = "car",
                    max_dur_min = max_dur_min,
                    mode = "CAR",
                    percentiles = 50)
}
#}