# Title     : OD travel time from Leisure POIs to Home (Simple)
# Objective : Compute all POIs to all homes with max_dur=120
# Created by: Yuan Liao
# Created: 2026-04-18
#
# Simple approach: one r5r call per mode-hour, filter in post-processing

# ============================================================================
# CONFIGURATION
# ============================================================================

config <- list(
  data_path = "dbs/sp_accessibility_v2/data",
  output_path = "dbs/sp_accessibility_v2",

  # Departure hours: Car only 17, PT: 16, 17, 18
  departure_hours_pt = c(16, 17, 18),
  departure_hours_car = c(17),

  max_trip_duration = 120,

  modes = list(
    pt = c("WALK", "TRANSIT"),
    car = "CAR"
  ),

  # Chunk POIs if memory is an issue (NULL = no chunking)
  poi_chunk_size = 10000
)

# ============================================================================
# SETUP
# ============================================================================

Sys.unsetenv("_JAVA_OPTIONS")
Sys.unsetenv("JAVA_TOOL_OPTIONS")
Sys.setenv(RJAVA_JVM_ARGS = "-Xms48g -Xmx53g -XX:+UseG1GC -XX:+UseStringDeduplication")
options(java.parameters = c("-Xms48g", "-Xmx53g", "-XX:+UseG1GC", "-XX:+UseStringDeduplication"))

library(rJava)
rJava::.jinit()
library(r5r)
library(data.table)

rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
cat(sprintf("JVM max memory: %.1f GB\n", rJava::.jcall(rt, "J", "maxMemory") / 2^30))

r5r_core <- setup_r5(data_path = config$data_path)

# ============================================================================
# LOAD DATA
# ============================================================================

df_pois <- fread(file.path(config$data_path, "destinations_leisure.csv"))
df_homes <- fread(file.path(config$data_path, "destinations_home.csv"))
df_budget <- fread(file.path(config$data_path, "time_budget.csv"))

message(sprintf("POIs: %d, Homes: %d", nrow(df_pois), nrow(df_homes)))

# Filter homes to valid individuals (has time_hw, positive remaining time)
df_budget <- df_budget[!is.na(time_hw) & (config$max_trip_duration - time_hw * 2) > 0]
message(sprintf("Valid individuals: %d", nrow(df_budget)))

if ("is_car" %in% names(df_budget)) {
  message(sprintf("  Car users: %d, Non-car: %d",
                  sum(df_budget$is_car == 1), sum(df_budget$is_car == 0)))
}

# ============================================================================
# MAIN
# ============================================================================

for (mode_name in names(config$modes)) {
  mode_spec <- config$modes[[mode_name]]
  departure_hours <- if (mode_name == "car") config$departure_hours_car else config$departure_hours_pt

  # Filter homes by mode
  if ("is_car" %in% names(df_budget)) {
    if (mode_name == "car") {
      valid_ids <- df_budget[is_car == 1]$ID
    } else {
      valid_ids <- df_budget[is_car == 0]$ID
    }
    homes <- df_homes[id %in% valid_ids]
  } else {
    homes <- df_homes[id %in% df_budget$ID]
  }

  message(sprintf("\n%s mode: %d homes", toupper(mode_name), nrow(homes)))

  for (hour in departure_hours) {
    key <- sprintf("%s_%02d", mode_name, hour)
    output_file <- file.path(config$output_path, sprintf("tt_kh_%s.csv", key))

    if (file.exists(output_file)) {
      message(sprintf("  %s: exists, skipping", key))
      next
    }

    message(sprintf("\n=== %s ===", key))

    departure_datetime <- as.POSIXct(
      sprintf("07-07-2025 %02d:00:00", hour),
      format = "%d-%m-%Y %H:%M:%S",
      tz = "UTC"
    )

    # Compute (with optional chunking)
    if (is.null(config$poi_chunk_size) || nrow(df_pois) <= config$poi_chunk_size) {
      message(sprintf("Computing %d POIs x %d homes...", nrow(df_pois), nrow(homes)))

      tt <- travel_time_matrix(
        r5r_core = r5r_core,
        origins = df_pois,
        destinations = homes,
        mode = mode_spec,
        departure_datetime = departure_datetime,
        max_trip_duration = config$max_trip_duration,
        percentiles = 50L,
        progress = TRUE
      )
    } else {
      n_chunks <- ceiling(nrow(df_pois) / config$poi_chunk_size)
      message(sprintf("Computing in %d chunks...", n_chunks))

      tt_list <- list()
      for (i in seq_len(n_chunks)) {
        start_idx <- (i - 1) * config$poi_chunk_size + 1
        end_idx <- min(i * config$poi_chunk_size, nrow(df_pois))

        message(sprintf("  Chunk %d/%d: POIs %d-%d", i, n_chunks, start_idx, end_idx))

        tt_chunk <- travel_time_matrix(
          r5r_core = r5r_core,
          origins = df_pois[start_idx:end_idx],
          destinations = homes,
          mode = mode_spec,
          departure_datetime = departure_datetime,
          max_trip_duration = config$max_trip_duration,
          percentiles = 50L,
          progress = TRUE
        )

        tt_list[[i]] <- tt_chunk
        gc()
      }

      tt <- rbindlist(tt_list)
    }

    # Remove NAs and save
    setDT(tt)
    tt <- tt[!is.na(travel_time_p50)]

    fwrite(tt, output_file)
    message(sprintf("Saved %d rows to %s", nrow(tt), output_file))

    gc()
  }
}

message("\n=== Complete ===")
