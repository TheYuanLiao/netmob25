# Title     : OD travel time from Work to Leisure POIs (Refactored)
# Objective : Compute travel times with binned max_dur for efficiency
# Created by: Yuan Liao
# Refactored: 2026-04-18
#
# Changes from 4-sp-accessibility-wk.R:
# - Uses binned max_dur based on individual's remaining time budget
# - Bins: 30, 60, 90, 120 min (reduces computation for short-budget individuals)
# - Parameterizes departure hour for sensitivity analysis (16, 17, 18)
# - Filtering by time budget done in post-processing (notebook 7b)

# ============================================================================
# CONFIGURATION
# ============================================================================

config <- list(
  # Directories
  data_path = "dbs/sp_accessibility_v2/data",
  output_path = "dbs/sp_accessibility_v2",

  # Max budget for sensitivity analysis
  max_budget = 120,

  # Bins for max_dur (based on remaining time after commute)
  # Individuals are grouped by their (max_budget - time_hw * 2) value
  dur_bins = c(30, 60, 90, 120),

  # Departure hours for sensitivity analysis
  # Car: only 17 (less sensitive to time); PT: 16, 17, 18
  departure_hours_pt = c(16, 17, 18),
  departure_hours_car = c(17),

  # Modes
  modes = list(
    pt = c("WALK", "TRANSIT"),
    car = "CAR"
  ),

  # Chunking for large computations
  chunk_size = 500
)

# ============================================================================
# SETUP
# ============================================================================

# JVM settings for r5r (must be set before loading rJava)
Sys.unsetenv("_JAVA_OPTIONS")
Sys.unsetenv("JAVA_TOOL_OPTIONS")
Sys.setenv(RJAVA_JVM_ARGS = "-Xms48g -Xmx53g -XX:+UseG1GC -XX:+UseStringDeduplication")
options(java.parameters = c("-Xms48g", "-Xmx53g", "-XX:+UseG1GC", "-XX:+UseStringDeduplication"))

# Confirm 64-bit Java
system("java -version")

# Load packages
library(rJava)
rJava::.jinit()
library(r5r)
library(data.table)
library(dplyr)

# Report memory
rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxB <- rJava::.jcall(rt, "J", "maxMemory")
cat(sprintf("JVM max memory: %.1f GB\n", maxB / 2^30))

# Setup r5r
r5r_core <- setup_r5(data_path = config$data_path)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

compute_travel_times <- function(r5r_core, origins, destinations, mode, hour,
                                  max_dur, chunk_size = NULL) {
  #' Compute travel time matrix, optionally in chunks
  #' Returns data.table (does not save to file)

  departure_datetime <- as.POSIXct(
    sprintf("07-07-2025 %02d:00:00", hour),
    format = "%d-%m-%Y %H:%M:%S",
    tz = "UTC"
  )

  if (is.null(chunk_size) || nrow(origins) <= chunk_size) {
    message(sprintf("  Computing %d origins x %d destinations (max_dur=%d)...",
                    nrow(origins), nrow(destinations), max_dur))

    tt <- travel_time_matrix(
      r5r_core = r5r_core,
      origins = origins,
      destinations = destinations,
      mode = mode,
      departure_datetime = departure_datetime,
      max_trip_duration = max_dur,
      percentiles = 50L,
      progress = TRUE,
      verbose = FALSE
    )
  } else {
    n_chunks <- ceiling(nrow(origins) / chunk_size)
    message(sprintf("  Processing %d origins in %d chunks (max_dur=%d)...",
                    nrow(origins), n_chunks, max_dur))

    tt_list <- list()
    for (i in seq_len(n_chunks)) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, nrow(origins))

      message(sprintf("    Chunk %d/%d (origins %d-%d)...",
                      i, n_chunks, start_idx, end_idx))

      origins_chunk <- origins[start_idx:end_idx, ]

      tt_chunk <- travel_time_matrix(
        r5r_core = r5r_core,
        origins = origins_chunk,
        destinations = destinations,
        mode = mode,
        departure_datetime = departure_datetime,
        max_trip_duration = max_dur,
        percentiles = 50L,
        progress = TRUE,
        verbose = FALSE
      )

      tt_list[[i]] <- tt_chunk
      gc(full = TRUE)
    }

    tt <- rbindlist(tt_list)
  }

  # Process results
  tt <- tt %>%
    mutate(across(starts_with("travel_time"), as.numeric)) %>%
    filter(if_any(starts_with("travel_time"), ~ !is.na(.)))

  return(tt)
}

assign_dur_bin <- function(remaining_time, bins) {
  #' Assign each individual to a duration bin based on remaining time
  #' Returns the bin value (max_dur to use)
  if (is.na(remaining_time) || remaining_time <= 0) {
    return(max(bins))  # Default to max for NA or negative
  }
  for (b in bins) {
    if (remaining_time <= b) return(b)
  }
  return(max(bins))
}

# ============================================================================
# LOAD DATA
# ============================================================================

# Load origins (all work locations)
origins_file <- file.path(config$data_path, "origins_work.csv")
if (!file.exists(origins_file)) {
  stop("Origins file not found. Run notebook 3b first: ", origins_file)
}
origins <- fread(origins_file)
message(sprintf("Loaded %d work locations as origins", nrow(origins)))

# Load destinations (leisure POIs)
destinations_file <- file.path(config$data_path, "destinations_leisure.csv")
if (!file.exists(destinations_file)) {
  stop("Destinations file not found. Run notebook 3b first: ", destinations_file)
}
destinations <- fread(destinations_file)
message(sprintf("Loaded %d leisure POIs as destinations", nrow(destinations)))

# Load time budget to get commute times
budget_file <- file.path(config$data_path, "time_budget.csv")
if (!file.exists(budget_file)) {
  stop("Time budget file not found. Run notebook 3b first: ", budget_file)
}
df_budget <- fread(budget_file)
message(sprintf("Loaded time budget for %d individuals", nrow(df_budget)))

# Compute remaining time and assign bins
df_budget[, remaining_time := config$max_budget - time_hw * 2]
df_budget[, dur_bin := sapply(remaining_time, assign_dur_bin, bins = config$dur_bins)]

# Exclude individuals with NA time_hw (no commute data = STA will be zero)
df_budget_valid <- df_budget[!is.na(time_hw) & remaining_time > 0]
n_excluded <- nrow(df_budget) - nrow(df_budget_valid)
message(sprintf("Excluded %d individuals with NA time_hw or non-positive remaining time", n_excluded))

# Check for is_car column (for mode-specific filtering)
if ("is_car" %in% names(df_budget_valid)) {
  message(sprintf("Mode distribution: %d car users, %d non-car users",
                  sum(df_budget_valid$is_car == 1), sum(df_budget_valid$is_car == 0)))
} else {
  message("Warning: is_car column not found. Will compute STA for all individuals for all modes.")
  df_budget_valid[, is_car := NA]
}

# Summary of bins
message("\nDuration bin distribution (valid individuals):")
print(table(df_budget_valid$dur_bin, useNA = "ifany"))
message(sprintf("Remaining time range: %.1f to %.1f",
                min(df_budget_valid$remaining_time, na.rm = TRUE),
                max(df_budget_valid$remaining_time, na.rm = TRUE)))

# Create ID-to-bin mapping (only valid individuals)
id_bin_map <- df_budget_valid[, .(id = ID, dur_bin, is_car)]
setkey(id_bin_map, id)

# Merge bin info with origins using data.table join
setkey(origins, id)
origins_all <- id_bin_map[origins, nomatch = NULL]  # Exclude non-matching (NA) individuals

message(sprintf("Origins after filtering: %d (excluded %d with no valid time budget)",
                nrow(origins_all), nrow(fread(origins_file)) - nrow(origins_all)))

# ============================================================================
# MAIN: Compute Work -> Leisure POI travel times (binned approach)
# ============================================================================

for (mode_name in names(config$modes)) {
  mode_spec <- config$modes[[mode_name]]

  # Filter origins by mode: car users for CAR, non-car users for PT
  if (!is.na(origins_all$is_car[1])) {
    if (mode_name == "car") {
      origins <- origins_all[is_car == 1]
      message(sprintf("\nFiltered to %d car users for CAR mode", nrow(origins)))
    } else {
      origins <- origins_all[is_car == 0]
      message(sprintf("\nFiltered to %d non-car users for PT mode", nrow(origins)))
    }
  } else {
    origins <- origins_all
    message("\nNo mode filtering (is_car not available)")
  }

  # Get mode-specific departure hours
  departure_hours <- if (mode_name == "car") config$departure_hours_car else config$departure_hours_pt

  for (hour in departure_hours) {
    message(sprintf("\n=== Mode: %s, Hour: %02d ===", toupper(mode_name), hour))

    output_file <- file.path(
      config$output_path,
      sprintf("tt_wk_%s_%02d.csv", mode_name, hour)
    )

    # Skip if already computed
    if (file.exists(output_file)) {
      message(sprintf("Output exists, skipping: %s", output_file))
      next
    }

    # Process each bin separately
    tt_all <- list()

    for (bin in config$dur_bins) {
      origins_bin <- origins[dur_bin == bin]

      if (nrow(origins_bin) == 0) {
        message(sprintf("  Bin %d: No origins, skipping", bin))
        next
      }

      message(sprintf("\n  Bin %d min: %d origins", bin, nrow(origins_bin)))

      tt_bin <- compute_travel_times(
        r5r_core = r5r_core,
        origins = origins_bin[, .(id, lon, lat)],
        destinations = destinations,
        mode = mode_spec,
        hour = hour,
        max_dur = bin,
        chunk_size = config$chunk_size
      )

      if (nrow(tt_bin) > 0) {
        tt_all[[as.character(bin)]] <- tt_bin
        message(sprintf("    -> %d OD pairs", nrow(tt_bin)))
      }

      gc(full = TRUE)
    }

    # Combine all bins
    if (length(tt_all) > 0) {
      tt_combined <- rbindlist(tt_all)
      fwrite(tt_combined, file = output_file)
      message(sprintf("\nSaved %d total rows to %s", nrow(tt_combined), output_file))
    } else {
      message("Warning: No results to save")
    }

    gc(full = TRUE)
    Sys.sleep(1)
  }
}

message("\n=== Work -> POI travel times complete ===")
message(sprintf("Output directory: %s", config$output_path))
message("Next step: Run 5b-sp-accessibility-kh-prep.ipynb")
