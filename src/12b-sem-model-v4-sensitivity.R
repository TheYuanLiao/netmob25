# ============================================================================
# SEM Model Sensitivity Analysis
# ============================================================================
#
# Runs the v4 SEM model with different STA parameter settings:
#   - Sensitivity 1: 60 min budget (restrictive)
#   - Sensitivity 2: 120 min budget (generous)
#
# Note: Car accessibility is time-invariant (departure hour ignored).
#       PT budget variations only available at hour=17.
#
# Output: results/sem_sensitivity/{setting}/
#
# ============================================================================

library(lavaan)
library(dplyr)

# ============================================================================
# CONFIGURATION
# ============================================================================

config <- list(
  base_features_file = "dbs/data_p/commuter_model_features_r.csv",
  sensitivity_file = "results/sensitivity/sta_sensitivity_full.csv",
  output_dir = "results/sem_sensitivity",

  outcome_y1 = "hill_q1",
  outcome_y2 = "mean_leisure_duration",
  mediator_var = "total_travel_time",
  weight_var = "weight_ind",
  exclude_household_types = c(5, 7)
)

# Sensitivity settings to test
sensitivity_settings <- list(
  list(name = "budget_60min", budget = 60, description = "Restrictive: 60 min budget"),
  list(name = "budget_120min", budget = 120, description = "Generous: 120 min budget")
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

run_sem_sensitivity <- function(setting, config) {

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Running:", setting$description, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  output_path <- file.path(config$output_dir, setting$name)
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

  log_file <- file.path(output_path, "analysis_log.txt")
  log_msg <- function(msg) {
    timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
    full_msg <- paste(timestamp, msg)
    cat(full_msg, "\n")
    cat(full_msg, "\n", file = log_file, append = TRUE)
  }

  log_msg(paste("Setting:", setting$name))
  log_msg(paste("Budget:", setting$budget, "min"))

  # --------------------------------------------------------------------------
  # 1. LOAD AND MERGE DATA
  # --------------------------------------------------------------------------

  log_msg("Loading data...")

  # Load base features (without STA)
  d_base <- read.csv(config$base_features_file, stringsAsFactors = FALSE)
  log_msg(paste("Base features:", nrow(d_base), "rows"))

  # Load sensitivity STA values
  d_sens <- read.csv(config$sensitivity_file, stringsAsFactors = FALSE)

  # Filter to desired budget and hour=17 (only hour with all budgets)
  d_sens_filtered <- d_sens %>%
    filter(budget == setting$budget, hour == 17) %>%
    select(ID, ak, mode) %>%
    rename(ak_sens = ak, mode_sens = mode)

  log_msg(paste("STA records for budget=", setting$budget, ":", nrow(d_sens_filtered)))

  # Map mode labels in base data
  d_base$mode_sens <- ifelse(d_base$mode == "Car", "car", "pt")

  # Merge STA values
  d <- merge(d_base, d_sens_filtered, by = c("ID", "mode_sens"), all.x = TRUE)

  log_msg(paste("After merge:", nrow(d), "rows"))

  # Fill missing with 0 and apply IHS transformation
  d$ak_sens[is.na(d$ak_sens)] <- 0
  d$ak_ihs <- asinh(d$ak_sens)

  log_msg(paste("Merged data:", nrow(d), "rows"))
  log_msg(paste("Non-zero STA:", sum(d$ak_sens > 0), "(",
                round(100 * mean(d$ak_sens > 0), 1), "%)"))

  # --------------------------------------------------------------------------
  # 2. PREPARE VARIABLES
  # --------------------------------------------------------------------------

  d <- d %>% filter(!(Household_type %in% config$exclude_household_types))
  log_msg(paste("After household filter:", nrow(d), "rows"))

  d$female <- ifelse(d$Gender == "Woman", 1, 0)
  d$edu_high <- ifelse(d$Education %in% c(5, 9), 1, 0)
  d$mode_pt <- ifelse(d$mode == "Public transit", 1, 0)
  d$pt_sub <- ifelse(tolower(as.character(d$pt_sub)) %in% c("true", "t", "yes", "y", "1"), 1, 0)
  d$active_mode <- as.numeric(d$active_mode)
  d$hh_6 <- ifelse(d$Household_type == 6, 1, 0)

  # --------------------------------------------------------------------------
  # 3. CREATE ANALYSIS DATASET
  # --------------------------------------------------------------------------

  sta_var <- "ak_ihs"
  mediator_var <- config$mediator_var
  outcome_y1 <- config$outcome_y1
  outcome_y2 <- config$outcome_y2

  socio_vars <- c("female", "edu_high", "poverty_rate", "hh_6")
  transport_vars <- c("mode_pt", "pt_sub", "active_mode")
  continuous_vars <- c(sta_var, mediator_var, outcome_y1, outcome_y2, "poverty_rate")

  all_vars <- c(sta_var, mediator_var, outcome_y1, outcome_y2,
                socio_vars, transport_vars, config$weight_var)
  d_analysis <- d[, all_vars, drop = FALSE]

  complete_idx <- complete.cases(d_analysis)
  d_analysis <- d_analysis[complete_idx, ]
  d_analysis <- d_analysis[d_analysis[[config$weight_var]] > 0, ]
  log_msg(paste("Complete cases:", nrow(d_analysis)))

  # --------------------------------------------------------------------------
  # 4. STANDARDIZE CONTINUOUS VARIABLES
  # --------------------------------------------------------------------------

  log_msg("Standardizing...")

  scaling_params <- data.frame(variable = character(), mean = numeric(), sd = numeric())

  for (v in continuous_vars) {
    m <- mean(d_analysis[[v]], na.rm = TRUE)
    s <- sd(d_analysis[[v]], na.rm = TRUE)
    d_analysis[[v]] <- (d_analysis[[v]] - m) / s
    scaling_params <- rbind(scaling_params, data.frame(variable = v, mean = m, sd = s))
  }

  write.csv(scaling_params, file.path(output_path, "scaling_parameters.csv"), row.names = FALSE)

  # --------------------------------------------------------------------------
  # 5. SEM MODEL
  # --------------------------------------------------------------------------

  log_msg("Building SEM syntax...")

  socio_sta <- c("female", "edu_high", "poverty_rate")
  socio_tt <- c("female", "edu_high", "poverty_rate")
  socio_lp <- c("female", "edu_high", "poverty_rate", "hh_6")

  make_rhs <- function(vars, prefix = "") {
    if (length(vars) == 0) return("")
    paste(paste0(prefix, vars, "*", vars), collapse = " + ")
  }

  syntax_lines <- c(
    "# Measurement model",
    paste0("leisure_part =~ 1*", outcome_y1, " + lam2*", outcome_y2),
    "",
    "# Structural model",
    paste(sta_var, "~", make_rhs(socio_sta, "s_"), "+", make_rhs(transport_vars, "t_")),
    paste(mediator_var, "~", paste0("a*", sta_var), "+", make_rhs(socio_tt, "g_"), "+", make_rhs(transport_vars, "m_")),
    paste("leisure_part ~", paste0("c*", sta_var), "+", paste0("b*", mediator_var), "+",
          make_rhs(socio_lp, "d_"), "+", make_rhs(transport_vars, "r_")),
    "",
    "# Effect decomposition",
    "indirect := a * b",
    "direct := c",
    "total := c + a * b",
    "",
    "# Effects on indicators",
    "direct_div := c * 1",
    "direct_dur := c * lam2",
    "indirect_div := a * b * 1",
    "indirect_dur := a * b * lam2",
    "total_div := (c + a * b) * 1",
    "total_dur := (c + a * b) * lam2"
  )

  sem_syntax <- paste(syntax_lines, collapse = "\n")
  writeLines(sem_syntax, file.path(output_path, "sem_syntax.txt"))

  # --------------------------------------------------------------------------
  # 6. FIT MODEL
  # --------------------------------------------------------------------------

  log_msg("Fitting SEM model...")

  fit <- tryCatch({
    sem(
      model = sem_syntax,
      data = d_analysis,
      estimator = "DWLS",
      sampling.weights = config$weight_var,
      std.lv = FALSE
    )
  }, error = function(e) {
    log_msg(paste("ERROR fitting model:", e$message))
    NULL
  })

  if (is.null(fit)) {
    log_msg("Model fitting failed.")
    return(NULL)
  }

  log_msg("Model fitted successfully")

  if (!lavInspect(fit, "converged")) {
    log_msg("WARNING: Model did not converge!")
  }

  # --------------------------------------------------------------------------
  # 7. EXTRACT RESULTS
  # --------------------------------------------------------------------------

  log_msg("Extracting results...")

  summary_output <- capture.output(
    summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
  )
  writeLines(summary_output, file.path(output_path, "model_summary.txt"))

  pe <- parameterEstimates(fit, standardized = TRUE)
  write.csv(pe, file.path(output_path, "parameter_estimates.csv"), row.names = FALSE)

  fit_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
  fit_values <- fitMeasures(fit, fit_indices)
  fit_df <- data.frame(measure = names(fit_values), value = as.numeric(fit_values))
  write.csv(fit_df, file.path(output_path, "fit_measures.csv"), row.names = FALSE)

  log_msg("=== FIT MEASURES ===")
  for (i in seq_len(nrow(fit_df))) {
    log_msg(paste(" ", fit_df$measure[i], "=", round(fit_df$value[i], 4)))
  }

  rsq <- lavInspect(fit, "rsquare")
  rsq_df <- data.frame(variable = names(rsq), rsquare = as.numeric(rsq))
  write.csv(rsq_df, file.path(output_path, "rsquare.csv"), row.names = FALSE)

  saveRDS(fit, file.path(output_path, "model_fit.rds"))

  # --------------------------------------------------------------------------
  # 8. KEY RESULTS
  # --------------------------------------------------------------------------

  log_msg("=== KEY PATHWAY RESULTS ===")

  key_labels <- c("a", "b", "c", "indirect", "direct", "total",
                  "direct_div", "direct_dur", "indirect_div", "indirect_dur",
                  "total_div", "total_dur")
  key_effects <- subset(pe, label %in% key_labels)
  key_effects <- key_effects[match(key_labels, key_effects$label), ]

  for (i in seq_len(nrow(key_effects))) {
    row <- key_effects[i, ]
    sig <- ifelse(row$pvalue < 0.001, "***", ifelse(row$pvalue < 0.01, "**",
             ifelse(row$pvalue < 0.05, "*", "")))
    log_msg(sprintf("  %s: %.3f (SE=%.3f, p=%.4f, std=%.3f) %s",
                    row$label, row$est, row$se, row$pvalue, row$std.all, sig))
  }

  write.csv(key_effects[, c("label", "est", "se", "pvalue", "std.all")],
            file.path(output_path, "key_results.csv"), row.names = FALSE)

  log_msg(paste("Results saved to:", output_path))

  return(list(
    setting = setting$name,
    fit = fit,
    key_effects = key_effects,
    fit_measures = fit_df
  ))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SEM SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

for (setting in sensitivity_settings) {
  result <- run_sem_sensitivity(setting, config)
  if (!is.null(result)) {
    results[[setting$name]] <- result
  }
}

# ============================================================================
# COMPARISON SUMMARY
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SENSITIVITY COMPARISON SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Compare key effects across settings
comparison <- data.frame()

for (name in names(results)) {
  res <- results[[name]]
  ke <- res$key_effects

  for (label in c("direct", "indirect", "total")) {
    row <- ke[ke$label == label, ]
    comparison <- rbind(comparison, data.frame(
      setting = name,
      effect = label,
      estimate = row$est,
      std = row$std.all,
      pvalue = row$pvalue
    ))
  }

  # Add fit measures
  fm <- res$fit_measures
  cfi <- fm$value[fm$measure == "cfi"]
  rmsea <- fm$value[fm$measure == "rmsea"]
  cat(sprintf("%s: CFI=%.3f, RMSEA=%.3f\n", name, cfi, rmsea))
}

cat("\nEffect comparison:\n")
print(comparison)

write.csv(comparison, file.path(config$output_dir, "sensitivity_comparison.csv"), row.names = FALSE)

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("Results saved to:", config$output_dir, "\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
