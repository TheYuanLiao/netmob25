# ============================================================================
# SEM Model for Space-Time Accessibility and Leisure Participation
# Version 4.0 - Latent Variable Approach
# ============================================================================
#
# Key change: Instead of diversity -> duration path, both are indicators
# of a latent "leisure_participation" construct. This avoids imposing
# causal ordering between two parallel outcomes.
#
# Model structure:
#   1. STA ~ socio + transport
#   2. Travel time ~ STA + socio + transport
#   3. leisure_participation =~ hill_q1 + mean_leisure_duration (LATENT)
#   4. leisure_participation ~ STA + travel_time + socio + transport
#
# Visualization: Run 13-sem-plot.R separately after this script
#
# ============================================================================

library(lavaan)
library(dplyr)

# ============================================================================
# CONFIGURATION
# ============================================================================

config <- list(
  input_file = "dbs/data_p/commuter_model_features_r.csv",
  output_dir = "results/sem",
  run_id = format(Sys.time(), "%Y%m%d_%H%M%S"),

  sta_var = "ak_ihs",

  # Indicators of latent leisure participation
  outcome_y1 = "hill_q1",
  outcome_y2 = "mean_leisure_duration",

  mediator_var = "total_travel_time",

  weight_var = "weight_ind",
  exclude_household_types = c(5, 7),
  education_recode = TRUE
)

output_path <- file.path(config$output_dir, config$run_id)
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(output_path, "analysis_log.txt")
log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  full_msg <- paste(timestamp, msg)
  cat(full_msg, "\n")
  cat(full_msg, "\n", file = log_file, append = TRUE)
}

log_msg("=== SEM Analysis v4 (Latent Variable) Started ===")
log_msg(paste("Run ID:", config$run_id))

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================

log_msg("Loading data...")
d_raw <- read.csv(config$input_file, stringsAsFactors = FALSE)
log_msg(paste("Raw data:", nrow(d_raw), "rows"))

d <- d_raw %>% filter(!(Household_type %in% config$exclude_household_types))
log_msg(paste("After household filter:", nrow(d), "rows"))

# ============================================================================
# 2. RECODE VARIABLES
# ============================================================================

log_msg("Recoding variables...")

d$female <- ifelse(d$Gender == "Woman", 1, 0)

if (config$education_recode) {
  d$edu_level <- case_when(
    d$Education %in% c(0, 1, 2, 3, 4) ~ "low_medium",
    d$Education %in% c(5, 9) ~ "high",
    TRUE ~ NA_character_
  )
  d$edu_high <- ifelse(d$edu_level == "high", 1, 0)
}

hh_levels <- sort(unique(d$Household_type))
for (h in hh_levels[-1]) {
  d[[paste0("hh_", h)]] <- ifelse(d$Household_type == h, 1, 0)
}

d$mode_pt <- ifelse(d$mode == "Public transit", 1, 0)
d$pt_sub <- ifelse(tolower(as.character(d$pt_sub)) %in% c("true", "t", "yes", "y", "1"), 1, 0)
d$active_mode <- as.numeric(d$active_mode)

log_msg(paste("Female:", sum(d$female), "/ PT mode:", sum(d$mode_pt)))

# ============================================================================
# 3. CREATE ANALYSIS DATASET
# ============================================================================

sta_var <- config$sta_var
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

# ============================================================================
# 4. STANDARDIZE CONTINUOUS VARIABLES
# ============================================================================

log_msg("Standardizing...")

scaling_params <- data.frame(variable = character(), mean = numeric(), sd = numeric())

for (v in continuous_vars) {
  m <- mean(d_analysis[[v]], na.rm = TRUE)
  s <- sd(d_analysis[[v]], na.rm = TRUE)
  d_analysis[[v]] <- (d_analysis[[v]] - m) / s
  scaling_params <- rbind(scaling_params, data.frame(variable = v, mean = m, sd = s))
}

write.csv(scaling_params, file.path(output_path, "scaling_parameters.csv"), row.names = FALSE)

# ============================================================================
# 5. BUILD SEM MODEL WITH LATENT VARIABLE
# ============================================================================

log_msg("Building SEM syntax with latent variable...")

# Predictors without HH for STA and travel time (based on v3 findings)
socio_sta <- c("female", "edu_high", "poverty_rate")
socio_tt <- c("female", "edu_high", "poverty_rate")
socio_lp <- c("female", "edu_high", "poverty_rate", "hh_6")

make_rhs <- function(vars, prefix = "") {
  if (length(vars) == 0) return("")
  paste(paste0(prefix, vars, "*", vars), collapse = " + ")
}

syntax_lines <- c(
  "# ============================================================",
  "# LATENT VARIABLE: Leisure Participation",
  "# Both diversity and duration are indicators of underlying",
  "# leisure engagement - no causal ordering imposed between them",
  "# ============================================================",
  "",
  "# Measurement model: latent leisure_participation",
  "# First loading fixed to 1 for identification",
  paste0("leisure_part =~ 1*", outcome_y1, " + lam2*", outcome_y2),
  "",
  "# ============================================================",
  "# STRUCTURAL MODEL",
  "# ============================================================",
  "",
  "# 1. STA equation",
  paste(sta_var, "~", make_rhs(socio_sta, "s_"), "+", make_rhs(transport_vars, "t_")),
  "",
  "# 2. Travel time equation",
  paste(mediator_var, "~", paste0("a*", sta_var), "+", make_rhs(socio_tt, "g_"), "+", make_rhs(transport_vars, "m_")),
  "",
  "# 3. Latent leisure participation equation",
  paste("leisure_part ~", paste0("c*", sta_var), "+", paste0("b*", mediator_var), "+",
        make_rhs(socio_lp, "d_"), "+", make_rhs(transport_vars, "r_")),
  "",
  "# ============================================================",
  "# EFFECT DECOMPOSITION",
  "# ============================================================",
  "",
  "# Effects on latent construct",
  "indirect := a * b",
  "direct := c",
  "total := c + a * b",
  "",
  "# ============================================================",
  "# EFFECTS ON SPECIFIC INDICATORS",
  "# ============================================================",
  "# Since loadings transmit the latent effect to observed variables:",
  "#   Effect on indicator = Effect on latent × Loading",
  "# First loading (diversity) is fixed to 1, second (duration) is lam2",
  "",
  "# Direct effects on each indicator",
  "direct_div := c * 1",
  "direct_dur := c * lam2",
  "",
  "# Indirect effects on each indicator (via travel time)",
  "indirect_div := a * b * 1",
  "indirect_dur := a * b * lam2",
  "",
  "# Total effects on each indicator",
  "total_div := (c + a * b) * 1",
  "total_dur := (c + a * b) * lam2"
)

sem_syntax <- paste(syntax_lines, collapse = "\n")
writeLines(sem_syntax, file.path(output_path, "sem_syntax.txt"))
log_msg("SEM syntax saved")

cat("\n=== SEM SYNTAX ===\n")
cat(sem_syntax)
cat("\n==================\n\n")

# ============================================================================
# 6. FIT MODEL
# ============================================================================

log_msg("Fitting SEM model...")

fit <- tryCatch({
  sem(
    model = sem_syntax,
    data = d_analysis,
    estimator = "DWLS",
    sampling.weights = config$weight_var,
    std.lv = FALSE  # Use marker variable (first loading = 1)
  )
}, error = function(e) {
  log_msg(paste("ERROR fitting model:", e$message))
  NULL
})

if (is.null(fit)) {
  log_msg("Model fitting failed.")
  stop("Model fitting failed")
}

log_msg("Model fitted successfully")

if (!lavInspect(fit, "converged")) {
  log_msg("WARNING: Model did not converge!")
}

# ============================================================================
# 7. EXTRACT RESULTS
# ============================================================================

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

log_msg("=== R-SQUARED ===")
for (i in seq_len(nrow(rsq_df))) {
  log_msg(paste(" ", rsq_df$variable[i], "=", round(rsq_df$rsquare[i], 3)))
}

saveRDS(fit, file.path(output_path, "model_fit.rds"))

# ============================================================================
# 8. KEY RESULTS
# ============================================================================

log_msg("=== KEY PATHWAY RESULTS ===")

# Measurement model
log_msg("--- Measurement Model ---")
loadings <- subset(pe, op == "=~")
for (i in seq_len(nrow(loadings))) {
  row <- loadings[i, ]
  log_msg(sprintf("  %s =~ %s: %.3f (std=%.3f)", row$lhs, row$rhs, row$est, row$std.all))
}

# Core pathways
log_msg("--- Structural Paths ---")
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

# ============================================================================
# 9. MODIFICATION INDICES
# ============================================================================

log_msg("=== MODIFICATION INDICES (top 10) ===")

mi <- modificationIndices(fit, sort. = TRUE, maximum.number = 20)
write.csv(mi, file.path(output_path, "modification_indices.csv"), row.names = FALSE)

for (i in seq_len(min(10, nrow(mi)))) {
  row <- mi[i, ]
  log_msg(sprintf("  %s %s %s: MI=%.2f, EPC=%.3f", row$lhs, row$op, row$rhs, row$mi, row$epc))
}

# ============================================================================
# 10. SUMMARY
# ============================================================================

log_msg("=== ANALYSIS COMPLETE ===")
log_msg(paste("Output directory:", output_path))

cat("\n========================================\n")
cat("FIT MEASURES\n")
cat("========================================\n")
print(fit_df)

cat("\n========================================\n")
cat("MEASUREMENT MODEL\n")
cat("========================================\n")
print(loadings[, c("lhs", "rhs", "est", "std.all")])

cat("\n========================================\n")
cat("KEY PATHWAY EFFECTS\n")
cat("========================================\n")
print(key_effects[, c("label", "est", "se", "pvalue", "std.all")])

cat("\n========================================\n")
cat("For visualization, run: src/13-sem-plot.R\n")
cat("========================================\n")
