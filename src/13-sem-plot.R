# ============================================================================
# SEM Visualization Script
# ============================================================================
#
# Creates path diagram from saved SEM model fit.
# Shows significant direct and indirect pathways with |std.coef| > 0.1
#
# Usage:
#   Rscript 13-sem-plot.R                              # Uses latest run in results/sem/
#   Rscript 13-sem-plot.R results/sem/20260420_114317  # Uses specific path
#   Rscript 13-sem-plot.R results/sem_sensitivity/budget_60min
#
# Input: {path}/model_fit.rds
# Output: {path}/sem_diagram.svg, sem_diagram.png
#
# ============================================================================

library(lavaan)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Set path to SEM results folder (change this manually)
# Examples:
#   output_path <- "results/sem/20260420_114317"
#   output_path <- "results/sem_sensitivity/budget_60min"
#   output_path <- "results/sem_sensitivity/budget_120min"

output_path <- "results/sem_sensitivity/budget_120min"  # <-- CHANGE THIS

cat("Using SEM results from:", output_path, "\n")

# Visualization thresholds
viz_pvalue_threshold <- 0.05
viz_effect_threshold <- 0.10  # Show paths with |std.coef| > 0.1

# Variable names (must match model)
sta_var <- "ak_ihs"
mediator_var <- "total_travel_time"
outcome_y1 <- "hill_q1"
outcome_y2 <- "mean_leisure_duration"

# ============================================================================
# 1. LOAD MODEL FIT
# ============================================================================

fit_file <- file.path(output_path, "model_fit.rds")
if (!file.exists(fit_file)) {
  stop("Model fit not found: ", fit_file)
}

fit <- readRDS(fit_file)
cat("Model loaded successfully\n")

# Extract parameter estimates
pe <- parameterEstimates(fit, standardized = TRUE)

# ============================================================================
# 2. SELECT SIGNIFICANT PATHS
# ============================================================================

# Get significant regression paths with |std.all| > threshold
sig_reg <- subset(pe, op == "~" &
                    pvalue < viz_pvalue_threshold &
                    abs(std.all) > viz_effect_threshold)

cat("\nSignificant regression paths (p <", viz_pvalue_threshold,
    ", |std| >", viz_effect_threshold, "):\n")
print(sig_reg[, c("lhs", "op", "rhs", "std.all", "pvalue")])

# Always include measurement model paths (latent -> indicators)
measurement_paths <- subset(pe, op == "=~")

# Combine
sig_paths <- rbind(sig_reg, measurement_paths)

cat("\nTotal paths to display:", nrow(sig_paths), "\n")

# ============================================================================
# 3. BUILD GRAPHVIZ DOT
# ============================================================================

nodes_in_paths <- unique(c(sig_paths$lhs, sig_paths$rhs))

# Label dictionary for display names
label_dict <- list()
label_dict[[sta_var]] <- "Space-time\\naccessibility"
label_dict[[mediator_var]] <- "Travel time"
label_dict[["leisure_part"]] <- "Leisure\\nparticipation"
label_dict[[outcome_y1]] <- "Location\\ndiversity"
label_dict[[outcome_y2]] <- "Leisure\\nduration"
label_dict[["female"]] <- "Female"
label_dict[["edu_high"]] <- "Education:\\nHigh"
label_dict[["poverty_rate"]] <- "Poverty rate"
label_dict[["mode_pt"]] <- "Public transit"
label_dict[["pt_sub"]] <- "PT subscription"
label_dict[["active_mode"]] <- "Active mode"
label_dict[["hh_6"]] <- "Couple w/\\nchildren"

get_label <- function(id) {
  if (!is.null(label_dict[[id]])) label_dict[[id]] else id
}

# Build DOT syntax
dot <- c(
  "digraph sem {",
  '  graph [rankdir=LR, nodesep=0.8, ranksep=1.0, splines=spline, fontname=Helvetica, fontsize=24];',
  '  node [shape=box, style="rounded,filled", fillcolor="#F9F9F9", fontname=Helvetica, fontsize=28];',
  '  edge [fontname=Helvetica, fontsize=24];',
  '  leisure_part [shape=ellipse, fillcolor="#E8F4FD", label="Leisure\\nparticipation"];'
)

# Add nodes
for (id in setdiff(nodes_in_paths, "leisure_part")) {
  lbl <- get_label(id)
  dot <- c(dot, sprintf('  "%s" [label="%s"];', id, lbl))
}

# Add edges
for (i in seq_len(nrow(sig_paths))) {
  row <- sig_paths[i, ]
  if (row$op == "=~") {
    # Measurement: latent -> indicator (dashed)
    from <- row$lhs
    to <- row$rhs
    est <- round(row$std.all, 2)
    dot <- c(dot, sprintf('  "%s" -> "%s" [label="%.2f", style=dashed, color="#666666"];',
                          from, to, est))
  } else {
    # Regression: predictor -> outcome
    from <- row$rhs
    to <- row$lhs
    est <- round(row$std.all, 2)
    col <- ifelse(row$std.all >= 0, "#40B0A6", "#E66100")  # Teal for positive, orange for negative
    penw <- 1 + 3 * abs(row$std.all)
    dot <- c(dot, sprintf('  "%s" -> "%s" [label="%.2f", color="%s", penwidth=%.1f];',
                          from, to, est, col, penw))
  }
}

dot <- c(dot, "}")

# Save DOT source
dot_file <- file.path(output_path, "sem_diagram.dot")
writeLines(dot, dot_file)
cat("\nDOT source saved:", dot_file, "\n")

# ============================================================================
# 4. RENDER DIAGRAM
# ============================================================================

tryCatch({
  g <- DiagrammeR::grViz(paste(dot, collapse = "\n"))

  # Export SVG
  svg_content <- export_svg(g)
  svg_file <- file.path(output_path, "sem_diagram.svg")
  writeLines(svg_content, svg_file)
  cat("SVG saved:", svg_file, "\n")

  # Export PNG
  png_file <- file.path(output_path, "sem_diagram.png")
  rsvg_png(charToRaw(svg_content), file = png_file, width = 3000)
  cat("PNG saved:", png_file, "\n")

}, error = function(e) {
  cat("Warning: Could not create diagram:", e$message, "\n")
})

# ============================================================================
# 5. SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("========================================\n")
cat("Path:", output_path, "\n")
cat("Effect threshold: |std.coef| >", viz_effect_threshold, "\n")
cat("P-value threshold:", viz_pvalue_threshold, "\n")
cat("Paths displayed:", nrow(sig_paths), "\n")
cat("  - Measurement paths:", nrow(measurement_paths), "\n")
cat("  - Significant regression paths:", nrow(sig_reg), "\n")
