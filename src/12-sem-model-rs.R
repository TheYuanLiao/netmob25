# ---------------------------------------------------
# SEM data prep + model
# ---------------------------------------------------
library(lavaan)
library(dplyr)
library(lavaanPlot)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# 1) Load & light filtering
d <- read.csv("dbs/data_p/commuter_model_features_r.csv") %>%
  filter(Household_type != 5, Household_type != 7)

# 2) Variable roles
socio_cat  <- c("Gender", "Education", "Household_type")
socio_con <- "poverty_rate"
ls_var <- "active_mode"
mode_var   <- "mode"
pt_sub_var <- "pt_sub"
spa_var    <- "ak_log"
beh_cnt    <- "total_travel_time"    # xs_total_hws is dropped
leisure_y  <- "hill_q1"   # "activity_time_third" is dropped
weight_var <- "weight_ind"

# 3) Helpers
safe <- function(x) {
  x <- gsub("[^0-9A-Za-z]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}
mk_dummies <- function(df, vars, drop_first = TRUE) {
  if (length(vars) == 0) return(data.frame())
  mm <- model.matrix(
    as.formula(paste0("~ 0 + ", paste(sprintf("factor(%s)", vars), collapse = " + "))),
    data = df
  )
  out <- as.data.frame(mm)
  names(out) <- safe(gsub("^factor\\(([^)]+)\\)", "\\1", names(out)))
  if (drop_first) {
    groups <- sub("_.*$", "", names(out))
    keep <- unlist(tapply(seq_along(names(out)), groups, function(ix) ix[-1]))
    out <- out[, keep, drop = FALSE]
  }
  out
}
is_binary <- function(x) {
  ux <- unique(na.omit(x))
  length(ux) <= 2 && all(ux %in% c(0, 1))
}

# 4) Core transforms
# d[[beh_bin]] <- ifelse(as.numeric(d[[beh_bin]]) > 0, 1, 0)
#eps <- 1e-6
#d$entropy_logit <- qlogis(pmin(pmax(d$entropy_mm, eps), 1 - eps))

# Sanitize factor levels
to_sanitize <- c(socio_cat, mode_var, pt_sub_var)
for (v in to_sanitize[to_sanitize %in% names(d)]) {
  d[[v]] <- as.factor(d[[v]])
  levels(d[[v]]) <- safe(levels(d[[v]]))
}

# 5) Exogenous block
exog_dum <- mk_dummies(d, socio_cat, drop_first = TRUE)

d[[mode_var]] <- as.factor(d[[mode_var]])
mode_mm <- model.matrix(~ 0 + factor(mode), data = d)
mode_mm <- as.data.frame(mode_mm)
names(mode_mm) <- safe(gsub("^factor\\(mode\\)", "mode", names(mode_mm)))
base_mode <- safe(levels(d[[mode_var]])[1])
mode_dum <- mode_mm[, !grepl(paste0("_", base_mode, "$"), names(mode_mm)), drop = FALSE]

if (!is.numeric(d[[pt_sub_var]])) {
  d[[pt_sub_var]] <- ifelse(tolower(as.character(d[[pt_sub_var]])) %in% c("yes","y","true","t","1"), 1, 0)
}
pt_num <- data.frame(pt_sub = as.numeric(d[[pt_sub_var]]))

if (!is.numeric(d[[ls_var]])) {
  d[[ls_var]] <- ifelse(tolower(as.character(d[[ls_var]])) %in% c("yes","y","true","t","1"), 1, 0)
}
ls_dum <- data.frame(active_mode = as.numeric(d[[ls_var]]))

exog_block <- cbind(exog_dum, mode_dum, pt_num, ls_dum)
names(exog_block) <- safe(names(exog_block))
exog_names <- names(exog_block)

# 6) Final dataset
# d[[beh_bin]] <- factor(d[[beh_bin]], levels = c(0, 1), labels = c("no", "yes"), ordered = TRUE)
needed <- c(spa_var, socio_con, beh_cnt, leisure_y, weight_var, exog_names)

d_prepped <- cbind(
  d[, c(spa_var, beh_cnt, leisure_y, weight_var)],
  exog_block
)
d_prepped <- d_prepped[complete.cases(d_prepped[, needed]), ]

# 7) Scale continuous vars
cont_vars <- c("ak_log", "total_travel_time", "hill_q1", "poverty_rate")
cont_vars <- cont_vars[cont_vars %in% names(d_prepped)]
for (v in cont_vars) {
  d_prepped[[v]] <- as.numeric(scale(d_prepped[[v]]))
}

# 8) Normalize weights
d_prepped <- d_prepped[d_prepped[[weight_var]] > 0, ]
d_prepped[[weight_var]] <- d_prepped[[weight_var]] / mean(d_prepped[[weight_var]])

# 9) Drop near-constant dummies
num_cols <- setdiff(names(d_prepped)[sapply(d_prepped, is.numeric)], weight_var)
rare_or_tiny <- c()
for (nm in num_cols) {
  if (is_binary(d_prepped[[nm]])) {
    p <- mean(d_prepped[[nm]], na.rm = TRUE)
    if (p < 0.01 | p > 0.99) rare_or_tiny <- c(rare_or_tiny, nm)
  }
  if (sd(d_prepped[[nm]], na.rm = TRUE) < 1e-5) rare_or_tiny <- c(rare_or_tiny, nm)
}
if (length(rare_or_tiny) > 0) {
  d_prepped <- d_prepped[, !(names(d_prepped) %in% rare_or_tiny)]
}
exog_names_final <- intersect(exog_names, names(d_prepped))
exog_names_final <- exog_names_final[sapply(d_prepped[exog_names_final], is.numeric)]
rhs_exog <- if (length(exog_names_final) > 0) paste(exog_names_final, collapse = " + ") else "1"

# Variables lavaan will see (observed)
vars_in_model <- unique(c(
  "ak_log", "total_travel_time", "hill_q1",
  exog_names_final
))
X <- d_prepped[, vars_in_model, drop = FALSE]

# a) drop zero/near-zero variance
nzv <- names(which(sapply(X, function(x) is.numeric(x) && var(x, na.rm=TRUE) < 1e-8)))
if (length(nzv)) {
  message("Dropping near-zero variance: ", paste(nzv, collapse=", "))
  X <- X[, !(names(X) %in% nzv), drop = FALSE]
}

# b) drop perfectly or near-perfectly correlated duplicates
num_cols <- names(X)[sapply(X, is.numeric)]
if (length(num_cols) >= 2) {
  M <- suppressWarnings(cor(X[, num_cols, drop=FALSE], use="pairwise.complete.obs"))
  keep <- rep(TRUE, length(num_cols))
  names(keep) <- num_cols
  for (i in seq_len(ncol(M))) {
    for (j in seq_len(i-1)) {
      if (is.finite(M[j,i]) && abs(M[j,i]) > 0.9999) {
        # drop the later column i
        keep[colnames(M)[i]] <- FALSE
      }
    }
  }
  drop_corr <- names(keep)[!keep]
  if (length(drop_corr)) {
    message("Dropping near-duplicate columns (|r|>0.9999): ", paste(drop_corr, collapse=", "))
    X <- X[, keep, drop = FALSE]
  }
}

# c) hard check: covariance eigenvalues
S <- cov(X, use = "pairwise.complete.obs")
eig <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
if (any(!is.finite(eig)) || min(eig) <= 1e-10) {
  # Backstop: QR-drop linear dependencies
  keep2 <- qr(X)$pivot[seq_len(qr(X)$rank)]
  X <- X[, sort(keep2), drop = FALSE]
  message("QR pruning applied to remove linear dependencies.")
}

# Replace the columns in d_prepped with the pruned set, keep weight
d_prepped_pd <- cbind(X, weight_ind = d_prepped$weight_ind)

# Baseline model ----
exog_final2 <- intersect(exog_names_final, names(d_prepped_pd))
rhs_exog2 <- if (length(exog_final2) > 0) paste(exog_final2, collapse = " + ") else "1"

# Complete model ----
# ===== SEM syntax =====
## ------------------------------
## Build lavaan syntax dynamically
## ------------------------------

# Endogenous & outcomes to exclude from attribute set
endo_out <- c("ak_log","total_travel_time", "hill_q1")

# Detect transport variables in your selected exogenous set
mode_vars <- exog_final2[grepl("^mode", exog_final2)]
pt_var    <- intersect("pt_sub", exog_final2)

# Attributes = exog_final2 minus transport & endogenous/outcomes
exog_attr <- setdiff(exog_final2, c(mode_vars, pt_var, endo_out))

# helpers
rhs_plain   <- function(vars) if (length(vars)) paste(vars, collapse=" + ") else "1"
rhs_labeled <- function(vars, pref) {
  if (!length(vars)) return(character(0))
  paste0(pref, vars, "*", vars)
}

# 1) Transport mode equations (each mode dummy ~ attributes only)
mode_lines <- character(0)
if (length(mode_vars)) {
  for (m in mode_vars) {
    rhs <- if (length(exog_attr)) paste(rhs_labeled(exog_attr, paste("g_", m, "_", sep="")), collapse=" + ") else "1"
    mode_lines <- c(mode_lines, paste(m, "~", rhs))
  }
}

# 2) pt_sub ~ attributes
ptsub_line <- if (length(pt_var)) {
  paste("pt_sub ~", if (length(exog_attr)) paste(rhs_labeled(exog_attr, "h_"), collapse=" + ") else "1")
} else character(0)

# Individual -> Trips (trip making equation)
tt_line_i <- paste(
  "total_travel_time ~",
  paste(rhs_labeled(exog_attr, "g_tt_"), collapse = " + ")
)


# 3) ak_log ~ attributes + all transport vars (label per predictor)
ak_rhs_parts <- character(0)
if (length(exog_attr)) ak_rhs_parts <- c(ak_rhs_parts, paste(rhs_labeled(exog_attr, "s_"), collapse=" + "))
if (length(mode_vars)) ak_rhs_parts <- c(ak_rhs_parts, paste(paste("k", mode_vars, "*", mode_vars, sep=""), collapse=" + "))
if (length(pt_var))    ak_rhs_parts <- c(ak_rhs_parts, "k_pt*pt_sub")
if (!length(ak_rhs_parts)) ak_rhs_parts <- "1"
aklog_line <- paste("ak_log ~", paste(ak_rhs_parts, collapse=" + "))

# 4) SPA -> travel behavior
tt_line <- "total_travel_time ~ a_tt*ak_log"


# 5) Leisure outcomes: SPA + behaviors + attributes (direct)
ent_line  <- paste("hill_q1 ~ c1*ak_log + b_tt*total_travel_time",
                   if (length(exog_attr)) paste("+", paste(rhs_labeled(exog_attr, "d_"), collapse=" + ")) else "")


# TransportMode -> Participation outcomes
mode_participation_lines <- character(0)
t_var <- c(mode_vars, pt_sub_var)
if (length(t_var)) {
  mode_participation_lines <- c(
    paste("hill_q1 ~", paste(rhs_labeled(t_var, "r_"), collapse = " + "))
  )
}

# 6) Residual covariance between leisure outcomes
# out_cov_line <- "hill_q1 ~~ activity_time_third"

# 7) Correlate attributes pairwise (lower triangle only)
cov_attr <- character(0)
if (length(exog_attr) >= 2) {
  for (i in seq_len(length(exog_attr)-1)) {
    left  <- exog_attr[i]
    right <- exog_attr[(i+1):length(exog_attr)]
    cov_attr <- c(cov_attr, paste0(left, " ~~ ", paste(right, collapse=" + ")))
  }
}

# 8) Correlate transport residuals (mode dummies among themselves, each with pt_sub)
cov_mode <- character(0)
if (length(mode_vars) >= 2) {
  for (i in seq_len(length(mode_vars)-1)) {
    left  <- mode_vars[i]
    right <- mode_vars[(i+1):length(mode_vars)]
    cov_mode <- c(cov_mode, paste0(left, " ~~ ", paste(right, collapse=" + ")))
  }
}
if (length(mode_vars) && length(pt_var)) {
  cov_mode <- c(cov_mode, paste(paste(mode_vars, "pt_sub", sep=" ~~ "), collapse="\n"))
}

# 9) SPA decompositions
spa_decomp <- c(
  "indirect_entropy := a_tt*b_tt",
  "direct_entropy := c1",
  "total_entropy := direct_entropy + indirect_entropy"
)

# 10) Attribute-level decompositions (direct + via SPA and SPA->behaviors)
attr_decomp <- character(0)
if (length(exog_attr)) {
  for (z in exog_attr) {
    # SPA-from-attribute z = direct s_z + via each mode m: k_m * g_{m,z} + via pt_sub: k_pt * h_z
    via_modes <- if (length(mode_vars)) paste(paste("k", mode_vars, "*g_", mode_vars, "_", z, sep=""), collapse=" + ") else "0"
    via_pt    <- if (length(pt_var))   paste("k_pt*h_", z, sep="") else "0"
    spa_from_z <- paste0("spa_from_", z, " := s_", z, " + ", via_modes, " + ", via_pt)

    # entropy
    ind_e_dir <- paste0("ind_", z, "_entropy_v_spa := spa_from_", z, " * c1")
    ind_e_tt  <- paste0("ind_", z, "_entropy_v_ttime := spa_from_", z, " * a_tt * b_tt")
    ind_e_tot <- paste0("ind_", z, "_entropy_total := ",
                        "ind_", z, "_entropy_v_spa + ind_", z, "_entropy_v_ttime")
    dir_e     <- paste0("dir_", z, "_entropy := d_", z)
    tot_e     <- paste0("tot_", z, "_entropy := dir_", z, "_entropy + ind_", z, "_entropy_total")

    attr_decomp <- c(attr_decomp,
                     spa_from_z,
                     ind_e_dir, ind_e_tt, ind_e_tot, dir_e, tot_e)
  }
}

# 11) Assemble full syntax
lines <- c(
  mode_lines,
  ptsub_line,
  aklog_line,
  tt_line,
  tt_line_i, 
  ent_line,
  mode_participation_lines,
  cov_attr,
  cov_mode,
  spa_decomp,
  attr_decomp
  )

sem_syntax <- paste(lines[lines != ""], collapse = "\n")
cat(sem_syntax)

fit <- sem(
  model = sem_syntax,
  data  = d_prepped,
  estimator = "DWLS",
  sampling.weights = "weight_ind",
  std.lv = TRUE
)
# Capture the lavaan summary into a character vector
out <- capture.output(
  summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
)

# Send to clipboard
writeClipboard(out)

## --- mapping ----
cat_name_dict <- list(
  hill_q1        = "Leisure location diversity",
  #activity_time_third  = "Leisure activity time (min)",
  total_travel_time    = "Total travel time (min)",
  #xs_total_hws         = "Trip chaining complexity",
  ak_log               = "STA value", # "Ai (log)",
  pt_sub               = "Public transit subscription",
  active_mode          = "Use of active mode",
  modeCar              = "Car as main mode"
)
household_order <- c(
  "Living alone","In a couple w/o children","Single parent",
  "Living with parent(s)","Not related to other household members",
  "In a shared apartment","In a couple w/ child(ren)",
  "Another family member in the household"
)
cat_name_dict[["Household_type2"]] <- paste("Household:", household_order[3])
cat_name_dict[["Household_type3"]] <- paste("Household:", household_order[4])
cat_name_dict[["Household_type4"]] <- paste("Household:", household_order[5])
cat_name_dict[["Household_type6"]] <- paste("Household:", household_order[7])

## --- significant standardized paths from lavaan ---
pe <- parameterEstimates(fit, standardized = TRUE)
paths <- subset(pe, op == "~" & pvalue < .05 & abs(std.all) > 0.1)

## --- node groups (IDs must match your lavaan variable names) ---
attr_nodes <- c("Household_type6","active_mode") #"Household_type2","Household_type3","Household_type4"
trans_nodes <- c("modeCar","pt_sub")
spa_node    <- "ak_log"
beh_nodes   <- c("total_travel_time")
out_nodes   <- c("hill_q1")
all_nodes   <- c(attr_nodes, trans_nodes, spa_node, beh_nodes, out_nodes)

## helper to emit cluster with vertical order and spacing
emit_cluster <- function(cluster_name, label, ids, lab_fontsize = 24) {
  if (length(ids) == 0) return(character(0))
  # invisible chain to enforce top->bottom order; constraint=false so it doesn't affect column flow
  invis_chain <- if (length(ids) >= 2) {
    paste0(
      paste(
        sprintf('"%s" -> "%s"', head(ids, -1), tail(ids, 1)),
        collapse = " -> "
      ),
      ' [style=invis, weight=100, constraint=false];'
    )
  } else ""
  c(
    sprintf('subgraph %s {', cluster_name),
    '  rank=same;',   # in LR layout, same rank = vertical stack
    sprintf('  label="%s"; labelloc="t"; fontsize=%d; fontname=Helvetica; fontcolor=black;',
            label, lab_fontsize),
    '  color=white;',
    paste(sprintf('  "%s";', ids), collapse="\n"),
    if (nchar(invis_chain)) paste0("  ", invis_chain) else "",
    '}'
  )
}

## --- build DOT ---
dot <- c(
  "digraph sem {",
  '  graph [rankdir=LR, nodesep=0.6, ranksep=0.8, splines=spline, fontname=Helvetica, ordering="out"];',
  '  node  [shape=box, style="rounded,filled", fillcolor="#F9F9F9", fontname=Helvetica, fontsize=20];'
)

# (a) explicit node defs with pretty labels
for (id in all_nodes) {
  lbl <- if (!is.null(cat_name_dict[[id]])) cat_name_dict[[id]] else id
  dot <- c(dot, sprintf('  "%s" [label="%s"];', id, lbl))
}

# (b) clusters: vertical lists within each column
dot <- c(dot,
  emit_cluster("cluster_attr", "Individual attributes", attr_nodes),
  emit_cluster("cluster_trans", "Transport mode",  trans_nodes),
  emit_cluster("cluster_spa",   "Space-time accessibility",        spa_node),
  emit_cluster("cluster_beh",   "Mobility",   beh_nodes),
  emit_cluster("cluster_out",   "Leisure activity participation",    out_nodes)
)

# (c) draw significant edges (coef label, color by sign, width by magnitude)
for (i in seq_len(nrow(paths))) {
  from <- paths$rhs[i]
  to   <- paths$lhs[i]
  est  <- round(paths$std.all[i], 2)
  col  <- ifelse(paths$std.all[i] >= 0, "#40B0A6", "#E66100")
  penw <- 1 + 6*abs(paths$std.all[i])
  dot <- c(dot,
    sprintf('  "%s" -> "%s" [label="%s", color="%s", penwidth=%s, arrowsize=0.7, fontsize=24];',
            from, to, est, col, penw)
  )
}

dot <- c(dot, "}")

# render
g <- DiagrammeR::grViz(paste(dot, collapse = "\n"))
# Save to file
svg <- export_svg(g)                     # convert to raw SVG text
cm_to_px <- function(cm, dpi = 300) (cm / 2.54) * dpi

width_cm  <- 10

rsvg_png(
  charToRaw(svg),
  file = "figures/sem_plot_r.png",
  width  = cm_to_px(width_cm, 300)
)
