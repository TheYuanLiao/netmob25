library(dplyr)
library(tidyr)
library(ggplot2)
library(arrow)
library(ggthemes)
library(scales)
library(ggsci)
library(Hmisc)  # for wtd.quantile
library(sf)         # For reading spatial data
library(viridis)    # Optional: nice color scale
library(ggmap)
library(ggspatial)
library(arrow)
options(scipen = 10000)
a <- "Essential needs"
t <- "30 min"

df <- read_parquet("results/activity_access_ind_model.parquet") %>%
  filter(amenity == a) %>%
  filter(time_threshold == t)

# 1. Simple box plot ----
# Function to compute weighted boxplot stats per group
weighted_box_stats <- function(data, group_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      ymin  = wtd.quantile(log_disparity, weights = weight_ind, probs = 0.05, na.rm = TRUE),
      lower = wtd.quantile(log_disparity, weights = weight_ind, probs = 0.25, na.rm = TRUE),
      middle= wtd.quantile(log_disparity, weights = weight_ind, probs = 0.5,  na.rm = TRUE),
      upper = wtd.quantile(log_disparity, weights = weight_ind, probs = 0.75, na.rm = TRUE),
      ymax  = wtd.quantile(log_disparity, weights = weight_ind, probs = 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(x = !!group_var)
}

# List of grouping variables
group_vars <- c("Gender", "Education", "Household_type", 
                "Car_no", "Bike_no", "Two_wheeler_no", "Escooter_no", "pt_sub")

# Loop over each group variable
for (var in group_vars) {
  stats_df <- weighted_box_stats(df, var)

  p <- ggplot(stats_df, aes(x = as.factor(x))) +
    geom_boxplot(
      aes(
        ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax
      ),
      stat = "identity", fill = "steelblue", alpha = 0.7
    ) +
    labs(
      x = var, y = "log_disparity"
    ) +
    theme_classic() +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()

  ggsave(filename = paste0("figures/access_disparity_distr_en_15_", var, ".png"), plot = p,
       width = 3, height = 3, unit = "in", dpi = 300)
}

# 2. EBM feature importance ----
# The current model is based on Public transit 15 min access to essential needs
df_fscore <- read.csv("results/ebm/f_score.csv") %>%
            arrange(desc(Score)) %>%
            slice(1:7)
colors <- df_fscore$Color[order(df_fscore$Score, decreasing = TRUE)]
df_fscore$Name <- factor(df_fscore$Name, levels = df_fscore$Name, labels = df_fscore$Name)


g2 <- ggplot(data = df_fscore, aes(y = Name, x = Score)) +
  theme_hc() +
  labs(y = "Feature",
       x = "Importance score") +
  geom_point(size = 3) +
  xlim(0, 0.5) +
  geom_segment(aes(y = Name,
                   yend = Name,
                   x = min(Score),
                   xend = max(Score)),
               linetype = "dashed",
               linewidth = 0.3) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.y = element_text(colour = df_fscore$Color[as.integer(df_fscore$Name)]))
g2
ggsave(filename = "figures/ebm_features.png", plot = g2,
       width = 7, height = 5, unit = "in", dpi = 300)

# 3. Feature effect ----
feature_dict <- list(
  d2h_nh = "Distance to Home Neighborhood",
  Age = "Age",
  access_h = "Access (home)",
  `time_threshold` = "Time Threshold",
  `amenity` = "Amenity",
  `mode` = "Mode",
  `Gender` = "Gender",
  `Education` = "Education",
  `Household_type` = "Household type",
  `Car_no` = "Car number",
  `Bike_no` = "Bike number",
  `Two_wheeler_no` = "Two-wheeler number",
  `Escooter_no` = "E-scooter number",
  `pt_sub` = "Public Transport Subscription"
)
var_continuous <- c('d2h_nh', 'Age', "access_h")

df_vis <- as.data.frame(read_parquet("results/activity_access_ind_model.parquet"))

# Load single-feature effect
df_effect <- read.csv("results/ebm/features.csv")

single.feature.plot <- function(var.name, df, df.raw, log.label, y.label){
  df2plot <- df[df$var==var.name, ]
  if (var.name %in% var_continuous) {df2plot$x <- as.numeric(df2plot$x)}
  qts <- quantile(df.raw[,var.name], probs = seq(0, 1, 0.01))
  df.qts <- data.frame(qts)
  names(df.qts) <- 'qts'
  rugy <- -0.025 #min(df2plot$y_lower) - 0.001

  g <- ggplot(data = df2plot) + theme_minimal() +
    geom_point(data = df.qts, aes(x=qts, y=rugy), shape=108) +
    geom_line(aes(x=x, y=y), size=0.8) +
    geom_ribbon(aes(x=x, ymin = y_lower, ymax = y_upper),
                fill='royalblue4', color=NA,
                alpha = 0.5) +
    #ylim(-0.03, 0.06) +
    geom_hline(yintercept=0, color='gray40', alpha=0.7, size=.5) +
    labs(x=feature_dict[var.name], y=y.label) +
    theme(plot.margin = margin(1,0,0,0, "cm"))

  if (log.label == TRUE) {
    g <- g +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))
  }
  return(g)
}

var <- "Age"
g3 <- single.feature.plot(var.name=var, df=df_effect, df.raw=df_vis,
        log.label=FALSE, y.label = 'Impact on access disparity')
ggsave(filename = paste0("figures/", "ebm_f_effect_", var, ".png"), plot = g3,
       width = 3, height = 3, unit = "in", , bg = "white", dpi = 300)

var <- "d2h_nh"
g4 <- single.feature.plot(var.name=var, df=df_effect, df.raw=df_vis,
        log.label=TRUE, y.label = 'Impact on access disparity')
ggsave(filename = paste0("figures/", "ebm_f_effect_", var, ".png"), plot = g4,
       width = 3, height = 3, unit = "in", , bg = "white", dpi = 300)

var <- "access_h"
g5 <- single.feature.plot(var.name=var, df=df_effect, df.raw=df_vis,
        log.label=TRUE, y.label = 'Impact on access disparity')
ggsave(filename = paste0("figures/", "ebm_f_effect_", var, ".png"), plot = g5,
       width = 3, height = 3, unit = "in", , bg = "white", dpi = 300)

# 4. Feature effect --- (top categorical)
# pt_sub, Car_no, Education, Household_type, 'Bike_no', Gender
for (var.tst in c('pt_sub', 'Car_no')){
    df2plot <- df_effect[df_effect$var==var.tst, ]

    g5 <- ggplot(data = df2plot) + theme_classic() +
    geom_point(aes(x=as.factor(x), y=y), size=1) +
    geom_errorbar(aes(x=as.factor(x),ymin=y_lower, ymax=y_upper), width=.1, alpha=0.7, color='royalblue4') +
    geom_hline(yintercept=0, color='gray40', alpha=0.7, size=.5) +
    labs(x=feature_dict[var.tst], y='Score') +
    theme(axis.text.x = element_text(angle = 0, vjust=0.7),
            plot.margin = margin(1,0,0,0, "cm")) +
    coord_flip()
    ggsave(filename = paste0("figures/", "ebm_f_effect_", var.tst, ".png"), plot = g5,
        width = 3, height = 3, unit = "in", , bg = "white", dpi = 300)
}

# 3.1 Education ----
var.tst <- "Education"
df2plot <- df_effect[df_effect$var==var.tst, ]
df2plot <- df2plot[df2plot$x != '9.0', ]
df2plot$x <- factor(df2plot$x,
                    levels=c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0"),
                    labels=c('No diploma', 'Vocational certificate', 'Lower secondary certificate',
                    'Upper secondary diploma', '3–4 year higher education', '5+ year higher education'))

g6 <- ggplot(data = df2plot) + theme_classic() +
geom_point(aes(x=as.factor(x), y=y), size=1) +
geom_errorbar(aes(x=as.factor(x),ymin=y_lower, ymax=y_upper), width=.1, alpha=0.7, color='royalblue4') +
geom_hline(yintercept=0, color='gray40', alpha=0.7, size=.5) +
labs(x=feature_dict[var.tst], y='Score') +
theme(axis.text.x = element_text(angle = 0, vjust=0.7),
        plot.margin = margin(1,0,0,0, "cm")) +
coord_flip()
ggsave(filename = paste0("figures/", "ebm_f_effect_", var.tst, ".png"), plot = g6,
    width = 5, height = 3, unit = "in", , bg = "white", dpi = 300)

# 3.2 Household type ----
var.tst <- "Household_type"
df2plot <- df_effect[df_effect$var==var.tst, ]
df2plot$x <- factor(df2plot$x,
                    levels=c("0", "1", "2", "3", "4", "5", "6", "7"),
                    labels=c('Living alone',
    'In a couple without children',
    'Single parent (divorced / separated / widowed)',
    'Living with one or both parents',
    'Not related to other household members',
    'In a shared apartment (flat-sharing)',
    'In a couple with child(ren)',
    'Another family member in the household'))

g7 <- ggplot(data = df2plot) + theme_classic() +
geom_point(aes(x=as.factor(x), y=y), size=1) +
geom_errorbar(aes(x=as.factor(x),ymin=y_lower, ymax=y_upper), width=.1, alpha=0.7, color='royalblue4') +
geom_hline(yintercept=0, color='gray40', alpha=0.7, size=.5) +
labs(x=feature_dict[var.tst], y='Score') +
theme(axis.text.x = element_text(angle = 0, vjust=0.7),
        plot.margin = margin(1,0,0,0, "cm")) +
coord_flip()
ggsave(filename = paste0("figures/", "ebm_f_effect_", var.tst, ".png"), plot = g7,
    width = 5, height = 3, unit = "in", , bg = "white", dpi = 300)

# 4. Interaction effect (distance x access)
df_inter <- read.csv("results/ebm/interactions.csv")

# Filter for a specific interaction
df_plot <- df_inter %>%
  filter(feature1 == "d2h_nh", feature2 == "access_h")

# Convert to factor if needed
df_plot$x <- as.numeric(df_plot$x)
df_plot$y <- as.numeric(df_plot$y)

# Plot as scatter plot
g8 <- ggplot(df_plot, aes(x = x, y = y, color = effect)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "Distance to home (km)", y = "Access at home", 
       color = "Interaction effect") +
  theme_classic(base_size = 14) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  theme(legend.position = "top")

ggsave(filename = paste0("figures/", "ebm_i_effect_da.png"), plot = g8,
    width = 4, height = 3, unit = "in", , bg = "white", dpi = 300)
