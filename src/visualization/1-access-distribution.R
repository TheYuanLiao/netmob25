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

# options(scipen = 10000)
# 1. Access gap ----
a <- "Essential needs"
# a <- "Public transit"
t <- "15 min"

# Load the data
df <- read_parquet("results/activity_access_ind_distr.parquet") %>%
  filter(amenity == a) %>%
  filter(time_threshold == t)

df_raw <- read_parquet("results/activity_access_ind.parquet") %>%
  group_by(amenity, mode, time_threshold) %>%
  summarise(gap_md = wtd.quantile(gap, weights = weight_ind, probs = 0.5, na.rm = TRUE)) %>%
  ungroup()

# Plot pdf
df2plot_raw <- df_raw %>%
  filter(time_threshold == t) %>%
  filter(amenity == a)

g1 <- ggplot(df, aes(x = x, y = y, color = factor(mode))) +
  theme_classic() +
  geom_line() +
  geom_vline(data = df2plot_raw,
    aes(xintercept = gap_md, color = factor(mode)),
    linetype = "dashed", alpha = 0.7
  ) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = scales::label_math()(c(-2, -1, 0, 1, 2))
  ) +
  scale_y_log10(
    breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1),
    labels = scales::label_math()(c(-5, -4, -3, -2, -1, 0))
  ) +
  labs(
    x = "Accessibility disparity (non-home/home)",
    y = "Probability density (pdf)",
    color = "Mode"
  ) +
  scale_color_npg(name = "Transport mode",
                  breaks = c("Car", "Public transit"),
                  labels = c("Car", "Public transit")) +
  # scale_color_npg(name = "Amenity",
  #                 breaks = c("Essential needs", "Education", "Health services", "Social & Leisure"),
  #                 labels = c("Essential needs", "Education", "Health services", "Social & Leisure")) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    legend.position = "top"
  )

ggsave(filename = "figures/access_disparity_distr_en_15.png", plot = g1,
       width = 4, height = 3, unit = "in", dpi = 300)

# 2. Access (home) ----
a <- "Essential needs"
# a <- "Public transit"
t <- "15 min"

# Load the data
df <- read_parquet("results/activity_access_h_ind_distr.parquet") %>%
  filter(amenity == a) %>%
  filter(time_threshold == t)

df_raw <- read_parquet("results/activity_access_ind.parquet") %>%
  group_by(amenity, mode, time_threshold) %>%
  summarise(gap_md = wtd.quantile(access_h, weights = weight_ind, probs = 0.5, na.rm = TRUE)) %>%
  ungroup()

# Plot pdf
df2plot_raw <- df_raw %>%
  filter(time_threshold == t) %>%
  filter(amenity == a)

g12 <- ggplot(df, aes(x = x, y = y, color = factor(mode))) +
  theme_classic() +
  geom_line() +
  geom_vline(data = df2plot_raw,
    aes(xintercept = gap_md, color = factor(mode)),
    linetype = "dashed", alpha = 0.7
  ) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = scales::label_math()(c(-2, -1, 0, 1, 2, 3, 4))
  ) +
  scale_y_log10(
    breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1),
    labels = scales::label_math()(c(-5, -4, -3, -2, -1, 0))
  ) +
  labs(
    x = "Accessibility (home)",
    y = "Probability density (pdf)",
    color = "Mode"
  ) +
  scale_color_npg(name = "Transport mode",
                  breaks = c("Car", "Public transit"),
                  labels = c("Car", "Public transit")) +
  # scale_color_npg(name = "Amenity",
  #                 breaks = c("Essential needs", "Education", "Health services", "Social & Leisure"),
  #                 labels = c("Essential needs", "Education", "Health services", "Social & Leisure")) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    legend.position = "top"
  )

ggsave(filename = "figures/access_home_distr_en_15.png", plot = g12,
       width = 4, height = 3, unit = "in", dpi = 300)

# Basemaps ----
gdf <- st_read("results/activity_access_ind_area.gpkg")
ggmap::register_stadiamaps(key = "1ffbd641-ab9c-448b-8f83-95630d3c7ee3")
z.level <- 11
bbox <- st_bbox(gdf)
names(bbox) <- c("left", "bottom", "right", "top")
paris_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lines", zoom = z.level)

gdf2plot <- gdf %>%
  filter(amenity == "Essential needs") %>%
  filter(time_threshold == "15 min") %>%
  filter(mode == "Public transit") %>%
  drop_na(gap)

g2 <- ggmap(paris_basemap) +
  geom_sf(data = gdf2plot, aes(fill = gap),
          color = NA, size = 0, alpha = 0.7, 
          show.legend = T, inherit.aes = FALSE) +
  scale_fill_viridis(name = "Access disparity", trans = "log10") +
  # Add a scale bar
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.5) +  
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6),
    height = unit(0.8, "cm"),  # Adjust arrow height
    width = unit(0.8, "cm")    # Adjust arrow width
  ) +
  theme_void() +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 1))

ggsave(filename = "figures/access_disparity.png", plot = g2,
       width = 6, height = 4, unit = "in", dpi = 300)
