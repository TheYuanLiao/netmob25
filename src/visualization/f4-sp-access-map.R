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
library(ggpubr)
options(scipen = 999)   # higher penalty against scientific notation

# Basemaps ----
gdf <- st_read("results/h3_s7_ak_tc.gpkg")
ggmap::register_stadiamaps(key = "1ffbd641-ab9c-448b-8f83-95630d3c7ee3")
z.level <- 11
bbox <- st_bbox(gdf)
names(bbox) <- c("left", "bottom", "right", "top")
paris_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lines", zoom = z.level)


g1 <- ggmap(paris_basemap) +
  geom_sf(data = gdf, aes(fill = ak_non_zero),
          color = NA, size = 0, alpha = 0.7, 
          show.legend = T, inherit.aes = FALSE) +
  scale_fill_viridis(name = "% inhabitants w/ SPA > 0", trans = "log10") +
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

g2 <- ggmap(paris_basemap) +
  geom_sf(data = gdf, aes(fill = tc_non_zero),
          color = NA, size = 0, alpha = 0.7, 
          show.legend = T, inherit.aes = FALSE) +
  scale_fill_viridis(name = "% inhabitants w/ trip chaining", trans = "log10") +
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

g3 <- ggmap(paris_basemap) +
  geom_sf(data = gdf, aes(fill = entropy_mm_norm),
          color = NA, size = 0, alpha = 0.7, 
          show.legend = T, inherit.aes = FALSE) +
  scale_fill_viridis(name = "Third activity entropy", trans = "log10") +
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

g4 <- ggmap(paris_basemap) +
  geom_sf(data = gdf, aes(fill = activity_time_third),
          color = NA, size = 0, alpha = 0.7, 
          show.legend = T, inherit.aes = FALSE) +
  scale_fill_viridis(name = "Third activity time (min)", trans = "log10") +
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

# g4 <- ggplot(data=gdf, aes(x=ak_non_zero, y=tc_non_zero)) +
#     geom_point(color='steelblue', alpha=0.7) + 
#     geom_abline(intercept = 0, slope = 1, color="green", linetype="dashed") +
#     labs(x='% inhabitants w/ SPA > 0',
#          y='% inhabitants w/ trip chaining') +
#     theme_classic()

# g5 <- ggplot(data=gdf, aes(x=ak_non_zero, y=entropy_mm)) +
#     geom_point(color='steelblue', alpha=0.7) + 
#     # geom_abline(intercept = 0, slope = 1, color="green", linetype="dashed") +
#     labs(x='% inhabitants w/ SPA > 0',
#          y='Third activity entropy') +
#    theme_classic()

#g <- ggarrange(g1, g2, ncol = 2, nrow = 1, labels = c('a', 'b'))
# g0 <- ggarrange(g4, g5, ncol = 2, nrow = 1, labels = c('d', 'e'))
#g1 <- ggarrange(g3, g0, ncol = 2, nrow = 1, labels = c('c', ''))
G <- ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2)
ggsave(filename = "figures/ak_tc_map.png", plot = G, bg='white',
       width = 12, height = 10, unit = "in", dpi = 300)
