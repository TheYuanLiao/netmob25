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
library(magick)

# Basemaps ----
# Read your GeoJSON
gdf <- st_read("dbs/geo/paris_iris.geojson")

# Dissolve (aggregate) polygons by column `insee_com`
gdf_dissolved <- gdf %>%
  group_by(insee_com) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

metro <- st_read("dbs/geo/gtfs_routes.gpkg", layer = "metro")
tram  <- st_read("dbs/geo/gtfs_routes.gpkg", layer = "tram")
rail <- st_read("dbs/geo/gtfs_routes.gpkg", layer = "rail")
# bus  <- st_read("dbs/geo/gtfs_routes.gpkg", layer = "bus")
# 1) Combine transit layers with a label column
metro2 <- metro %>% mutate(Mode = "Metro")
tram2  <- tram  %>% mutate(Mode = "Tram")
rail2  <- rail  %>% mutate(Mode = "Rail")
lines  <- dplyr::bind_rows(metro2, tram2, rail2)

df.l1 <- read.csv("dbs/geo/paris_locs.csv")

ggmap::register_stadiamaps(key = "1ffbd641-ab9c-448b-8f83-95630d3c7ee3")
z.level <- 11
bbox <- st_bbox(gdf)
names(bbox) <- c("left", "bottom", "right", "top")
paris_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lines", zoom = z.level)

paris_ctr_box <- c(2.0172474628,48.6483631813,2.673535729,49.054069964)
bbox2plot <- data.frame(
  xmin = paris_ctr_box[1], xmax = paris_ctr_box[3],   # longitude range
  ymin = paris_ctr_box[2], ymax = paris_ctr_box[4]  # latitude range
)

# 2) Plot
g1 <- ggmap(paris_basemap) +
  # IRIS polygons (not in legend)
  geom_sf(data = gdf, fill = NA, color = "darkgray", size = 0.5,
          alpha = 0.8, inherit.aes = FALSE, show.legend = FALSE) +
  
  # Transit lines (legend by color)
  geom_sf(data = lines, aes(color = Mode),
          size = 0.05, alpha = 0.5, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(
    name = "Public transit",
    values = c(Metro = "#D81B60", Tram = "#1E88E5", Rail = "#ddcc77")
  ) +
    # ---- bounding box overlay ----
  annotate("rect",
           xmin = bbox2plot$xmin, xmax = bbox2plot$xmax,
           ymin = bbox2plot$ymin, ymax = bbox2plot$ymax,
           color = "darkgray", fill = NA, size = 0.5, linetype = "dashed") +
  # # Anchor locations as points (legend by shape)
  # geom_point(data = df.l1, aes(x = lon, y = lat, shape = Location),
  #            color = "black", size = 0.2, inherit.aes = FALSE) +
  # scale_shape_manual(
  #   name = "Anchor locations",
  #   values = c(`Work/Study` = 17, Home = 19) # triangle, circle
  # ) +
  
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.5) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6),
    height = unit(0.8, "cm"), width = unit(0.8, "cm")
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 2)),
         shape = guide_legend(nrow = 1))

ggsave(filename = "figures/paris_map.png", plot = g1, bg='white',
       width = 12, height = 10, units = "in", dpi = 300)

# Zoom-in
# 2) Download a new basemap for this bbox
zoom_map <- get_stadiamap(
  bbox = c(left  = paris_ctr_box[1],
           bottom= paris_ctr_box[2],
           right = paris_ctr_box[3],
           top   = paris_ctr_box[4]),
  maptype = "stamen_toner_lines", zoom = 13
)

# 3) Plot (reuse your layers)
g_zoom <- ggmap(zoom_map) +
  geom_sf(data = gdf, fill = NA, color = "darkgray", size = 0.2,
          alpha = 0.8, inherit.aes = FALSE, show.legend = FALSE) +
  geom_sf(data = lines, aes(color = Mode),
          size = 0.2, alpha = 0.7, inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(
    values = c(Metro = "#D81B60", Tram = "#1E88E5", Rail = "#ddcc77"),
    guide = "none"   # <--- kill the legend
  ) +
  geom_point(data = df.l1, aes(x = lon, y = lat),
             color = "black", size = 0.2, alpha = 0.5,
             inherit.aes = FALSE, show.legend = FALSE) +
  coord_sf(
    xlim = c(paris_ctr_box["xmin"], paris_ctr_box["xmax"]),
    ylim = c(paris_ctr_box["ymin"], paris_ctr_box["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.5) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6),
    height = unit(0.8, "cm"), width = unit(0.8, "cm")
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
    legend.position = "none",    # <--- turn off globally
    plot.title = element_text(hjust = 0.5)
  )

  ggsave(filename = "figures/paris_map_z.png", plot = g_zoom, bg='white',
       width = 5.5, height = 5, units = "in", dpi = 300)

# Load base and overlay images
base    <- image_read("figures/paris_map.png")
overlay <- image_read("figures/paris_map_z.png")

# Resize / position overlay (optional)
overlay <- image_scale(overlay, "1400x1400")   # shrink overlay
# place overlay 50 px right and 10 px down from the TOP-LEFT corner
result <- image_composite(
  base, overlay,
  operator = "over",
  gravity  = "southeast",        # anchor at top-left
  offset   = "+50+50"            # x,y offset in pixels
)


# Save / show
image_write(result, "figures/paris_region.png")
