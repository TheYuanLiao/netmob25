library(magick)

read.img <- function(path, lb, size = 60, pdf = FALSE) {
  if (pdf) {
    image <- image_read_pdf(path)
  } else {
    image <- image_read(path)
  }

  # Create a label image
  label_img <- image_blank(width = image_info(image)$width, height = size + 10, color = "white") %>%
    image_annotate(lb, gravity = "northwest", location = "+10+10", color = "black", size = size, weight = 700)

  # Stack label image on top of the actual image
  final_img <- image_append(c(label_img, image), stack = TRUE)
  return(final_img)
}

# Figure 2 (updated) ----
image1 <- read.img(path="figures/total_travel_time_spa.png", lb='a')
image2 <- read.img(path="figures/hill_q1_spa.png", lb='')
image3 <- read.img(path="figures/total_travel_time.png", lb='b')
image4 <- read.img(path="figures/hill_q1.png", lb='c')

## Combine images 1-3 & 4-5
# Get height of image 3
image1_w <- image_info(image1)$width

# Create blank space between them and stack three
blank_space_h <- image_blank(image1_w, 2, color = "white")

combined_image1 <- image_append(c(image1, blank_space_h, image2), stack = T)
combined_image1_h <- image_info(combined_image1)$height

blank_space_h2 <- image_blank(2, combined_image1_h, color = "white")

image3_w <- image_info(image3)$width

# Create blank space between them and stack three
blank_space_w <- image_blank(image3_w, 2, color = "white")

combined_image2 <- image_append(c(image3, blank_space_w, image4), stack = T)

# Now stack all three images vertically
final_figure <- image_append(c(combined_image1, blank_space_h2, combined_image2), stack = F)
image_write(final_figure, "figures/figure2.png")

# Figure 2----
image1 <- read.img(path="figures/trip_chaining_presence_spa.png", lb='a')
image2 <- read.img(path="figures/xs_total_hws_spa.png", lb='')
image3 <- read.img(path="figures/hill_q1_spa.png", lb='')
image31 <- read.img(path="figures/activity_time_third_spa.png", lb='')
image4 <- read.img(path="figures/trip_chaining_presence.png", lb='b')
image5 <- read.img(path="figures/xs_total_hws.png", lb='c')
image6 <- read.img(path="figures/hill_q1.png", lb='d')
image61 <- read.img(path="figures/activity_time_third.png", lb='e')

## Combine images 1-3 & 4-5
# Get height of image 3
image1_w <- image_info(image1)$width

# Create blank space between them and stack three
blank_space_h <- image_blank(image1_w, 2, color = "white")

combined_image1 <- image_append(c(image1, blank_space_h, image2, blank_space_h, image3, blank_space_h, image31), stack = T)
combined_image1_h <- image_info(combined_image1)$height

blank_space_h2 <- image_blank(2, combined_image1_h, color = "white")

image4_w <- image_info(image4)$width

# Create blank space between them and stack three
blank_space_w <- image_blank(image4_w, 2, color = "white")

combined_image2 <- image_append(c(image4, blank_space_w, image5, blank_space_w, image6, blank_space_w, image61), stack = T)

# Now stack all three images vertically
final_figure <- image_append(c(combined_image1, blank_space_h2, combined_image2), stack = F)
image_write(final_figure, "figures/figure2.png")

# Figure 3----
image1 <- read.img(path="figures/trip_chaining_presence_coeff.png", lb='a')
image2 <- read.img(path="figures/xs_total_hws_coeff.png", lb='b')
image3 <- read.img(path="figures/entropy_mm_coeff.png", lb='c')

## Combine images 1-3 & 4-5
# Get height of image 3
image1_w <- image_info(image1)$width

# Create blank space between them and stack three
blank_space_h <- image_blank(image1_w, 2, color = "white")

combined_image1 <- image_append(c(image1, blank_space_h, image2), stack = T)
combined_image1_h <- image_info(combined_image1)$height

blank_space_h2 <- image_blank(2, combined_image1_h, color = "white")


# Now stack all three images vertically
final_figure <- image_append(c(combined_image1, blank_space_h2, image3), stack = F)
image_write(final_figure, "figures/figure3.png")

# Figure selectivity----
image1 <- read.img(path = "figures/selectivity_excluded_share.png", lb = "a")
image2 <- read.img(path = "figures/selectivity_effect_size.png",  lb = "b")
image3 <- read.img(path = "figures/selectivity_active_mode.png", lb = "c")

# ---- 1) Make image2 & image3 the same height and stitch side-by-side ----
info2 <- image_info(image2)
info3 <- image_info(image3)

# target height = min of the two (keeps them crisp)
h_target <- min(info2$height, info3$height)

image2_s <- image_scale(image2, paste0("x", h_target))  # scale by height
image3_s <- image_scale(image3, paste0("x", h_target))

bottom_row <- image_append(c(image2_s, image3_s), stack = FALSE)  # horizontal

# ---- 2) Pad the narrower row so widths match before vertical append ----
w_top    <- image_info(image1)$width
w_bottom <- image_info(bottom_row)$width

if (w_top < w_bottom) {
  # pad top to match bottom width
  image1_padded <- image_extent(
    image1,
    geometry = paste0(w_bottom, "x", image_info(image1)$height),
    color    = "white",
    gravity  = "center"
  )
  bottom_row_padded <- bottom_row
} else if (w_bottom < w_top) {
  # pad bottom to match top width
  bottom_row_padded <- image_extent(
    bottom_row,
    geometry = paste0(w_top, "x", image_info(bottom_row)$height),
    color    = "white",
    gravity  = "center"
  )
  image1_padded <- image1
} else {
  image1_padded     <- image1
  bottom_row_padded <- bottom_row
}

# ---- 3) Stack vertically: image1 on top, (image2 | image3) below ----
final_figure <- image_append(c(image1_padded, bottom_row_padded), stack = TRUE)

image_write(final_figure, "figures/selectivity.png")

# Figure sensitivity ----
img_a <- read.img(path = "figures/sensitivity_budget_distribution.png", lb = "a")
img_b <- read.img(path = "figures/sensitivity_budget_nonzero.png", lb = "b")
img_c <- read.img(path = "figures/sensitivity_hour_pt.png", lb = "c")

# Row 1: a + b side by side (match heights)
info_a <- image_info(img_a)
info_b <- image_info(img_b)
h_target <- min(info_a$height, info_b$height)

img_a_s <- image_scale(img_a, paste0("x", h_target))
img_b_s <- image_scale(img_b, paste0("x", h_target))

top_row <- image_append(c(img_a_s, img_b_s), stack = FALSE)

# Row 2: c (pad to match top row width)
w_top <- image_info(top_row)$width
w_c <- image_info(img_c)$width

if (w_c < w_top) {
  img_c_padded <- image_extent(img_c, geometry = paste0(w_top, "x", image_info(img_c)$height), color = "white", gravity = "center")
} else {
  img_c_padded <- img_c
  top_row <- image_extent(top_row, geometry = paste0(w_c, "x", image_info(top_row)$height), color = "white", gravity = "center")
}

# Stack vertically
final_sensitivity <- image_append(c(top_row, img_c_padded), stack = TRUE)
image_write(final_sensitivity, "figures/sensitivity.png")
message("Saved figures/sensitivity.png")

# Figure 6: Leisure participation outcomes ----
img_a <- read.img(path = "figures/hill_q1.png", lb = "a", size = 50)
img_b <- read.img(path = "figures/mean_leisure_duration.png", lb = "b", size = 50)

# Match widths
w_a <- image_info(img_a)$width
w_b <- image_info(img_b)$width
w_max <- max(w_a, w_b)

img_a_padded <- image_extent(img_a, geometry = paste0(w_max, "x", image_info(img_a)$height), color = "white", gravity = "center")
img_b_padded <- image_extent(img_b, geometry = paste0(w_max, "x", image_info(img_b)$height), color = "white", gravity = "center")

# Stack vertically
final_figure6 <- image_append(c(img_a_padded, img_b_padded), stack = TRUE)
image_write(final_figure6, "figures/figure7.png")
message("Saved figures/figure7.png")

# Figure SEM sensitivity: 60min and 120min diagrams side by side ----
img_a <- read.img(path = "results/sem_sensitivity/budget_60min/sem_diagram.png", lb = "a", size = 100)
img_b <- read.img(path = "results/sem_sensitivity/budget_120min/sem_diagram.png", lb = "b", size = 100)

# Match heights
info_a <- image_info(img_a)
info_b <- image_info(img_b)
h_target <- min(info_a$height, info_b$height)

img_a_s <- image_scale(img_a, paste0("x", h_target))
img_b_s <- image_scale(img_b, paste0("x", h_target))

# Append side by side
final_sem_sens <- image_append(c(img_a_s, img_b_s), stack = FALSE)
image_write(final_sem_sens, "figures/sem_sensitivity.png")
message("Saved figures/sem_sensitivity.png")

# Figure: Travel time budget and departure time side by side ----
img_a <- read.img(path = "figures/travel_time_budget.png", lb = "a")
img_b <- read.img(path = "figures/work_departure_time.png", lb = "b")

# Match heights
info_a <- image_info(img_a)
info_b <- image_info(img_b)
h_target <- min(info_a$height, info_b$height)

img_a_s <- image_scale(img_a, paste0("x", h_target))
img_b_s <- image_scale(img_b, paste0("x", h_target))

# Append side by side
final_tt_dep <- image_append(c(img_a_s, img_b_s), stack = FALSE)
image_write(final_tt_dep, "figures/sta_parameters.png")
message("Saved figures/sta_parameters.png")