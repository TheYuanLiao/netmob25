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