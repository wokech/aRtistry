# Session 3: Pixel Filters

# https://art-from-code.netlify.app/day-2/session-3/

library(dplyr)
library(tibble)
library(ggplot2)
library(ggforce)
library(ggfx)
library(flametree)
library(ambient)

# The flametree_grow() function generates the raw data:

tree <- flametree_grow(
  seed = 1, 
  time = 9, 
  angle = c(-15, 15, 30)
)
tree

# You can render the output conveniently with the help of flametree_plot():

tree |> 
  flametree_plot(
    background = "#222222", 
    palette = c("#ffffff", "#f652a0")
  )

tree |> filter(path_id == 99)

leaf <- tree |> filter(id_leaf == TRUE)

base <- ggplot() + 
  scale_size_identity() + 
  theme_void() + 
  coord_equal()

# First, let’s create a layer representing the leaves. 

leaves <- geom_point(
  mapping = aes(coord_x, coord_y),
  data = leaf, 
  size = 1.3, 
  stroke = 0, 
  colour = "#e38b75"
)

# Trunk

trunk <- geom_bezier(
  mapping = aes(coord_x, coord_y, group = id_pathtree, size = seg_wid),
  data = tree, 
  lineend = "round", 
  colour = "#555555",
  show.legend = FALSE
)

base + trunk + leaves

# Glow

base + trunk + leaves

base +   
  trunk + 
  with_outer_glow(leaves, colour = "white")

base +   
  trunk + 
  with_outer_glow(leaves, colour = "white", sigma = 5, expand = 3)


# Dither


# The ggfx package supplies several dithering filters, including:

# with_dither() uses Floyd-Steinberg dithering
# with_halftone_dither() uses halftone dots
# with_ordered_dither() uses ordered dithering
# with_custom_dither() lets you build your own!

set.seed(1)
polar <- tibble(
  arc_start = runif(200),
  arc_end = arc_start + runif(200, min = -.2, max = .2),
  radius = runif(200),
  shade = runif(200), 
  size = runif(200)
)

base <- ggplot(
  data = polar, 
  mapping = aes(
    x = arc_start, 
    y = radius,
    xend = arc_end, 
    yend = radius, 
    colour = shade, 
    size = size
  )
) + 
  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep) +
  scale_x_continuous(limits = c(0, 1), oob = scales::oob_keep) + 
  scale_colour_viridis_c(option = "magma") +
  guides(colour = guide_none(), size = guide_none()) + 
  scale_size(range = c(0, 10)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#aaaaaa"))


base + geom_segment()
base + with_dither(geom_segment(), max_colours = 5)


base + with_halftone_dither(geom_segment())
with_halftone_dither(base + geom_segment())


# Mask

texture <- geom_raster(
  mapping = aes(x, y, fill = paint),
  data = long_grid(
    x = seq(from = -1, to = 1, length.out = 1000),
    y = seq(from = -1, to = 1, length.out = 1000)
  ) |> 
    mutate(
      lf_noise = gen_simplex(x, y, frequency = 2, seed = 1234),
      mf_noise = gen_simplex(x, y, frequency = 20, seed = 1234),
      hf_noise = gen_simplex(x, y, frequency = 99, seed = 1234),
      paint = lf_noise + mf_noise + hf_noise
    )
)

hex <- tibble(x = sin((0:6)/6 * 2 * pi), y = cos((0:6)/6 * 2 * pi))
mask <- geom_polygon(aes(x, y), hex, fill = "white")

base <- ggplot() + 
  theme_void() +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colours = c("#222222","#e83e8c"), 
    guide = guide_none()
  )


base + texture
base + mask


base + 
  as_reference(mask, id = "mask") + 
  with_mask(texture, "mask")


base + with_mask(texture, mask)


border <- geom_path(aes(x, y), hex, colour = "white", size = 15)

text <- geom_text(
  mapping = aes(x, y, label = text), 
  dat = tibble(x = 0, y = 0, text = "ART"), 
  size = 36,
  colour = "white", 
  fontface = "bold"
) 


base + texture + text + border
base + 
  as_group(texture, text, border, id = "content") +
  as_reference(mask, id = "mask") + 
  with_mask("content", "mask")


# Displace

polygon_layer <- function(x, y, fill = "white", alpha = .5) {
  geom_polygon(aes(x, y), fill = fill, alpha = alpha)
}
poly1 <- polygon_layer(x = c(1, 0, 0), y = c(0, 0, 1))
poly2 <- polygon_layer(x = c(0, 1, 1), y = c(0, 0, 1))
poly3 <- polygon_layer(x = c(.3, 1, 1), y = c(0, 0, .7))
poly4 <- polygon_layer(x = c(0, 0, .7), y = c(.3, 1, 1))


base <- ggplot() + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_void() + 
  theme(panel.background = element_rect(fill = "#333333"))

base + poly1 + poly2 + poly3 + poly4


text <- geom_text(
  mapping = aes(0.5, 0.5, label = "ART"), 
  size = 60, 
  colour = "black", 
  fontface = "bold"
)


base + poly1 + poly2 + poly3 + poly4 + text
base + 
  as_group(poly1, poly2, poly3, poly4, id = "polygons", include = TRUE) +
  as_reference("polygons", id = "displacement_map") + 
  with_displacement(
    text,
    x_map = ch_alpha("displacement_map"),
    y_map = ch_alpha("displacement_map"), 
    x_scale = 150,
    y_scale = -150
  )


# Blend


leaves <- geom_point(
  data = leaf, 
  mapping = aes(coord_x, coord_y, colour = seg_col),
  colour = "white",
  size = 2, 
  stroke = 0
)

trunk <- geom_bezier(
  data = tree,
  mapping = aes(
    x = coord_x, 
    y = coord_y, 
    size = seg_wid,
    group = id_pathtree
  ),
  colour = "white",
  lineend = "round"
)


triangle <- polygon_layer(
  x = c(-4, 2, 2), 
  y = c(0, 0, 6), 
  fill = "white",
  alpha = 1
)


base <- ggplot() + 
  theme_void() +
  coord_equal(xlim = c(-3, 1), ylim = c(1, 5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_identity(guide = guide_none())

base +
  as_group(trunk, leaves, id = "tree") + 
  with_blend(triangle, "tree", blend_type = "xor")


