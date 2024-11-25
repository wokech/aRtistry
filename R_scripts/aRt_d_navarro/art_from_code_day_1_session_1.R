# Session 1: Getting Started

# https://art-from-code.netlify.app/day-1/session-1/
# Use https://coolors.co/ to generate palettes

# A) Art is theft

library(ggplot2)
library(tibble)

# PLOT 1

# Look at the required dataset
mpg

# Create the plot

mpg |>
  ggplot(aes(displ, hwy, colour = drv)) + 
  geom_point()

mpg |>
  ggplot(aes(displ, hwy, colour = drv, size = cyl)) + 
  geom_point(show.legend = FALSE) + 
  theme_void() + 
  scale_color_brewer()

mpg |>
  ggplot(aes(displ, hwy, colour = drv)) + 
  geom_point(show.legend = FALSE, size = 4) + 
  geom_point(show.legend = FALSE, size = 1, colour = "#222222") + 
  coord_polar() + 
  theme_void() + 
  scale_color_brewer()

# PLOT 2

# Use runif() to generate data

set.seed(1)
n <- 50
dat <- tibble(
  x0 = runif(n),
  y0 = runif(n),
  x1 = x0 + runif(n, min = -.2, max = .2),
  y1 = y0 + runif(n, min = -.2, max = .2),
  shade = runif(n), 
  size = runif(n)
)

dat

# Fixed coordinates

dat |> 
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade,
    size = size
  )) +
  geom_segment(show.legend = FALSE) +
  coord_fixed() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_color_viridis_c() + 
  scale_size(range = c(0, 10)) + 
  theme_void()

# Cartesian coordinates

dat |> 
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade,
    size = size
  )) +
  geom_segment(show.legend = FALSE) +
  coord_cartesian() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_color_viridis_c(option = "plasma") + 
  scale_size(range = c(0, 10)) + 
  theme_void()

# Polar coordinates

dat |> 
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade,
    size = size
  )) +
  geom_segment(show.legend = FALSE) +
  coord_polar() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_color_viridis_c(option = "plasma") + 
  scale_size(range = c(0, 10)) + 
  theme_void()

# B) Technique

# The definition of an artistic system is a reusable concept

polar_art <- function(seed, n, palette) {
  
  # set the state of the random number generator
  set.seed(seed)
  
  # data frame containing random values for 
  # aesthetics we might want to use in the art
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n)
  )
  
  # plot segments in various colours, using 
  # polar coordinates and a gradient palette
  dat |> 
    ggplot(aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    theme_void()
}

polar_art(seed = 1, n = 500, palette = c("antiquewhite", "orange", "bisque"))
polar_art(seed = 1, n = 500, palette = c("red", "black", "white"))
polar_art(seed = 2, n = 50, palette = c("red", "black", "white"))

# C) Colour

library(scales)
library(ggthemes)

# Use https://coolors.co/ to generate palettes

pal <- c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff")

show_col(pal)

# Construct a continuous colour scale

palette_fn <- colorRampPalette(pal)
palette_fn(100)

# Image of a smooth palette

image(
  x = matrix(1:100, ncol = 1), 
  col = palette_fn(100),
  useRaster = TRUE,
  axes = FALSE
)

# Look at built-in palettes

canva_palettes[[101]]

show_col(canva_palettes[[101]])

# Sample palettes
# Samples one of these palettes at random

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

polar_art(seed = 2, n = 100, palette = sample_canva(seed = 2))
polar_art(seed = 2, n = 100, palette = sample_canva(seed = 3))
polar_art(seed = 2, n = 100, palette = sample_canva(seed = 4))


polar_art(seed = 5, n = 100, palette = sample_canva(seed = 1))
polar_art(seed = 6, n = 100, palette = sample_canva(seed = 1))
polar_art(seed = 7, n = 100, palette = sample_canva(seed = 1))

# D) Composition

# Create a random tibble generator function

sample_data <- function(seed = NULL, n = 100){
  if(!is.null(seed)) set.seed(seed)
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n),
    shape = factor(sample(0:22, size = n, replace = TRUE))
  )
}

polar_styled_plot <- function(data = NULL, palette) {
  ggplot(
    data = data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) + 
    coord_polar(clip = "off") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    theme_void() + 
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}


dat <- sample_data(n = 100, seed = 1) 
pal <- sample_canva(seed = 1)

polar_styled_plot(data = dat, palette = pal) + geom_segment()
polar_styled_plot(data = dat, palette = pal) + geom_path()
polar_styled_plot(data = dat, palette = pal) + geom_point()

library(dplyr)

dat1 <- sample_data(n = 2000, seed = 123) 
dat2 <- sample_data(n = 100, seed = 456) |>  
  mutate(y0 = .3 + y0 * .6, y1 = .3)

polar_styled_plot(palette = sample_canva(seed = 7)) + 
  geom_segment(
    data = dat1 |> mutate(size = size * 3)
  ) + 
  geom_segment(
    data = dat2 |> mutate(size = size / 5), 
    lineend = "round", 
    colour = "white"
  ) +
  geom_segment(
    data = dat2 |> mutate(size = size / 40), 
    lineend = "round", 
    colour = "#222222"
  ) +
  geom_point(
    data = dat2 |> mutate(size = size * 2),
    colour = "#222222"
  )

dat <- sample_data(n = 2000, seed = 123) |>
  mutate(y1 = y0, size = size / 2)

polar_styled_plot(palette = sample_canva(seed = 456)) + 
  geom_segment(data = dat) + 
  geom_segment(data = dat |> mutate(y1 = y1 - .2, y0 = y0 - .2)) +
  geom_segment(data = dat |> mutate(y1 = y1 - .4, y0 = y0 - .4))

dat <- sample_data(n = 1000, seed = 1) |>
  mutate(y1 = y0, size = size / 4)

polar_styled_plot(palette = sample_canva(seed = 2)) + 
  geom_segment(data = dat, linetype = "331311") 
