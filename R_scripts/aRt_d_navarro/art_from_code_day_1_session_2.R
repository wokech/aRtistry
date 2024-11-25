# Session 2: Spatial noise tricks

# https://art-from-code.netlify.app/day-1/session-2/

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)

# Sampling spatial patterns

# Generative art relies on having access to a source of randomness, 
# and using that randomness to construct patterned objects.

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# R uses the pseudorandom number generator to 
# select a palette of four colours

sample_canva()
sample_canva()
sample_canva()

# Randomly generate tabular data structures

sample_cross_matrix <- function(n = 10, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  mat <- matrix(data = 0, nrow = n, ncol = n)
  mat[sample(n, 1), ] <- 1
  mat[, sample(n, 1)] <- 1
  return(mat)
}

sample_cross_matrix()

# Write functions to sample random spatial patterns

image(sample_cross_matrix(n = 50), axes = FALSE, useRaster = TRUE)
image(sample_cross_matrix(n = 50), axes = FALSE, useRaster = TRUE)
image(sample_cross_matrix(n = 50), axes = FALSE, useRaster = TRUE)

# Our first ambient artwork

#  If I want an 800x800 grid, I use the seq() function to define 
# a length-800 sequence of evenly-spaced numbers starting at 0 
# and ending at 1

x_coords <- seq(from = 0, to = 1, length.out = 800)
y_coords <- seq(from = 0, to = 1, length.out = 800)

canvas <- long_grid(x = x_coords, y = y_coords) 
canvas

# Generator functions in ambient package

# gen_waves() generates smooth concentric wave-like patterns
# gen_spheres() generates concentric circles
# gen_checkerboard() generates grids of squares in a checkerboard pattern

# gen_white() generates white noise: equal intensity at every spatial frequency

# gen_perlin() and gen_simplex() generate random “wavy” patterns
# gen_worley() generates random “cellular” patterns

gen_perlin(x = 1:5, y = 1, frequency = .001, seed = 1)
gen_perlin(x = 1:5, y = 1, frequency = .5, seed = 1)

canvas <- canvas |> 
  mutate(paint = gen_perlin(x, y, frequency = 10, seed = 1234))
canvas

art <- ggplot(canvas, aes(x, y, fill = paint)) + 
  geom_raster(show.legend = FALSE) 

art

art + 
  theme_void() +
  coord_equal()

art + 
  theme_void() +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = sample_canva())

# Our first system

# The art will always be a square grid rendered with geom_raster()
# The spatial pattern will always come from one of the ambient::gen_*() functions
# We’ll always remove extraneous elements from the art using theme_void() etc

# The colour palette could vary from piece to piece
# The underlying generator might be different in each piece
# The spatial frequency could be different in each piece
# The number of pixels in the grid could vary
# The random number generator seed could vary

make_noise_art <- function(
    generator = gen_perlin, 
    frequency = 10, 
    seed = 1234,
    pixels = 2000,
    palette = c("#e5ddc8", "#01949a", "#004369", "#db1f48"), 
    ...
) {
  
  # define the grid
  canvas <- long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) 
  
  # use the generator to add paint
  canvas <- canvas |>
    mutate(
      paint = generator(
        x, y, 
        frequency = frequency, 
        seed = seed, 
        ...
      )
    )
  
  # use ggplot2 to draw the picture
  art <- canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
  
  return(art)
}

# Varying seed changes the spatial pattern depicted in each piece, 
# but it doesn’t change it in any systematic way

make_noise_art(seed = 1234)
make_noise_art(seed = 1001)
make_noise_art(seed = 9999)

# When I change the frequency argument I get systematic variation. 
# The granularity of the spatial pattern changes in predictable 
# ways as the frequency changes

make_noise_art(frequency = 10)
make_noise_art(frequency = 20)
make_noise_art(frequency = 90)

# Vary the palette

make_noise_art(palette = c("white", "black"))
make_noise_art(palette = sample_canva(seed = 123))
make_noise_art(palette = sample_canva(seed = 456))

# We can vary the generator function

make_noise_art(generator = gen_perlin)
make_noise_art(generator = gen_worley)
make_noise_art(generator = gen_waves) 

# Why dplyr is a girls best friend

blank_canvas <- long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
) 

plot_painted_canvas <- function(canvas, palette = NULL) {
  if(is.null(palette)) {
    palette <- c("#e5ddc8","#01949a","#004369","#db1f48")
  }
  canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

blank_canvas |>
  mutate(paint = gen_perlin(x, y, frequency = 90, seed = 1234)) |>
  plot_painted_canvas()

blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1, seed = 1234),
    mf_noise = gen_simplex(x, y, frequency = 20, seed = 1234),
    hf_noise = gen_simplex(x, y, frequency = 99, seed = 1234),
    paint = lf_noise + mf_noise + hf_noise
  ) |>
  plot_painted_canvas()

blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1),
    mf_noise = gen_simplex(x, y, frequency = 20),
    hf_noise = gen_simplex(x, y, frequency = 99),
    gate = gen_spheres(x, y, frequency = 10) |> normalise(),
    paint = lf_noise +
      (1 + mf_noise) * (gate >= .1 & gate < .6) +
      (1 + hf_noise) * (gate >= .05)
  ) |>
  plot_painted_canvas(palette = sample_canva(seed = 2))

blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1),
    mf_noise = gen_simplex(x, y, frequency = 20),
    hf_noise = gen_simplex(x, y, frequency = 99),
    gate = gen_simplex(x, y, frequency = 10) |> normalise(),
    paint = lf_noise +
      (2 + mf_noise) * (gate >= .2 & gate < .8) +
      (2 + hf_noise) * (gate >= .1)
  ) |>
  plot_painted_canvas(palette = sample_canva(seed = 3))

# Fractals

# Suppose we have a “generator” function gen_sin() that generates 
# sinusoidal patterns with a particular frequency.

gen_sin <- function(x, frequency, ...) {
 sin(x * frequency)
}

fbm <- function(base, new, strength, ...) {
 base + new * strength
}

fracture(
  x = 1:20, 
  noise = gen_sin, 
  fractal = fbm, 
  octaves = 8
)

# In this code, octaves = 8 specifies the number of times 
# to apply the generator and fractal function.

dat <- tibble(
  x = seq(0, 10, length.out = 1000), 
  y1 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 1),
  y2 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 2),
  y8 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 8),
  y20 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 20)
) 

ggplot(dat) + geom_path(aes(x, y1)) + ggtitle("One iteration")
ggplot(dat) + geom_path(aes(x, y2)) + ggtitle("Two iterations")
ggplot(dat) + geom_path(aes(x, y8)) + ggtitle("Eight iterations")
ggplot(dat) + geom_path(aes(x, y20)) + ggtitle("Twenty iterations")



custom_fracture <- function(x, ...) {
  fracture(
    gain = function(strength) {strength * .8},
    frequency = function(frequency) {frequency * 1.3},
    noise = gen_sin, 
    fractal = fbm,
    x = x,
    ...
  )
}

dat <- tibble(
  x = seq(0, 10, length.out = 1000), 
  y1 = custom_fracture(x, octaves = 1),
  y2 = custom_fracture(x, octaves = 2),
  y8 = custom_fracture(x, octaves = 8),
  y20 = custom_fracture(x, octaves = 20)
) 

ggplot(dat) + geom_path(aes(x, y1)) + ggtitle("One iteration")
ggplot(dat) + geom_path(aes(x, y2)) + ggtitle("Two iterations")
ggplot(dat) + geom_path(aes(x, y8)) + ggtitle("Eight iterations")
ggplot(dat) + geom_path(aes(x, y20)) + ggtitle("Twenty iterations")


# To keep things simple and avoid the need to write plotting code 
# over and over, let’s define a fractal_art() function

fractal_art <- function(fractal, generator, palette = NULL, ...) {
  blank_canvas |>
    mutate(
      paint = fracture(
        noise = generator,
        fractal = fractal,
        x = x, 
        y = y, 
        ...
      )
    ) |>
    plot_painted_canvas(palette = palette)
}

# Here’s what happens when we use gen_checkerboard() as our 
# spatial pattern generator, and fbm() as our fractal function

fractal_art(fbm, gen_checkerboard, seed = 1, octaves = 1)
fractal_art(fbm, gen_checkerboard, seed = 1, octaves = 2)
fractal_art(fbm, gen_checkerboard, seed = 1, octaves = 20)

# As we increase the number of octaves the output contains 
# more copies of the “checker board” pattern, each one depicted 
# on a smaller scale than the last one.

fractal_art(fbm, gen_waves, seed = 1, octaves = 1)
fractal_art(fbm, gen_waves, seed = 1, octaves = 2)
fractal_art(fbm, gen_waves, seed = 1, octaves = 20)

# Simplex generator

blank_canvas |>
  mutate(paint = gen_simplex(x, y, seed = 2)) |>
  plot_painted_canvas()

# Combine gen_simplex() with the fbm() fractal function

fractal_art(fbm, gen_simplex, seed = 2, octaves = 1)
fractal_art(fbm, gen_simplex, seed = 2, octaves = 2)
fractal_art(fbm, gen_simplex, seed = 2, octaves = 20)

# The ridged() fractal function produces some very lovely patterns

fractal_art(ridged, gen_simplex, seed = 2, octaves = 1)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 2)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 20)

# It’s also possible to get nice effects by modifying the 
# gain and frequency functions.

# The strength of each successive iteration of ridged() noise

gf <- function(x) x * .8
fractal_art(ridged, gen_simplex, seed = 2, octaves = 1, gain = gf)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 2, gain = gf)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 20, gain = gf)

# Worley noise is an interesting case. The behaviour of 
# gen_worley() is to carve the image up in to distinct cells, 
# and colour each pixel in the image based on the cells they 
# belong to.

blank_canvas |>
  mutate(paint = gen_worley(x, y, seed = 6)) |>
  plot_painted_canvas()

fractal_art(billow, gen_worley, seed = 6, octaves = 1)
fractal_art(billow, gen_worley, seed = 6, octaves = 3)
fractal_art(billow, gen_worley, seed = 6, octaves = 8)

# However, gen_worley() allows also allows you to colour 
# the pixels in different ways.

blank_canvas |>
  mutate(paint = gen_worley(x, y, seed = 6, value = "distance")) |>
  plot_painted_canvas()

fractal_art(billow, gen_worley, seed = 6, octaves = 1, value = "distance")
fractal_art(billow, gen_worley, seed = 6, octaves = 3, value = "distance")
fractal_art(billow, gen_worley, seed = 6, octaves = 8, value = "distance")

# At the start of this section that’s exactly what I did in 
# one dimension with gen_sin().

gen_scope <- function(x, y, ...) {
  worley_cell <-gen_worley(x, y, value = "cell", ...)
  worley_dist <-gen_worley(x, y, value = "distance", ...)
  return(normalise(worley_cell) + 5 * normalise(worley_dist))
}

pal <- sample_canva(seed = 2)

blank_canvas |>
  mutate(paint = gen_scope(x, y, seed = 9)) |>
  plot_painted_canvas(palette = pal)

# I can now use my gen_scope() function as the 
# generator for my fractal

fractal_art(billow, gen_scope, palette = pal, seed = 9, octaves = 1)
fractal_art(billow, gen_scope, palette = pal, seed = 9, octaves = 2)
fractal_art(billow, gen_scope, palette = pal, seed = 9, octaves = 8)

#  The gen_gate() function below uses a “gating” mechanism

gen_gate <- function(x, y, frequency, ...) {
  lf <- gen_simplex(x, y, frequency = frequency, ...)
  mf <- gen_simplex(x, y, frequency = frequency * 20, ...)
  hf <- gen_simplex(x, y, frequency = frequency * 99, ...)
  gate <- gen_simplex(x, y, frequency = frequency * 10, ...) 
  gate <- normalise(gate)
  paint <- lf + 
    (mf + 2) * (gate >= .2 & gate < .8) + 
    (hf + 2) * (gate >= .1)
  return(paint)
}

pal <- sample_canva(seed = 3)

fractal_art(billow, gen_gate, palette = pal, seed = 9, octaves = 1)
fractal_art(billow, gen_gate, palette = pal, seed = 9, octaves = 2)
fractal_art(billow, gen_gate, palette = pal, seed = 9, octaves = 20)

# Curl (of a spatial) noise (pattern)

# Create a small grid of points

smol_grid <- long_grid(x = 1:20, y = 1:20)
ggplot(smol_grid) +
  geom_point(aes(x, y), colour = "black") + 
  theme_void() + 
  coord_equal()

# Create a small grid of points with various sizes

smol_simplex <- smol_grid |>
  mutate(z = gen_simplex(x, y, seed = 1, frequency = .1)) 

smol_simplex |>
  ggplot(aes(x, y, size = z)) +
  geom_point(colour = "black", show.legend = FALSE) + 
  theme_void() + 
  coord_equal()

smol_simplex |>
  ggplot(aes(x, y, z = z)) +
  geom_contour_filled(show.legend = FALSE, bins = 10) + 
  theme_void() + 
  coord_equal()

eps <- .001
smol_curl <- smol_grid |> mutate(
  x_add = gen_simplex(x + eps, y, seed = 1, frequency = .1),
  x_sub = gen_simplex(x - eps, y, seed = 1, frequency = .1),
  y_add = gen_simplex(x, y + eps, seed = 1, frequency = .1),
  y_sub = gen_simplex(x, y - eps, seed = 1, frequency = .1),
  x_slope = (x_add - x_sub) / (2 * eps), 
  y_slope = (y_add - y_sub) / (2 * eps),
  x_curl = -y_slope, 
  y_curl = x_slope
)

# Plot simplex noise field

ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x + x_slope * 2, 
      yend = y + y_slope * 2
    ), 
    colour = "gold2", 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()

# This map of arrows (a.k.a. vector field) depicts the slope at each
# point on the simplex noise surface. It’s a measure of how fast a 
# ball would start rolling if you placed it down at a particular spot.

ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x + x_curl * 2, 
      yend = y + y_curl * 2
    ), 
    colour = "orange", 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()


curl <- curl_noise(
  generator = gen_simplex,
  seed = 1,
  frequency = .1,
  x = smol_grid$x, 
  y = smol_grid$y
)


as_tibble(curl)


smol_grid |>
  mutate(
    x2 = x + curl$x * 2,
    y2 = y + curl$y * 2
  ) |> 
  ggplot() + 
  geom_segment(
    mapping = aes(x, y, xend = x2, yend = y2),
    colour = "navy", 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()


# Generative artists have a particular fondness for computing the 
# curl of a noise field and using it for nefarious purposes.

update_curl <- function(current_state, step_size = .0005, ...) {
  curl <- curl_noise(
    x = current_state$x, 
    y = current_state$y,
    ...
  )
  next_state <- current_state |>
    mutate(
      x = x + curl$x * step_size,
      y = y + curl$y * step_size,
      time = time + 1
    )
  return(next_state)
}

# Next, let’s define an initial state.

coords <- seq(0, 1, length.out = 50)
time_1 <- long_grid(x = coords, y = coords) |> 
  mutate(id = row_number(), time = 1)
time_1

# Now we can use our update_curl() function to compute 
# a new set of x and y values.

time_2 <- time_1 |>
  update_curl(
    generator = gen_simplex,
    frequency = 10, 
    seed = 1234
  )

time_3 <- time_2 |> 
  update_curl(
    generator = gen_simplex,
    frequency = 10, 
    seed = 1234
  )

time_3


# So let’s visualise these “curl updates”


dat12 <- bind_rows(time_1, time_2)
dat123 <- bind_rows(time_1, time_2, time_3)

dat12 |>
  ggplot(aes(x, y, group = id)) + 
  geom_path(colour = "brown") +
  theme_void() + 
  coord_equal()

dat123 |>
  ggplot(aes(x, y, group = id)) + 
  geom_path(colour = "purple") +
  theme_void() + 
  coord_equal() 

# Curl of a fractal pattern

# One nice thing about curl_noise() is that it can be 
# applied to any generator, including fracture().

curl_data <- function(
    data, 
    iterations = 50,
    step_size = .001,
    ...
) {
  
  update <- function(current_state, iteration, ...) {
    curl <- curl_noise(
      x = current_state$x, 
      y = current_state$y,
      generator = fracture,
      ...
    )
    next_state <- current_state |>
      mutate(
        x = x + curl$x * step_size,
        y = y + curl$y * step_size,
        time = time + 1
      )
    return(next_state)
  }
  
  data |> 
    mutate(id = row_number(), time = 1) |>
    accumulate(1:iterations, update, .init = _, ...) |>
    bind_rows()
}

curl_art <- function(...) {
  curl_data(...) |> 
    ggplot(aes(x, y, group = id)) + 
    geom_path(colour = "grey") +
    theme_void() + 
    coord_equal() 
}

# A grid of small fractal walks

smol_grid |>
  mutate(x = normalise(x), y = normalise(y)) |>
  curl_art(noise = gen_simplex, fractal = fbm, octaves = 4, freq_init = .5)

# An example where the initial points all lie on a circle

circle <- function(n = 100) {
  tibble(
    theta = 2 * pi * (1:n) / n, 
    x = cos(theta),
    y = sin(theta)
  )
}

curl_circle <- function(octaves) {
  curl_art(
    data = circle(500),
    iterations = 100, 
    noise = gen_simplex,
    fractal = fbm,
    octaves = octaves, 
    seed = 1, 
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    step_size = .003
  )
}

curl_circle(octaves = 1)
curl_circle(octaves = 3)
curl_circle(octaves = 8)

custom_curl_data <- function(data) {
  curl_data(
    data = data,
    iterations = 80, 
    octaves = 10,
    fractal = ridged,
    noise = gen_cubic,
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    seed = 1
  )
}

dat1 <- circle(5000) |> 
  custom_curl_data()

dat2 <- circle(5000) |>
  mutate(x = x * .99, y = y * .99) |>
  custom_curl_data()

ggplot(mapping = aes(x, y, group = time)) +
  geom_polygon(data = dat1, fill = "grey") +
  geom_polygon(data = dat2, fill = "grey2") +
  theme_void() + 
  coord_equal()
