# Session 4: Shading tricks

# https://art-from-code.netlify.app/day-1/session-4/

library(rayshader)
library(tibble)
library(ambient)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tictoc)
library(dplyr)

# Rayshader art

# The rayshader package is a tool used to generate 2D and 3D visualisations in R.

# To help get us started, I’ll build a very simple generative art system. 
# All it does is overlay a few circles on top of one another.

is_within_circle <- function(x_coord, y_coord, x_center, y_center, radius) {
  (x_coord - x_center)^2 + (y_coord - y_center)^2 < radius^2
}

# The additive_circles() function generates n circles at random

additive_circles <- function(n = 5, pixels = 1000, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  # setup canvas
  art <- long_grid(
    x = seq(0, 1, length.out = pixels),
    y = seq(0, 1, length.out = pixels)
  )
  art$paint <- 0
  
  for(i in 1:n) {
    
    # sample a random circle
    x_center <- runif(1, min = .3, max = .7)
    y_center <- runif(1, min = .3, max = .7)
    radius <- runif(1, min = .05, max = .25)
    
    # add +1 to all points inside the circle
    art <- art |>
      mutate(
        paint = paint + is_within_circle(
          x, y, x_center, y_center, radius
        )
      )
  }
  
  # normalise paint to [0, 1] range and return
  art$paint <- normalise(art$paint)
  return(art)
}



circle_art <- additive_circles(seed = 99)
circle_art


ggplot(circle_art, aes(x, y, fill = paint)) +
  geom_raster(show.legend = FALSE) + 
  theme_void()


circle_array <- circle_art |>
  as.array(value = paint) 

circle_array[1:10, 1:10]

circle_array |> 
  image(axes = FALSE, asp = 1, useRaster = TRUE)


circle_shadow <- ray_shade(
  heightmap = circle_array,
  sunaltitude = 15, 
  sunangle = 135,
  zscale = .01,
  multicore = TRUE
)

plot_map(circle_shadow, rotate = 270)


circle_scape <- circle_array |> 
  height_shade() |>
  add_shadow(
    shadowmap = circle_shadow,
    max_darken = .1
  )

plot_map(circle_scape, rotate = 270)


# Shadowed noise fields


sample_canva2 <- function(seed = NULL, n = 4) {
  
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

sample_canva2(seed = 1)
sample_canva2(seed = 1, n = 7)


# Ridge art


ridge_art <- function(seed = NULL, pixels = 2000) {
  
  if(!is.null(seed)) set.seed(seed)
  long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) |> 
    mutate(
      paint = fracture(
        x = x, 
        y = y,
        noise = gen_simplex,
        fractal = ridged,
        octaves = 8,
        frequency = 10,
        seed = seed
      ),
      paint = normalise(paint)
    ) |>
    as.array(value = paint)
}



ridge_art(1234) |> 
  image(
    axes = FALSE, 
    asp = 1, 
    useRaster = TRUE, 
    col = sample_canva2(seed = 1234, n = 256)
  ) 



shaded_ridge_art <- function(seed = NULL) {
  
  art <- ridge_art(seed) 
  height_shade(
    heightmap = art,
    texture = sample_canva2(seed, 256)
  ) |>
    add_shadow(
      shadowmap = ray_shade(
        heightmap = art, 
        sunaltitude = 30, 
        sunangle = 90,
        multicore = TRUE, 
        zscale = .05
      ), 
      max_darken = .1
    ) |>
    plot_map()
}


tic()
shaded_ridge_art(1234)
toc()


shaded_ridge_art(100)
shaded_ridge_art(101)
shaded_ridge_art(102) 


# Fractured terrain


transform_to_curl_space <- function(x, y, frequency = 1, octaves = 10) {
  curl_noise(
    generator = fracture,
    noise = gen_simplex,
    fractal = fbm,
    octaves = octaves,
    frequency = frequency,
    x = x,
    y = y
  )
}


define_worley_cells <- function(x, y, frequency = 3, octaves = 6) {
  fracture(
    noise = gen_worley,
    fractal = billow,
    octaves = octaves,
    frequency = frequency,
    value = "cell",
    x = x,
    y = y
  ) |>
    rank() |> 
    normalise()
}


simplex_noise <- function(x, y, frequency = .1, octaves = 10) {
  fracture(
    noise = gen_simplex,
    fractal = ridged,
    octaves = octaves,
    frequency = frequency,
    x = x,
    y = y
  ) |>
    normalise()
}



ice_floe <- function(seed) {
  
  set.seed(seed)
  
  grid <- long_grid(
    x = seq(0, 1, length.out = 2000),
    y = seq(0, 1, length.out = 2000)
  )
  
  coords <- transform_to_curl_space(grid$x, grid$y)
  
  grid |>
    mutate(
      cells = define_worley_cells(coords$x, coords$y),
      paint = simplex_noise(x + cells, y + cells),
      paint = normalise(paint)
    ) |>
    as.array(value = paint)
}



ice_floe <- function(seed) {
  
  set.seed(seed)
  
  grid <- long_grid(
    x = seq(0, 1, length.out = 2000),
    y = seq(0, 1, length.out = 2000)
  )
  
  coords <- transform_to_curl_space(grid$x, grid$y)
  
  grid |>
    mutate(
      cells = define_worley_cells(coords$x, coords$y),
      paint = simplex_noise(x + cells, y + cells),
      paint = normalise(paint)
    ) |>
    as.array(value = paint)
}



ice_floe(170) |> 
  image(
    axes = FALSE, 
    asp = 1, 
    useRaster = TRUE, 
    col = sample_canva2(seed = 170, n = 256)
  )



shaded_ice_floe <- function(seed) {
  
  art <- ice_floe(seed)
  
  height_shade(
    heightmap = art,
    texture = sample_canva2(seed, 256)
  ) |>
    add_shadow(
      shadowmap = ray_shade(
        heightmap = art, 
        sunaltitude = 30, 
        sunangle = 90,
        multicore = TRUE, 
        zscale = .005
      ), 
      max_darken = .05
    ) |>
    plot_map()
}

shaded_ice_floe(170)


shaded_ice_floe(100)
shaded_ice_floe(101)
shaded_ice_floe(102)



shaded_ice_floe(106)
shaded_ice_floe(107)
shaded_ice_floe(108)



# Three dimensional art


# To be reviewed
# https://art-from-code.netlify.app/day-1/session-4/


