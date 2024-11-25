# Session 3: Polygon Tricks

# https://art-from-code.netlify.app/day-1/session-3/

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(tictoc)
library(ggthemes)
library(gifski)

# Semi-transparent polygons

# A commonly used trick in generative art is to simulate graded 
# textures by plotting many slightly-different and mostly-transparent 
# polygons over the top of one another.

square <- tibble(
  x = c(0, 1, 1, 0, 0),
  y = c(0, 0, 1, 1, 0),
  seg_len = c(1, 1, 1, 1, 0)
)


show_polygon <- function(polygon, show_vertices = TRUE, ...) {
  
  pic <- ggplot(polygon, aes(x, y)) +
    geom_polygon(colour = "black", fill = NA, show.legend = FALSE, ...) + 
    coord_equal() + 
    theme_void()
  
  if(show_vertices == TRUE) {
    pic <- pic + geom_point(colour = "black", size = 2)
  }
  return(pic)
}

show_polygon(square)

# The next step in our process is to think about ways 
# that we can deform this polygon.

sample_edge <- function(polygon) {
  sample(nrow(polygon), 1, prob = polygon$seg_len)
}

# Next step is to realise that if we break an edge into two 
# edges, we’ll need to compute the length of these two new edges

edge_length <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Add size argument

edge_noise <- function(size) {
  runif(1, min = -size/2, max = size/2)
}


insert_edge <- function(polygon, noise) {
  
  # sample and edge and remember its length
  ind <- sample_edge(polygon)
  len <- polygon$seg_len[ind]
  
  # one endpoint of the old edge
  last_x <- polygon$x[ind]
  last_y <- polygon$y[ind]
  
  # the other endpoint of the old edge
  next_x <- polygon$x[ind + 1]
  next_y <- polygon$y[ind + 1]
  
  # location of the new point to be inserted: noise 
  # is scaled proportional to the length of the old edge
  new_x <- (last_x + next_x) / 2 + edge_noise(len * noise)
  new_y <- (last_y + next_y) / 2 + edge_noise(len * noise)
  
  # the new row for insertion into the tibble, 
  # containing coords and length of the 'new' edge
  new_row <- tibble(
    x = new_x,
    y = new_y,
    seg_len = edge_length(new_x, new_y, next_x, next_y)
  )
  
  # update the length of the 'old' edge
  polygon$seg_len[ind] <- edge_length(
    last_x, last_y, new_x, new_y
  )
  
  # insert a row into the tibble
  bind_rows(
    polygon[1:ind, ],
    new_row,
    polygon[-(1:ind), ]
  )
}

# Here’s the function in action

set.seed(2)
polygon <- square 
polygon <- insert_edge(polygon, noise = .5); show_polygon(polygon)
polygon <- insert_edge(polygon, noise = .5); show_polygon(polygon)
polygon <- insert_edge(polygon, noise = .5); show_polygon(polygon)

# Grow the polygon

grow_polygon <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  for(i in 1:iterations) polygon <- insert_edge(polygon, noise)
  return(polygon)
}

square |> 
  grow_polygon(iterations = 30, noise = .5, seed = 2) |> 
  show_polygon(show_vertices = FALSE)
square |> 
  grow_polygon(iterations = 100, noise = .5, seed = 2) |> 
  show_polygon(show_vertices = FALSE)
square |> 
  grow_polygon(iterations = 1000, noise = .5, seed = 2) |> 
  show_polygon(show_vertices = FALSE)


grow_multipolygon <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon(base_shape, ...)
  }
  polygons <- bind_rows(polygons, .id = "id")
  polygons
}

show_multipolygon <- function(polygon, fill, alpha = .02, ...) {
  ggplot(polygon, aes(x, y, group = id)) +
    geom_polygon(colour = NA, alpha = alpha, fill = fill, ...) + 
    coord_equal() + 
    theme_void()
}


tic()
dat <- square |> 
  grow_polygon(iterations = 100, noise = .5, seed = 2) |>
  grow_multipolygon(n = 50, iterations = 1000, noise = 1, seed = 2)
toc()


show_multipolygon(dat, fill = "#d43790")

# Growing polygons faster

square_l <- transpose(square)

sample_edge_l <- function(polygon) {
  sample(length(polygon), 1, prob = map_dbl(polygon, ~ .x$seg_len))
}

insert_edge_l <- function(polygon, noise) {
  
  ind <- sample_edge_l(polygon)
  len <- polygon[[ind]]$seg_len
  
  last_x <- polygon[[ind]]$x
  last_y <- polygon[[ind]]$y
  
  next_x <- polygon[[ind + 1]]$x
  next_y <- polygon[[ind + 1]]$y
  
  new_x <- (last_x + next_x) / 2 + edge_noise(len * noise)
  new_y <- (last_y + next_y) / 2 + edge_noise(len * noise)
  
  new_point <- list(
    x = new_x,
    y = new_y,
    seg_len = edge_length(new_x, new_y, next_x, next_y)
  )
  
  polygon[[ind]]$seg_len <- edge_length(
    last_x, last_y, new_x, new_y
  )
  
  c(
    polygon[1:ind],
    list(new_point),
    polygon[-(1:ind)]
  )
}

grow_polygon_l <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  for(i in 1:iterations) polygon <- insert_edge_l(polygon, noise)
  return(polygon)
}

grow_multipolygon_l <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon_l(base_shape, ...) |>
      transpose() |>
      as_tibble() |>
      mutate(across(.fn = unlist))
  }
  polygons <- bind_rows(polygons, .id = "id")
  polygons
}


tic()
dat <- square_l |> 
  grow_polygon_l(iterations = 100, noise = .5, seed = 2) |>
  grow_multipolygon_l(n = 50, iterations = 1000, noise = 1, seed = 2) 
toc()


show_multipolygon(dat, fill = "#d43790")

# Using the method: splotches

# With that in mind, the splotch() function below wraps a slightly cruder version 
# of the method than the one I showed earlier. It generates fewer polygons, and 
# those polygons have fewer vertices.

splotch <- function(seed, layers = 10) {
  set.seed(seed)
  square_l <- transpose(tibble(
    x = c(0, 1, 1, 0, 0),
    y = c(0, 0, 1, 1, 0),
    seg_len = c(1, 1, 1, 1, 0)
  ))
  square_l |> 
    grow_polygon_l(iterations = 10, noise = .5, seed = seed) |>
    grow_multipolygon_l(n = layers, iterations = 500, noise = .8, seed = seed) 
}



tic()
splotch_1 <- splotch(seed = 12) 
splotch_2 <- splotch(seed = 34)
splotch_3 <- splotch(seed = 56)
splotch_4 <- splotch(seed = 78)
toc()


show_multipolygon(splotch_1, "#f51720", alpha = .2)
show_multipolygon(splotch_2, "#f8d210", alpha = .2)
show_multipolygon(splotch_3, "#059dc0", alpha = .2)
show_multipolygon(splotch_4, "#81b622", alpha = .2)

# Using the method: Smudged hexagons

# The goal of splotch() is to have a tool we can play around with and explore 
# the method. That’s nice and all, but can we also use the method to 
# make something fun?

smudged_hexagon <- function(seed, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)
  
  # define hexagonal base shape
  theta <- (0:6) * pi / 3
  hexagon <- tibble(
    x = sin(theta),
    y = cos(theta),
    seg_len = edge_length(x, y, lead(x), lead(y))
  )
  hexagon$seg_len[7] <- 0
  hexagon <- transpose(hexagon)
  base <- hexagon |> 
    grow_polygon_l(
      iterations = 60, 
      noise = noise1
    )
  
  # define intermediate-base-shapes in clusters
  polygons <- list()
  ijk <- 0
  for(i in 1:3) {
    base_i <- base |> 
      grow_polygon_l(
        iterations = 50, 
        noise = noise2
      )
    
    for(j in 1:3) {
      base_j <- base_i |> 
        grow_polygon_l(
          iterations = 50, 
          noise = noise2
        )
      
      # grow 10 polygons per intermediate-base
      for(k in 1:10) {
        ijk <- ijk + 1
        polygons[[ijk]] <- base_j |>
          grow_polygon_l(
            iterations = 500, 
            noise = noise3
          ) |>
          transpose() |>
          as_tibble() |>
          mutate(across(.fn = unlist))
      }
    }
  }
  
  # return as data frame
  bind_rows(polygons, .id = "id")
}


tic()
dat <- smudged_hexagon(seed = 1)
toc()


dat |> show_multipolygon(fill = "#d4379005")


smudged_hexagon(seed = 11) |> show_multipolygon(fill = "#d4379005")
smudged_hexagon(seed = 44) |> show_multipolygon(fill = "#d4379005")
smudged_hexagon(seed = 88) |> show_multipolygon(fill = "#d4379005") 


dat <- bind_rows(
  smudged_hexagon(seed = 11),
  smudged_hexagon(seed = 22),
  smudged_hexagon(seed = 44),
  smudged_hexagon(seed = 66),
  smudged_hexagon(seed = 88),
  .id = "source"
) |>
  mutate(
    id = paste(id, source),
    x = x + as.numeric(source)
  ) |>
  arrange(id)

ggplot(dat, aes(x, y, group = id, fill = factor(source))) +
  geom_polygon(alpha = .02, show.legend = FALSE) + 
  theme_void() + 
  scale_fill_manual(values = c(
    "#000000", "#FFFFFF", "#BB0000", "#FFFFFF", "#006600"
  )) +
  coord_equal() 



# Slightly misshapen objects

# If I have a vector describing the angle around circle from 0 to 2, I can 
# compute the x- and y-coordinates for a heart shape using these functions:

heart_x <- function(angle) {
  x <- (16 * sin(angle) ^ 3) / 17
  return(x - mean(x))
}

heart_y <- function(angle) {
  y <- (13 * cos(angle) - 5 * cos(2 * angle) - 2 * cos(3 * angle) -
          cos(4 * angle)) / 17
  return(y - mean(y))
}


# Here’s what it looks like when I draw a heart using these formulas:
  
heart_shape <- tibble(
  angle = seq(0, 2 * pi, length.out = 50),
  x = heart_x(angle),
  y = heart_y(angle)
)
show_polygon(heart_shape)


# Perlin blobs


# Let’s start with a slightly simpler version of the problem: instead of 
# deforming a heart shape we’ll deform a circle using Perlin noise. 
# Our base shape is a circle that looks like this:
  
  circle <- tibble(
    angle = seq(0, 2*pi, length.out = 50),
    x = cos(angle),
    y = sin(angle)
  )
show_polygon(circle)


# In any case, after computing the (Perlin-noise distorted) radius associated 
# with each coordinate, we compute the final x and y values for the 
# “Perlin blob” by multiplying the coordinates of the base shape by the radius.

normalise_radius <- function(x, min, max) {
  normalise(x, from = c(-0.5, 0.5), to = c(min, max))
}

perlin_blob <- function(n = 100, 
                        freq_init = 0.3,
                        octaves = 2, 
                        r_min = 0.5, 
                        r_max = 1) {
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    x_base = cos(angle),
    y_base = sin(angle),
    radius = fracture(
      x = x_base, 
      y = y_base, 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    x = radius * x_base,
    y = radius * y_base
  )
}



set.seed(1); perlin_blob() |> show_polygon(FALSE)
set.seed(2); perlin_blob() |> show_polygon(FALSE)
set.seed(3); perlin_blob() |> show_polygon(FALSE)


set.seed(1); perlin_blob(freq_init = .2) |> show_polygon(FALSE)
set.seed(1); perlin_blob(freq_init = .4) |> show_polygon(FALSE)
set.seed(1); perlin_blob(freq_init = .8) |> show_polygon(FALSE)


# The effect of the radius parameters is slightly different to the effect 
# of the noise parameter.


set.seed(1); 
perlin_blob(
  n = 1000,
  freq_init = 10, 
  r_min = .95, 
  r_max = 1
) |> 
  show_polygon(FALSE)


# Perlin hearts

# Modifying this system so that it draws distorted heart shapes rather than 
# distorted circles is not too difficult.

perlin_heart <- function(n = 100, 
                         freq_init = 0.3,
                         octaves = 2, 
                         r_min = 0.5, 
                         r_max = 1,
                         x_shift = 0,
                         y_shift = 0,
                         id = NA,
                         seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    x_base = cos(angle),
    y_base = sin(angle),
    radius = fracture(
      x = x_base, 
      y = y_base, 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    x = radius * heart_x(angle) + x_shift,
    y = radius * heart_y(angle) + y_shift,
    id = id
  )
}


perlin_heart(seed = 1) |> show_polygon(FALSE)
perlin_heart(seed = 2) |> show_polygon(FALSE)
perlin_heart(seed = 3) |> show_polygon(FALSE)


# To replicate that here I’ll need a palette generator and once again I’ll 
# fall back on our old favourite sample_canva()


sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}


perlin_heart_grid <- function(nx = 10, ny = 6, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  heart_settings <- expand_grid(
    r_min = .3, 
    r_max = .4, 
    x_shift = 1:nx, 
    y_shift = 1:ny
  ) |>
    mutate(id = row_number()) 
  
  heart_data <-  pmap_dfr(heart_settings, perlin_heart)
  
  heart_data |>
    ggplot(aes(x, y, group = id, fill = sample(id))) +
    geom_polygon(size = 0, show.legend = FALSE) +
    theme_void() +
    scale_fill_gradientn(colours = sample_canva(seed)) +
    coord_equal(xlim = c(0, nx + 1), ylim = c(0, ny + 1))
}

perlin_heart_grid(seed = 451)



perlin_heart2 <- function(n = 100, 
                          freq_init = 0.3,
                          octaves = 2, 
                          r_min = 0.5, 
                          r_max = 1,
                          w_min = 0,
                          w_max = 4,
                          rot = 0,
                          x_shift = 0,
                          y_shift = 0,
                          id = NA,
                          seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    
    radius = fracture(
      x = cos(angle), 
      y = sin(angle), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise_radius(r_min, r_max),
    
    x = radius * heart_x(angle) + x_shift,
    y = radius * heart_y(angle) + y_shift,
    
    width = fracture(
      x = cos(angle + rot), 
      y = sin(angle + rot), 
      freq_init = freq_init,
      noise = gen_perlin, 
      fractal = fbm, 
      octaves = octaves
    ) |>
      normalise(to = c(w_min, w_max)),
    
    id = id
  )
}



show_width <- function(polygon) {
  ggplot(polygon, aes(x, y, size = width)) +
    geom_path(colour = "white", fill = NA, show.legend = FALSE) + 
    coord_equal() + 
    scale_size_identity() +
    theme_void()
}

perlin_heart2(n = 1000, rot = 0, seed = 2) |> show_width()
perlin_heart2(n = 1000, rot = pi / 2, seed = 2) |> show_width()
perlin_heart2(n = 1000, rot = pi, seed = 2) |> show_width()



# Here’s an example where I plot several hearts at once courtesy 
# of the magic of pmap_dfr():



perlin_heart_grid2 <- function(nx = 4, ny = 2, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  heart_settings <- expand_grid(
    r_min = .3, 
    r_max = .4, 
    w_min = .01,
    w_max = 6,
    x_shift = 1:nx, 
    y_shift = 1:ny
  ) |>
    mutate(
      n = 200,
      x_shift = x_shift + runif(n(), -.1, .1),
      y_shift = y_shift + runif(n(), -.1, .1),
      rot = runif(n(), -.1, .1),
      id = row_number()
    ) 
  
  heart_data <-  pmap_dfr(heart_settings, perlin_heart2)
  
  heart_data |>
    ggplot(aes(x, y, group = id, colour = sample(id), size = width)) +
    geom_path(show.legend = FALSE) +
    theme_void() +
    scale_size_identity() +
    scale_colour_gradientn(colours = sample_canva(seed)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed(xlim = c(0, nx + 1), ylim = c(0, ny + 1))
}

perlin_heart_grid2(seed = 666)



# Animated perlin hearts


# The final example for this session uses the gifsky package to create an 
# animated version of the variable-width hearts from the last section, by 
# “rotating” or “sliding” the variable-with curves along the contours 
# of the Perlin hearts.



perlin_heart_data <- function(nhearts = 10, scatter = .05, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  palette <- sample_canva(seed) |>
    (\(x) colorRampPalette(x)(nhearts))()
  
  heart_settings <- tibble(
    id = 1:nhearts,
    n = 500,
    r_min = .35, 
    r_max = .4,
    w_min = -10, 
    w_max = 10,
    x_shift = runif(nhearts, -scatter/2, scatter/2),
    y_shift = runif(nhearts, -scatter/2, scatter/2),
    rot = runif(nhearts, -pi, pi)
  )
  
  heart_settings |>
    pmap_dfr(perlin_heart2) |>
    group_by(id) |>
    mutate(
      shade = sample(palette, 1),
      width = abs(width)
    )
}

generate_one_frame <- function(dat) {
  
  pic <- dat |>
    ggplot(aes(x, y, group = id, size = width, colour = shade)) +
    geom_path(show.legend = FALSE) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_identity() +
    scale_size_identity() +
    coord_fixed(xlim = c(-.6, .6), ylim = c(-.6, .6))
  
  print(pic)
}

rotate_vector <- function(x, percent) {
  
  len <- length(x)
  ind <- ceiling(len * percent)
  if(ind == 0) return(x)
  if(ind == len) return(x)
  c(x[(ind+1):len], x[1:ind])
}

generate_all_frames <- function(dat, nframes = 100) {
  
  for(frame in 1:nframes) {
    dat |>
      group_by(id) |>
      mutate(width = width |> rotate_vector(frame / nframes)) |>
      generate_one_frame()
  }
}

animated_perlin_heart <- function(seed, ...) {
  
  save_gif(
    expr = perlin_heart_data(seed = seed, ...) |> generate_all_frames(),
    gif_file = paste0("animated-perlin-heart-", seed, ".gif"),
    height = 1000,
    width = 1000,
    delay = .1,
    progress = TRUE,
    bg = "#222222"
  )
  invisible(NULL)
}

tic()
animated_perlin_heart(seed = 100)
toc()



animated_perlin_heart(seed = 123)
animated_perlin_heart(seed = 456)
animated_perlin_heart(seed = 789)

knitr::include_graphics("animated-perlin-heart-123.gif")
knitr::include_graphics("animated-perlin-heart-456.gif")
knitr::include_graphics("animated-perlin-heart-789.gif")

# Textured lines

library(e1071)

smooth_loess <- function(x, span) {
  n <- length(x)
  dat <- tibble(time = 1:n, walk = x)
  mod <- loess(walk ~ time, dat, span = span)
  predict(mod, tibble(time = 1:n))
}

smooth_path <- function(n = 1000, smoothing = .4, seed = NULL) { 
  if(!is.null(seed)) set.seed(seed)
  tibble(
    x = smooth_loess(rbridge(1, n), span = smoothing),
    y = smooth_loess(rbridge(1, n), span = smoothing),
    stroke = 1
  )
}

path <- smooth_path(seed = 123)

path |> 
  ggplot(aes(x, y)) +
  geom_path(colour = "white", size = 2) + 
  coord_equal() +
  theme_void() 


perturb <- function(path, noise = .01, span = .1) {
  path |> 
    group_by(stroke) |>
    mutate(
      x = x + rnorm(n(), 0, noise),
      y = y + rnorm(n(), 0, noise),
      x = smooth_loess(x, span),
      y = smooth_loess(y, span),
      alpha = runif(n()) > .5,
      size = runif(n(), 0, .2)
    )
}

brush <- function(path, bristles = 100, seed = 1, ...) {
  set.seed(seed)
  dat <- list()
  for(i in 1:bristles) {
    dat[[i]] <- perturb(path, ...)
  }
  return(bind_rows(dat, .id = "id"))
}

stroke <- function(dat, geom = geom_path, colour = "white", ...) {
  dat |>  
    ggplot(aes(
      x = x, 
      y = y, 
      alpha = alpha, 
      size = size, 
      group = paste0(stroke, id)
    )) + 
    geom(
      colour = colour, 
      show.legend = FALSE,
      ...
    ) + 
    coord_equal() +
    scale_alpha_identity() +
    scale_size_identity() +
    theme_void() + 
    theme(plot.background = element_rect(
      fill = "#222222", 
      colour = "#222222"
    ))
}


path |>
  brush() |>
  stroke()
path |>
  brush(bristles = 200, span = .08) |>
  mutate(size = size * 3) |>
  stroke(geom = geom_point, stroke = 0)



perlin_heart(n = 500, seed = 123) |>
  mutate(stroke = 1) |>
  brush(bristles = 100, noise = .02) |>
  stroke() 






