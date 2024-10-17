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

