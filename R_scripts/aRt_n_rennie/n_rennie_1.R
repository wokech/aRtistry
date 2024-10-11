# Getting started with generative art
# https://nrennie.rbind.io/blog/getting-started-generative-art/

# Implementing generative art systems 

# PLOT 1

# A) Parameters as variables 

n_lines <- 8
n_dots <- 10
max_size <- 3
main_col <- "black"
bg_col <- "white"
s <- 1234

# B) Simulating generative art data 

set.seed(s)
plot_data <- data.frame(
  x = stats::runif(n_lines * n_dots, 0, 10), # generate random values from a uniform distribution
  y = rep(1:n_lines, n_dots), # replicates the values 
  size = stats::rexp(n_lines * n_dots) #  generate random deviates from an exponential distribution
)

# C) Plotting with {ggplot2}

library(ggplot2)

g <- ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  geom_line(
    mapping = aes(group = y),
    alpha = 0.1, colour = main_col,
    linewidth = 0.3
  ) +
  geom_point(
    mapping = aes(size = size),
    pch = 21,
    fill = main_col, colour = main_col,
    alpha = 0.3
  )

g

g +
  scale_size(range = c(0.3, max_size)) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# PLOT 2

# A) Parameters as variables 

n_lines <- 30
n_dots <- 100
max_size <- 5
main_col <- "#326273"
bg_col <- "#BDD8DE"
s <- 2024

# B) Simulating generative art data 

set.seed(s)
plot_data <- data.frame(
  x = stats::runif(n_lines * n_dots, 0, 10), # generate random values from a uniform distribution
  y = rep(1:n_lines, n_dots), # replicates the values 
  size = stats::rexp(n_lines * n_dots) #  generate random deviates from an exponential distribution
)

# C) Plotting with {ggplot2}

library(ggplot2)

g <- ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  geom_line(
    mapping = aes(group = y),
    alpha = 0.1, colour = main_col,
    linewidth = 0.3
  ) +
  geom_point(
    mapping = aes(size = size),
    pch = 21,
    fill = main_col, colour = main_col,
    alpha = 0.3
  )

g

g +
  scale_size(range = c(0.5, max_size)) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

