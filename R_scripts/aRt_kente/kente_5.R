# Kente Cloth Designs Google AI Studio

# Plot 1

# Install and load necessary packages
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Define Kente colors (as hex codes)
kente_colors <- c(
  yellow = "#FFFF00",
  gold = "#FFD700",
  green = "#008000",
  blue = "#0000FF",
  red = "#FF0000",
  black = "#000000"
)


# Function to create a kente strip
create_kente_strip <- function(colors, width = 1, start_y = 0) {
  n_colors <- length(colors)
  data <- data.frame(
    x_start = seq(0, width, length.out = n_colors + 1)[1:n_colors],
    x_end = seq(0, width, length.out = n_colors + 1)[2:(n_colors + 1)],
    y_start = rep(start_y, n_colors),
    y_end = rep(start_y + 1, n_colors),
    fill_color = unname(colors)
  )
  return(data)
}

# Create multiple strips with different color patterns
strip1 <- create_kente_strip(c(kente_colors["yellow"], kente_colors["red"], kente_colors["yellow"], kente_colors["black"]), start_y = 0)
strip2 <- create_kente_strip(c(kente_colors["gold"], kente_colors["green"],kente_colors["gold"]), start_y = 1)
strip3 <- create_kente_strip(c(kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]), start_y = 2)
strip4 <- create_kente_strip(c(kente_colors["yellow"],kente_colors["gold"], kente_colors["green"], kente_colors["gold"],kente_colors["yellow"] ),start_y = 3)
strip5 <- create_kente_strip(c(kente_colors["black"],kente_colors["blue"], kente_colors["black"]), start_y = 4)
strip6 <- create_kente_strip(c(kente_colors["red"], kente_colors["green"], kente_colors["red"], kente_colors["black"], kente_colors["red"]), start_y = 5)
strip7 <- create_kente_strip(c(kente_colors["green"], kente_colors["gold"], kente_colors["green"]),start_y = 6)
strip8 <- create_kente_strip(c(kente_colors["black"], kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]),start_y = 7)

# Combine strips into one data frame
all_strips <- rbind(strip1, strip2, strip3, strip4, strip5, strip6, strip7, strip8)

# Create the plot
kente_plot <- ggplot(all_strips) +
  geom_rect(aes(xmin = x_start, xmax = x_end, ymin = y_start, ymax = y_end, fill = fill_color), color = NA) +
  scale_fill_identity() +  # Use color values directly
  theme_void() +
  coord_equal()+  # Ensure square aspect ratio
  theme(plot.background = element_rect(fill = "white"))


# Print the plot
print(kente_plot)

# Plot 2

# Install and load necessary packages
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Define Kente colors (as hex codes)
kente_colors <- c(
  yellow = "#FFFF00",
  gold = "#FFD700",
  green = "#008000",
  blue = "#0000FF",
  red = "#FF0000",
  black = "#000000"
)


# Function to create a kente strip with more complex patterns
create_kente_strip_complex <- function(colors, width = 1, start_y = 0, pattern = "stripe") {
  n_colors <- length(colors)
  if(pattern == "stripe") {
    data <- data.frame(
      x_start = seq(0, width, length.out = n_colors + 1)[1:n_colors],
      x_end = seq(0, width, length.out = n_colors + 1)[2:(n_colors + 1)],
      y_start = rep(start_y, n_colors),
      y_end = rep(start_y + 1, n_colors),
      fill_color = unname(colors)
    )
  } else if (pattern == "checkered") {
    repeats <- 3
    n_colors_per_repeat <- n_colors
    data <- data.frame()
    for(i in 1:repeats) {
      subdata <- data.frame(
        x_start = seq(0, width, length.out = n_colors_per_repeat + 1)[1:n_colors_per_repeat] + (i-1)*(width/repeats),
        x_end = seq(0, width, length.out = n_colors_per_repeat + 1)[2:(n_colors_per_repeat + 1)] + (i-1)*(width/repeats),
        y_start = rep(start_y, n_colors_per_repeat),
        y_end = rep(start_y + 1/repeats, n_colors_per_repeat),
        fill_color = unname(colors)
      )
      data <- rbind(data, subdata)
      colors <- rev(colors)
      
    }
    
    
  } else {
    stop("Invalid pattern type.")
  }
  
  return(data)
}


# Create multiple strips with different color patterns
strip1 <- create_kente_strip_complex(c(kente_colors["yellow"], kente_colors["red"], kente_colors["yellow"], kente_colors["black"]), start_y = 0)
strip2 <- create_kente_strip_complex(c(kente_colors["gold"], kente_colors["green"],kente_colors["gold"]), start_y = 1)
strip3 <- create_kente_strip_complex(c(kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]), start_y = 2)
strip4 <- create_kente_strip_complex(c(kente_colors["yellow"],kente_colors["gold"], kente_colors["green"], kente_colors["gold"],kente_colors["yellow"] ),start_y = 3)
strip5 <- create_kente_strip_complex(c(kente_colors["black"],kente_colors["blue"], kente_colors["black"]), start_y = 4)
strip6 <- create_kente_strip_complex(c(kente_colors["red"], kente_colors["green"], kente_colors["red"], kente_colors["black"], kente_colors["red"]), start_y = 5, pattern = "checkered")
strip7 <- create_kente_strip_complex(c(kente_colors["green"], kente_colors["gold"], kente_colors["green"]),start_y = 5+1/3)
strip8 <- create_kente_strip_complex(c(kente_colors["black"], kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]),start_y = 6+1/3)


# Combine strips into one data frame
all_strips <- rbind(strip1, strip2, strip3, strip4, strip5, strip6, strip7, strip8)

# Create the plot
kente_plot <- ggplot(all_strips) +
  geom_rect(aes(xmin = x_start, xmax = x_end, ymin = y_start, ymax = y_end, fill = fill_color), color = NA) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 7.5, ymax = 8.5), fill = kente_colors["black"], color = NA)+ # black border
  geom_rect(aes(xmin = 0, xmax = 1, ymin = -0.5, ymax = 0.5), fill = kente_colors["black"], color = NA)+ # black border
  geom_rect(aes(xmin = -0.1, xmax = 0, ymin = -0.5, ymax = 8.5), fill = kente_colors["gold"], color = NA)+ # gold border
  geom_rect(aes(xmin = 1, xmax = 1.1, ymin = -0.5, ymax = 8.5), fill = kente_colors["gold"], color = NA)+ # gold border
  scale_fill_identity() +  # Use color values directly
  theme_void() +
  coord_equal() +  # Ensure square aspect ratio
  theme(plot.background = element_rect(fill = "white"))

# Print the plot
print(kente_plot)

# Plot 3

# Install and load necessary packages
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Define Kente colors (as hex codes)
kente_colors <- c(
  yellow = "#FFFF00",
  gold = "#FFD700",
  green = "#008000",
  blue = "#0000FF",
  red = "#FF0000",
  black = "#000000"
)


# Function to create a kente strip with more complex patterns
create_kente_strip_complex <- function(colors, width = 1, start_y = 0, pattern = "stripe", spacing_factor = 1) {
  n_colors <- length(colors)
  if(pattern == "stripe") {
    data <- data.frame(
      x_start = seq(0, width, length.out = n_colors + 1)[1:n_colors],
      x_end = seq(0, width, length.out = n_colors + 1)[2:(n_colors + 1)],
      y_start = rep(start_y, n_colors),
      y_end = rep(start_y + 1, n_colors),
      fill_color = unname(colors)
    )
  } else if (pattern == "checkered") {
    repeats <- 3
    n_colors_per_repeat <- n_colors
    data <- data.frame()
    for(i in 1:repeats) {
      subdata <- data.frame(
        x_start = seq(0, width, length.out = n_colors_per_repeat + 1)[1:n_colors_per_repeat] + (i-1)*(width/repeats),
        x_end = seq(0, width, length.out = n_colors_per_repeat + 1)[2:(n_colors_per_repeat + 1)] + (i-1)*(width/repeats),
        y_start = rep(start_y, n_colors_per_repeat),
        y_end = rep(start_y + 1/repeats, n_colors_per_repeat),
        fill_color = unname(colors)
      )
      data <- rbind(data, subdata)
      colors <- rev(colors)
      
    }
    
    
  } else if(pattern == "spaced stripe") {
    spacing_width <- width/(n_colors*2) * spacing_factor
    data <- data.frame(
      x_start = seq(0, width, by = spacing_width)[1:n_colors] + (seq(0,width, by = spacing_width)[1:n_colors] - spacing_width),
      x_end = seq(0, width, by = spacing_width)[1:n_colors]+(seq(0,width, by = spacing_width)[1:n_colors] - spacing_width) + spacing_width,
      y_start = rep(start_y, n_colors),
      y_end = rep(start_y + 1, n_colors),
      fill_color = unname(colors)
    )
  }
  
  else {
    stop("Invalid pattern type.")
  }
  
  return(data)
}

# Function to create diamond motifs
create_diamond <- function(x_center, y_center, size, fill_color) {
  data.frame(
    x = c(x_center, x_center + size/2, x_center, x_center - size/2),
    y = c(y_center - size/2, y_center, y_center + size/2, y_center),
    fill = fill_color
  )
}


# Create multiple strips with different color patterns
strip1 <- create_kente_strip_complex(c(kente_colors["yellow"], kente_colors["red"], kente_colors["yellow"], kente_colors["black"]), start_y = 0)
strip2 <- create_kente_strip_complex(c(kente_colors["gold"], kente_colors["green"],kente_colors["gold"]), start_y = 1)
strip3 <- create_kente_strip_complex(c(kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]), start_y = 2)
strip4 <- create_kente_strip_complex(c(kente_colors["yellow"],kente_colors["gold"], kente_colors["green"], kente_colors["gold"],kente_colors["yellow"] ),start_y = 3, spacing_factor = 0.5, pattern = "spaced stripe")
strip5 <- create_kente_strip_complex(c(kente_colors["black"],kente_colors["blue"], kente_colors["black"]), start_y = 4)
strip6 <- create_kente_strip_complex(c(kente_colors["red"], kente_colors["green"], kente_colors["red"], kente_colors["black"], kente_colors["red"]), start_y = 5, pattern = "checkered")
strip7 <- create_kente_strip_complex(c(kente_colors["green"], kente_colors["gold"], kente_colors["green"]),start_y = 5+1/3)
strip8 <- create_kente_strip_complex(c(kente_colors["black"], kente_colors["blue"], kente_colors["red"], kente_colors["black"], kente_colors["blue"]),start_y = 6+1/3)

# Create diamond motifs
diamond1 <- create_diamond(x_center = 0.5, y_center = 7.5/2, size = 0.3, fill_color = kente_colors["gold"])
diamond2 <- create_diamond(x_center = 0.5, y_center = 1.5/2, size = 0.2, fill_color = kente_colors["yellow"])
diamond3 <- create_diamond(x_center = 0.5, y_center = 5.5/2, size = 0.25, fill_color = kente_colors["red"])


# Combine strips into one data frame
all_strips <- rbind(strip1, strip2, strip3, strip4, strip5, strip6, strip7, strip8)

# Combine all motif data into one data frame
all_motifs <- rbind(diamond1, diamond2, diamond3)


# Create the plot
kente_plot <- ggplot(all_strips) +
  geom_rect(aes(xmin = x_start, xmax = x_end, ymin = y_start, ymax = y_end, fill = fill_color), color = NA) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 7.5, ymax = 8.5), fill = kente_colors["black"], color = NA)+ # black border
  geom_rect(aes(xmin = 0, xmax = 1, ymin = -0.5, ymax = 0.5), fill = kente_colors["black"], color = NA)+ # black border
  geom_rect(aes(xmin = -0.1, xmax = 0, ymin = -0.5, ymax = 8.5), fill = kente_colors["gold"], color = NA)+ # gold border
  geom_rect(aes(xmin = 1, xmax = 1.1, ymin = -0.5, ymax = 8.5), fill = kente_colors["gold"], color = NA)+ # gold border
  scale_fill_identity() +  # Use color values directly
  geom_polygon(data = all_motifs, aes(x = x, y = y, fill = fill), color = NA) +
  theme_void() +
  coord_equal() + # Ensure square aspect ratio
  theme(plot.background = element_rect(fill = "white"))



# Print the plot
print(kente_plot)
