# Kente Cloth Designs Gemini

# Plot 1

library(ggplot2)
library(dplyr)

# Define Kente colors
kente_colors <- c("#FFD700", "#008000", "#FF0000", "#000000")

# Function to create a single strip data frame
create_strip_data <- function(strip_id, num_blocks = sample(3:4,1)) {
  data.frame(
    strip = strip_id,
    block = 1:num_blocks,
    color = sample(kente_colors, num_blocks, replace = TRUE)
  )
}

# Create data for multiple strips
num_strips <- 10
kente_data <- data.frame()
for (i in 1:num_strips) {
  kente_data <- rbind(kente_data, create_strip_data(i))
}

# Calculate block positions for plotting
kente_data <- kente_data %>%
  group_by(strip) %>%
  mutate(x_min = block - 1, x_max = block) %>%
  ungroup() %>%
  mutate(strip_offset = (strip - 1)) %>%
  mutate(x_min = x_min + strip_offset, x_max = x_max + strip_offset)

# Create the Kente cloth plot using ggplot2
kente_plot <- ggplot(kente_data) +
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = 1, fill = color), color = NA) + #color = NA removes borders
  scale_fill_manual(values = setNames(kente_colors, kente_colors)) + # Ensures correct color mapping
  theme_void() +  # Removes axes, gridlines, etc.
  coord_fixed(ratio = 0.1) + # Adjust aspect ratio to make strips look like cloth
  theme(legend.position = "none") #Remove legend

# Display the plot
print(kente_plot)

# Plot 2

library(ggplot2)
library(dplyr)

# Define Kente colors (expanded palette for more variety)
kente_colors <- c("#FFD700", "#008000", "#FF0000", "#000000", "#0000FF", "#800080", "#A0522D") #Added blue, purple, brown

# Function to create a single strip data frame with more complex patterns
create_strip_data <- function(strip_id) {
  num_blocks <- sample(4:7, 1) # More blocks for complexity
  block_widths <- sample(1:3, num_blocks, replace = TRUE) # Varying block widths
  block_widths <- block_widths / sum(block_widths) # Normalize to sum to 1
  
  data.frame(
    strip = strip_id,
    block = 1:num_blocks,
    color = sample(kente_colors, num_blocks, replace = TRUE),
    width = block_widths
  )
}

# Create data for multiple strips
num_strips <- 12
kente_data <- data.frame()
for (i in 1:num_strips) {
  kente_data <- rbind(kente_data, create_strip_data(i))
}

# Calculate block positions
kente_data <- kente_data %>%
  group_by(strip) %>%
  mutate(x_min = cumsum(c(0, width[-n()])), x_max = cumsum(width)) %>%
  ungroup() %>%
  mutate(strip_offset = (strip - 1)) %>%
  mutate(x_min = x_min + strip_offset, x_max = x_max + strip_offset)

# Create the Kente cloth plot
kente_plot <- ggplot(kente_data) +
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = 1, fill = color), color = "grey20", linewidth = 0.2) + # Added thin grey borders
  scale_fill_manual(values = setNames(kente_colors, kente_colors)) +
  theme_void() +
  coord_fixed(ratio = 0.1) +
  theme(legend.position = "none")

# Add geometric elements (small squares and lines)
for (i in 1:num_strips) {
  strip_data <- filter(kente_data, strip == i)
  for (j in 1:nrow(strip_data)) {
    x_center <- mean(c(strip_data$x_min[j], strip_data$x_max[j]))
    #Small squares
    kente_plot <- kente_plot + geom_rect(aes(xmin = x_center - 0.02, xmax = x_center + 0.02, ymin = 0.2, ymax = 0.8), fill="grey50", color=NA)
    #Horizontal lines
    kente_plot <- kente_plot + geom_segment(aes(x = strip_data$x_min[j], xend = strip_data$x_max[j], y = 0.5, yend = 0.5), color = "grey70", linewidth = 0.2)
  }
}

# Display the plot
print(kente_plot)

# Plot 3

library(ggplot2)
library(dplyr)

# Define Kente colors (adjusted balance - less bright red, more earth tones)
kente_colors <- c("#FFD700", "#008000", "#A0522D", "#000000", "#0000FF", "#800080", "#B8860B", "#8B4513") #Added dark goldenrod and saddlebrown

# Function to create a single strip data frame
create_strip_data <- function(strip_id) {
  num_blocks <- sample(4:7, 1)
  block_widths <- sample(1:3, num_blocks, replace = TRUE)
  block_widths <- block_widths / sum(block_widths)
  
  data.frame(
    strip = strip_id,
    block = 1:num_blocks,
    color = sample(kente_colors, num_blocks, replace = TRUE),
    width = block_widths
  )
}

# Create data for multiple strips
num_strips <- 12
kente_data <- data.frame()
for (i in 1:num_strips) {
  kente_data <- rbind(kente_data, create_strip_data(i))
}

# Calculate block positions
kente_data <- kente_data %>%
  group_by(strip) %>%
  mutate(x_min = cumsum(c(0, width[-n()])), x_max = cumsum(width)) %>%
  ungroup() %>%
  mutate(strip_offset = (strip - 1)) %>%
  mutate(x_min = x_min + strip_offset, x_max = x_max + strip_offset)

# Create the Kente cloth plot
kente_plot <- ggplot(kente_data) +
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = 1, fill = color), color = "grey20", linewidth = 0.2) +
  scale_fill_manual(values = setNames(kente_colors, kente_colors)) +
  theme_void() +
  coord_fixed(ratio = 0.1) +
  theme(legend.position = "none")

# Add geometric elements (adjusted spacing, scaled elements, new motifs)
for (i in 1:num_strips) {
  strip_data <- filter(kente_data, strip == i)
  for (j in 1:nrow(strip_data)) {
    x_center <- mean(c(strip_data$x_min[j], strip_data$x_max[j]))
    block_width <- strip_data$x_max[j] - strip_data$x_min[j]
    
    # Adjusted spacing and scaling of squares
    square_size <- block_width * 0.15 # Scale relative to block width
    kente_plot <- kente_plot + geom_rect(aes(xmin = x_center - square_size/2, xmax = x_center + square_size/2, ymin = 0.3, ymax = 0.7), fill="grey50", color=NA)
    
    # Adjusted spacing and scaling of lines
    line_y <- 0.5
    line_length <- block_width * 0.7
    kente_plot <- kente_plot + geom_segment(aes(x = x_center - line_length/2, xend = x_center + line_length/2, y = line_y, yend = line_y), color = "grey70", linewidth = 0.2)
    
    # New diamond motif (only on some blocks for variation)
    if (sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))) { # 30% chance of diamond
      diamond_size <- block_width * 0.2
      diamond_data <- data.frame(x = c(x_center, x_center + diamond_size/2, x_center, x_center - diamond_size/2),
                                 y = c(0.5 + diamond_size/2, 0.5, 0.5 - diamond_size/2, 0.5))
      kente_plot <- kente_plot + geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "grey80", color = NA)
    }
  }
}

# Display the plot
print(kente_plot)

