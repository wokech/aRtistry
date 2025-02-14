# Kente Cloth Designs ChatGPT

# Plot 1

# Load necessary library
library(ggplot2)

# Define the data for the kente cloth pattern
kente_data <- data.frame(
  x = c(1, 2, 3, 4, 1.5, 2.5, 3.5, 1, 3, 2, 4, 1.5, 3.5, 2.5),
  y = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5),
  color = c(
    "#FFD700", "#228B22", "#FF4500", "#1E90FF",
    "#B22222", "#FFD700", "#4682B4", "#FFFFFF",
    "#32CD32", "#B22222", "#FFD700", "#1E90FF",
    "#FFD700", "#228B22"
  )
)

# Create the plot
kente_plot <- ggplot(kente_data, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "black", size = 0.5) + # Add black borders to tiles
  scale_fill_identity() + # Use provided hex colors
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#000000", color = NA) # Black background
  ) +
  coord_fixed(ratio = 1) + # Square tiles
  labs(title = "Kente Cloth-Inspired Design")

# Display the plot
print(kente_plot)

# Plot 2

# Load the necessary library
library(ggplot2)
library(dplyr)

# Define data for a more complex kente cloth pattern
kente_data <- expand.grid(x = 1:12, y = 1:8) %>%
  mutate(
    color = case_when(
      # Define patterns based on rows and columns
      (x %% 4 == 1 & y %% 2 == 1) ~ "#FFD700",  # Gold stripes
      (x %% 4 == 2 & y %% 2 == 0) ~ "#FF4500",  # Red accents
      (x %% 4 == 3) ~ "#228B22",               # Green sections
      (x %% 4 == 0 & y %% 2 == 1) ~ "#1E90FF", # Blue highlights
      (y %% 2 == 0 & x %% 3 == 0) ~ "#000000", # Black geometric blocks
      TRUE ~ "#FFFFFF"                         # White as a filler
    )
  )

# Create the plot
kente_plot <- ggplot(kente_data, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "black", size = 0.3) +  # Black borders for definition
  scale_fill_identity() +                  # Apply color directly
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#000000", color = NA) # Black background
  ) +
  coord_fixed(ratio = 1) +                  # Square tiles for realistic proportions
  labs(title = "Enhanced Kente Cloth-Inspired Design")

# Add geometric elements: stripes and overlays
kente_plot <- kente_plot +
  annotate(
    "rect", xmin = 4, xmax = 8, ymin = 3, ymax = 5, alpha = 0.3, fill = "#B22222"
  ) +  # A red overlay stripe
  annotate(
    "segment", x = 1, xend = 12, y = 7.5, yend = 7.5, color = "#FFD700", size = 2
  ) +  # Horizontal gold stripe
  annotate(
    "segment", x = 1, xend = 12, y = 4.5, yend = 4.5, color = "#228B22", size = 2
  ) +  # Horizontal green stripe
  annotate(
    "segment", x = 6, xend = 6, y = 1, yend = 8, color = "#FF4500", size = 1
  )     # Vertical red stripe

# Display the enhanced plot
print(kente_plot)

# Plot 3

# Load libraries
library(ggplot2)
library(dplyr)

# Define the data for a more intricate kente cloth design
kente_data <- expand.grid(x = 1:14, y = 1:10) %>%
  mutate(
    color = case_when(
      # Define primary and secondary patterns
      (x %% 4 == 1 & y %% 3 == 1) ~ "#FFD700",  # Gold diagonal highlights
      (x %% 4 == 2 & y %% 2 == 0) ~ "#FF4500",  # Red alternating blocks
      (x %% 4 == 3 & y %% 2 == 1) ~ "#228B22", # Green elements
      (x %% 4 == 0 & y %% 3 == 2) ~ "#1E90FF", # Blue accents
      (y %% 2 == 0 & x %% 3 == 0) ~ "#000000", # Black larger elements
      TRUE ~ "#FFFFFF"                         # White filler for spacing
    )
  )

# Base plot for the kente cloth
kente_plot <- ggplot(kente_data, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "black", size = 0.3) +  # Add borders for definition
  scale_fill_identity() +                  # Use predefined hex colors
  theme_void() +                           # Remove axis and gridlines
  theme(
    panel.background = element_rect(fill = "#000000", color = NA) # Black background
  ) +
  coord_fixed(ratio = 1) +                  # Square proportions for realism
  labs(title = "Advanced Kente Cloth-Inspired Design")

# Add geometric motifs: zigzag and checkerboard patterns
kente_plot <- kente_plot +
  annotate(
    "polygon",
    x = c(2, 3, 4, 3), y = c(8, 9, 8, 7),
    fill = "#FF4500", color = "black", alpha = 0.8
  ) + # Zigzag motif
  annotate(
    "rect",
    xmin = 6, xmax = 8, ymin = 3, ymax = 5,
    fill = "#1E90FF", alpha = 0.7, color = "black"
  ) + # Blue checkerboard overlay
  annotate(
    "segment",
    x = 1, xend = 14, y = 9.5, yend = 9.5,
    color = "#FFD700", size = 2
  ) + # Horizontal gold stripe
  annotate(
    "segment",
    x = 7, xend = 7, y = 1, yend = 10,
    color = "#228B22", size = 1
  ) + # Vertical green stripe
  annotate(
    "tile",
    x = c(10.5, 11.5), y = c(6.5, 6.5),
    fill = "#000000", color = "black"
  )    # Black small block motifs

# Display the enhanced plot
print(kente_plot)

