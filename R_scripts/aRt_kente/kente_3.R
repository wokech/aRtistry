# Kente Cloth Designs Perplexity

# Plot 1

# Load necessary libraries
library(ggplot2)

# Define the colors for the kente cloth
colors <- c("#C8102E", "#FFD700", "#4CAF50", "#2196F3", "#000000", "#FFFFFF", "#800080")

# Create a data frame for the pattern
kente_pattern <- expand.grid(x = 1:10, y = 1:10)
kente_pattern$color <- sample(colors, nrow(kente_pattern), replace = TRUE)

# Generate the kente cloth design
ggplot(kente_pattern, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "black") +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Kente Cloth Design")

# Plot 2

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the colors for the kente cloth
colors <- c("#C8102E", "#FFD700", "#4CAF50", "#2196F3", "#000000", "#FFFFFF", "#800080", "#FF5733", "#FFC300")

# Create a data frame for the pattern with more complexity
kente_pattern <- expand.grid(x = 1:20, y = 1:15)

# Create a more complex color distribution with geometric shapes
kente_pattern$color <- sample(colors, nrow(kente_pattern), replace = TRUE)

# Introduce geometric patterns by modifying color assignment based on position
kente_pattern$shape <- ifelse((kente_pattern$x + kente_pattern$y) %% 2 == 0, "striped", "plain")

# Generate the kente cloth design
ggplot(kente_pattern, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), color = "black") +
  geom_tile(data = kente_pattern %>% filter(shape == "striped"),
            aes(fill = color), alpha = 0.5) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Enhanced Kente Cloth Design") +
  coord_fixed(ratio = 1) # Maintain aspect ratio for squares

# Plot 2

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the colors for the kente cloth
colors <- c("#C8102E", "#FFD700", "#4CAF50", "#2196F3", "#000000", "#FFFFFF", "#800080", "#FF5733", "#FFC300")

# Create a data frame for the pattern with more complexity
kente_pattern <- expand.grid(x = 1:20, y = 1:15)

# Create a more complex color distribution with geometric shapes
kente_pattern$color <- sample(colors, nrow(kente_pattern), replace = TRUE)

# Introduce geometric patterns by modifying color assignment based on position
kente_pattern$shape <- ifelse((kente_pattern$x + kente_pattern$y) %% 2 == 0, "striped", "plain")

# Generate the kente cloth design
ggplot(kente_pattern, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), color = "black") +
  geom_tile(data = kente_pattern %>% filter(shape == "striped"),
            aes(fill = color), alpha = 0.5) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Enhanced Kente Cloth Design") +
  coord_fixed(ratio = 1) # Maintain aspect ratio for squares

# Plot 3

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the colors for the kente cloth
colors <- c("#C8102E", "#FFD700", "#4CAF50", "#2196F3", "#000000", "#FFFFFF", "#800080", "#FF5733", "#FFC300")

# Create a data frame for the pattern with more complexity
kente_pattern <- expand.grid(x = 1:20, y = 1:15)

# Create a more complex color distribution with geometric shapes
kente_pattern$color <- sample(colors, nrow(kente_pattern), replace = TRUE)

# Introduce geometric patterns and motifs
kente_pattern$shape <- ifelse((kente_pattern$x + kente_pattern$y) %% 3 == 0, "large_striped",
                              ifelse((kente_pattern$x + kente_pattern$y) %% 3 == 1, "small_dots", "plain"))

# Adjust the spacing and scale of certain patterns
kente_pattern$size <- ifelse(kente_pattern$shape == "large_striped", 1.5, 
                             ifelse(kente_pattern$shape == "small_dots", 0.5, 1))

# Generate the kente cloth design
ggplot(kente_pattern, aes(x = x, y = y)) +
  geom_tile(aes(fill = color), color = "black") +
  # Add large striped pattern
  geom_tile(data = kente_pattern %>% filter(shape == "large_striped"),
            aes(width = size, height = size), alpha = 0.5) +
  # Add small dot pattern
  geom_point(data = kente_pattern %>% filter(shape == "small_dots"),
             aes(size = size), shape = 21, fill = "white", color = "black") +
  scale_fill_identity() +
  scale_size_continuous(range = c(1, 5)) + # Adjust point sizes for dots
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Kente Cloth Design with Traditional Motifs") +
  coord_fixed(ratio = 1) # Maintain aspect ratio for squares
