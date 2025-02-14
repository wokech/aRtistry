# Kente Cloth Designs Claude

# Plot 1 

library(ggplot2)
library(dplyr)

# Set random seed for reproducibility
set.seed(123)

# Create base data frame for the pattern
n_rows <- 20
n_cols <- 40

# Generate grid coordinates
grid_data <- expand.grid(
  x = 1:n_cols,
  y = 1:n_rows
)

# Define traditional Kente colors
kente_colors <- c(
  "#FFD700",  # Gold
  "#CC0000",  # Red
  "#000000",  # Black
  "#006400",  # Dark Green
  "#CD853F"   # Brown
)

# Create pattern data
pattern_data <- grid_data %>%
  mutate(
    # Create different pattern sections
    pattern_type = case_when(
      y %% 4 == 0 ~ "zigzag",
      y %% 4 == 1 ~ "stripe",
      y %% 4 == 2 ~ "diamond",
      TRUE ~ "block"
    ),
    # Assign colors based on position and pattern
    color = case_when(
      pattern_type == "zigzag" ~ kente_colors[1 + (x %% 5)],
      pattern_type == "stripe" ~ kente_colors[1 + ((x + y) %% 5)],
      pattern_type == "diamond" ~ kente_colors[1 + ((x * y) %% 5)],
      TRUE ~ kente_colors[1 + (x %% 5)]
    )
  )

# Create the visualization
ggplot(pattern_data, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "black", size = 0.1) +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    aspect.ratio = 0.5
  ) +
  coord_equal()

# Plot 2

library(ggplot2)
library(dplyr)
library(tidyr)

# Set random seed for reproducibility
set.seed(456)

# Enhanced color palette with more traditional variations
kente_colors <- c(
  "#FFD700",  # Gold
  "#CC0000",  # Red
  "#000000",  # Black
  "#006400",  # Dark Green
  "#CD853F",  # Brown
  "#8B4513",  # Saddle Brown
  "#DAA520",  # Goldenrod
  "#800000"   # Maroon
)

# Create larger grid for more detailed patterns
n_rows <- 40
n_cols <- 80

# Generate base grid
grid_data <- expand.grid(
  x = 1:n_cols,
  y = 1:n_rows
) %>%
  as_tibble()

# Function to create diamond pattern
create_diamond <- function(x, y, size) {
  abs(x %% size - size/2) + abs(y %% size - size/2) < size/2
}

# Function to create zigzag pattern
create_zigzag <- function(x, y, period) {
  (x + round(sin(y/period * 2 * pi) * period)) %% 2 == 0
}

# Enhanced pattern generation
pattern_data <- grid_data %>%
  mutate(
    # Create more complex pattern sections
    section = case_when(
      y %% 8 < 2 ~ "border",
      y %% 8 >= 2 & y %% 8 < 4 ~ "diamond",
      y %% 8 >= 4 & y %% 8 < 6 ~ "zigzag",
      TRUE ~ "weave"
    ),
    
    # Generate complex patterns
    pattern_value = case_when(
      section == "border" ~ as.numeric(x %% 4 < 2),
      section == "diamond" ~ as.numeric(create_diamond(x, y, 8)),
      section == "zigzag" ~ as.numeric(create_zigzag(x, y, 4)),
      TRUE ~ as.numeric((x + y) %% 4 < 2)
    ),
    
    # Create more varied color distribution
    color_group = (y %/% 8) %% length(kente_colors),
    base_color = kente_colors[1 + color_group],
    
    # Add alternating color patterns
    final_color = case_when(
      pattern_value == 1 ~ base_color,
      section == "border" ~ kente_colors[1],
      section == "diamond" ~ kente_colors[2],
      section == "zigzag" ~ kente_colors[3],
      TRUE ~ kente_colors[4]
    )
  )

# Create smaller geometric elements
geometric_elements <- data.frame(
  x = rep(seq(2, n_cols-1, 8), times = n_rows %/% 8),
  y = rep(seq(4, n_rows-1, 8), each = n_cols %/% 8)
) %>%
  mutate(
    size = 2,
    color = sample(kente_colors, n(), replace = TRUE)
  )

# Create the enhanced visualization
ggplot() +
  # Base pattern
  geom_tile(data = pattern_data, 
            aes(x = x, y = y, fill = final_color),
            color = "black", 
            size = 0.1) +
  # Add geometric elements
  geom_point(data = geometric_elements,
             aes(x = x, y = y, color = color),
             size = 2,
             shape = 18) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    aspect.ratio = 0.5,
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  coord_equal()

# Plot 3

library(ggplot2)
library(dplyr)
library(tidyr)

# Set random seed for reproducibility
set.seed(789)

# Refined color palette with better balance
kente_colors <- c(
  "#FFD700",  # Gold (primary)
  "#CC0000",  # Red (primary)
  "#000000",  # Black (primary)
  "#006400",  # Dark Green
  "#8B4513",  # Saddle Brown
  "#DAA520",  # Goldenrod
  "#800000",  # Maroon
  "#004225",  # Dark Forest Green
  "#FFA500"   # Orange
)

# Create larger grid for more intricate patterns
n_rows <- 60
n_cols <- 120

# Generate base grid
grid_data <- expand.grid(
  x = 1:n_cols,
  y = 1:n_rows
) %>%
  as_tibble()

# Function to create traditional adinkra symbol-inspired patterns
create_adinkra <- function(x, y, size) {
  center_dist <- sqrt((x %% size - size/2)^2 + (y %% size - size/2)^2)
  pattern1 <- center_dist < size/3
  pattern2 <- (x %% size + y %% size) < size
  pattern3 <- abs(x %% size - y %% size) < size/4
  return(pattern1 | pattern2 | pattern3)
}

# Enhanced pattern generation with more traditional motifs
pattern_data <- grid_data %>%
  mutate(
    # Create more diverse pattern sections
    section = case_when(
      y %% 12 < 2 ~ "border",
      y %% 12 >= 2 & y %% 12 < 5 ~ "adinkra",
      y %% 12 >= 5 & y %% 12 < 8 ~ "diamond",
      y %% 12 >= 8 & y %% 12 < 10 ~ "zigzag",
      TRUE ~ "weave"
    ),
    
    # Generate complex patterns with adjusted scales
    pattern_value = case_when(
      section == "border" ~ as.numeric(x %% 6 < 3),
      section == "adinkra" ~ as.numeric(create_adinkra(x, y, 12)),
      section == "diamond" ~ as.numeric(abs(x %% 12 - 6) + abs(y %% 12 - 6) < 8),
      section == "zigzag" ~ as.numeric((x + round(sin(y/3 * pi) * 6)) %% 3 == 0),
      TRUE ~ as.numeric((x + y) %% 6 < 3)
    ),
    
    # Improved color distribution
    color_group = (y %/% 12 + x %/% 20) %% length(kente_colors),
    base_color = kente_colors[1 + color_group],
    
    # Enhanced color balance
    final_color = case_when(
      pattern_value == 1 ~ base_color,
      section == "border" ~ kente_colors[1],  # More gold in borders
      section == "adinkra" ~ kente_colors[2], # More red in symbols
      section == "diamond" ~ kente_colors[3], # More black in diamonds
      TRUE ~ sample(kente_colors[4:9], 1)     # Mix of other colors
    )
  )

# Create more varied geometric elements with adjusted spacing
geometric_elements <- bind_rows(
  # Primary geometric elements
  data.frame(
    x = rep(seq(3, n_cols-2, 12), times = n_rows %/% 12),
    y = rep(seq(6, n_rows-2, 12), each = n_cols %/% 12),
    size = 3,
    shape = 18,
    color = kente_colors[1]
  ),
  # Secondary geometric elements
  data.frame(
    x = rep(seq(9, n_cols-2, 12), times = n_rows %/% 12),
    y = rep(seq(3, n_rows-2, 12), each = n_cols %/% 12),
    size = 2,
    shape = 15,
    color = kente_colors[2]
  ),
  # Tertiary elements
  data.frame(
    x = rep(seq(6, n_cols-2, 12), times = n_rows %/% 12),
    y = rep(seq(9, n_rows-2, 12), each = n_cols %/% 12),
    size = 2.5,
    shape = 19,
    color = kente_colors[3]
  )
)

# Create the final visualization
ggplot() +
  # Base pattern
  geom_tile(data = pattern_data, 
            aes(x = x, y = y, fill = final_color),
            color = "black", 
            size = 0.1) +
  # Layered geometric elements
  geom_point(data = geometric_elements,
             aes(x = x, y = y, 
                 color = color, 
                 size = size, 
                 #shape = factor(shape)
                 )) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_size_identity() +
  scale_shape_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    aspect.ratio = 0.5,
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  coord_equal()
