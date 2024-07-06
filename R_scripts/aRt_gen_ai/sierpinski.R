# GenAI Art

# Computational Art created with Gen AI

# Function to generate Sierpinski triangle
sierpinski <- function(n) {
  # Initialize matrix to store the triangle
  triangle <- matrix(0, nrow = n, ncol = 2 * n - 1)
  # Set the first point
  triangle[1, n] <- 1
  # Loop through rows
  for (i in 2:n) {
    # Set the midpoints
    triangle[i, (n - (i - 1)):(n + (i - 1))] <- 1
    # Set the tip of the triangle
    triangle[i, n] <- 1
  }
  # Plot the triangle
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rasterImage(triangle, 0, 0, 1, 1)
}

# Generate Sierpinski triangle with 6 levels
sierpinski(6)
