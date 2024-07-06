# Flametree
# Cite the package

#install.packages("flametree")
library(flametree)

# pick some colours
shades <- c("red", "green", "white", "black")

# data structure defining the trees
dat <- flametree_grow(time = 10, trees = 10)

# draw the plot
dat %>% 
  flametree_plot(
    background = "beige",
    palette = shades, 
    style = "nativeflora"
  )


