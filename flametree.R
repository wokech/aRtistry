# Generative art resources in R
# Danielle Navarro 
# @djnavarro

#install.packages("flametree")
library(flametree)

# pick some colours
shades <- c("#000000", "#000000", "white",  "#BE3A34", "#BE3A34", "white", "#009A44", "#009A44")

# data structure defining the trees
dat <- flametree_grow(time = 10, trees = 10)

# draw the plot
dat %>% 
  flametree_plot(
    background = "pink",
    palette = shades, 
    style = "nativeflora"
  )
