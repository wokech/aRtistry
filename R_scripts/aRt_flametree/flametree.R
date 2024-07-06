# Generative art resources in R
# Danielle Navarro 
# @djnavarro

#install.packages("flametree")
library(flametree)

# pick some colours
shades <- c("#000000", "#000000", "white", "#BE3A34", "#BE3A34", "white", "#009A44", "#009A44")

# data structure defining the trees
dat <- flametree_grow(time = 10, trees = 20)

# draw the plot
dat %>% 
  flametree_plot(
    background = "white",
    palette = shades, 
    style = "nativeflora"
  )

# Save the image

ggsave("images/flametree/flametree.png", width = 16, height = 12, dpi = 72)

flametree_grow(trees = 3, time = 10) %>% 
  flametree_plot(
    palette = c("#000000", "#000000", "white", "#BE3A34", "#BE3A34", "white", "#009A44", "#009A44")
  )

#ggsave("images/flametree/flametree_2.png", width = 16, height = 12, dpi = 72)
