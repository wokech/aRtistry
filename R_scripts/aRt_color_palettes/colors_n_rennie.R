# Working with colours in R

# Website: https://nrennie.rbind.io/blog/colours-in-r/
# Website: https://r-graph-gallery.com/color-palette-finder
# Website: https://coolors.co/

# References
# 1) https://www.cararthompson.com/talks/nhsr2022-palatable-palettes/
# 2) https://blog.datawrapper.de/colors-for-data-vis-style-guides/


# We’ll start by creating a function, plot_palette(), that takes a vector 
# of colours and returns a plot showing them.

plot_palette <- function(palette) {
  g <- ggplot2::ggplot(
    data = data.frame(
      x = seq_len(length(palette)),
      y = "1",
      fill = palette
    ),
    mapping = ggplot2::aes(
      x = x, y = y, fill = fill
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  return(g)
}

# Different ways to define colours 

# Colour names 

plot_palette(c("tomato", "skyblue", "yellow2"))
plot_palette(c("azure3", "darkblue", "gold3"))

# Hex codes 

plot_palette(c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7"))

# RGB

colours <- rgb(
  red = c(32, 243, 176),
  blue = c(52, 62, 200),
  green = c(165, 10, 50),
  maxColorValue = 255
)
plot_palette(colours)

# Other definitions = HSV (Hue, Saturation, Value) / HCL (Hue, Chroma, Lightness)

col2rgb(c("red", "#DB778F"))

# Choosing colours 

# Colour palettes 

plot_palette(terrain.colors(n = 10))

library(paletteer)
plot_palette(paletteer_d("MetBrewer::Tara"))

# Accessible choices 

#
#remotes::install_github("wilkelab/cowplot")
#install.packages("colorspace", repos = "http://R-Forge.R-project.org")
#remotes::install_github("clauswilke/colorblindr")
#
# colorblindr: simulates how a plot you’ve created may look to 
# people with colour blindness

library(colorblindr)
library(ggplot2)
g <- ggplot() +
  geom_bar(
    data = mpg,
    mapping = aes(y = class, fill = drv)
  )
cvd_grid(g)

# The {plotcolr} package also shows how colours may look with different 
# types of colour vision deficiency.

# remotes::install_github("thedatacollective/plotcolr")

# NOT WORKING ...
plotcolr::plot_palette(
  palette = c("#F75C03", "#D90368", "#04A777", "#820263", "#F4E409"),
  cvd = "all"
)

# Generating colours 

library(monochromeR)
generate_palette(
  colour = "#E69F52",
  blend_colour = "#DB778F",
  n_colours = 4,
  view_palette = TRUE
)

# Blending colours 

all_colours <- colorRampPalette(c("tomato", "skyblue", "yellow2"))(100)
plot_palette(all_colours)

set.seed(123)
choose_colours <- sample(all_colours, 5)
plot_palette(choose_colours)

# Random hex codes 

random_hex <- function(n) {
  generate_hex <- function() {
    choices <- sample(c(as.character(0:9), LETTERS[1:6]), size = 6, replace = TRUE)
    output <- paste0("#", stringr::str_flatten(choices))
    return(output)
  }
  hex <- replicate(n = n, generate_hex(), simplify = TRUE)
  return(hex)
}


set.seed(123)
plot_palette(random_hex(6))


# Colours from images 

#devtools::install_github("doehm/eyedroppeR")

library(eyedroppeR)
extract_pal(
  n = 4,
  img_path = "https://nrennie.rbind.io/blog/creating-typewriter-images-r/image.jpg"
)
##########
# Jumping Rivers Blog
# Website: https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/
##########

# If you regularly make plots at work, it’s great to have 
# them be consistent with your company’s branding.

# Building a colour palette: 

# 1) Defining your colours

cvi_colours = list(
  cvi_purples = c("#381532", "#4b1b42", "#5d2252", "#702963",
                  "#833074", "#953784", "#a83e95"),
  my_favourite_colours = c("#702963", "#637029",    "#296370")
)

# 2) Generating a palette

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

cvi_palettes("my_favourite_colours", type = "discrete")

# 3) Creating {ggplot2} functions

library("ggplot2")
df = data.frame(x = c("A", "B", "C"),
                y = 1:3)
g = ggplot(data = df,
           mapping = aes(x = x, y = y)) +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.title = element_blank(), 
        axis.title = element_blank())

g + geom_col(aes(fill = x), colour = "black", size = 2) + ggtitle("Fill")
g + geom_col(aes(colour = x), fill = "white", size = 2) + ggtitle("Colour")


scale_colour_cvi_d = function(name) {
  ggplot2::scale_colour_manual(values = cvi_palettes(name,
                                                     type = "discrete"))
}


scale_fill_cvi_d = function(name) {
  ggplot2::scale_fill_manual(values = cvi_palettes(name,
                                                   type = "discrete"))
}


scale_colour_cvi_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = cvi_palettes(name = name,
                                                         type = "continuous"))
}


scale_fill_cvi_c = function(name) {
  ggplot2::scale_fill_gradientn(colours = cvi_palettes(name = name,
                                                       type = "continuous"))
}


scale_color_cvi_d = scale_colour_cvi_d
scale_color_cvi_c = scale_colour_cvi_c


# Testing our colour palettes

g +
  geom_point(aes(colour = y), size = 3) +
  scale_colour_cvi_c("cvi_purples")
g +
  geom_col(aes(fill = x), size = 3) +
  scale_fill_cvi_d("my_favourite_colours")


# Extending the functionality of your colour palettes

# Printing colour palettes (print.palette())
# Turn your colour palettes into an R package
# Discrete vs continuous palettes

cvi_palettes("my_favourite_colours", type = "continuous", n = 20)

# Order the colours
# Checking for colourblind palettes





