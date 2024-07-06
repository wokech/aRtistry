# Load the required packages 

# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(ggthemes)

# Generate the lines

lines1 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(2), 19), 2))

lines2 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(4), 19), 4))

lines3 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(6), 19), 6))

lines4 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(8), 19), 8))


ggplot() + 
  geom_segment(data=lines4, color = "#00A1DE", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) +
  geom_segment(data=lines3, color = "#00A1DE", size = 3.6, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines2, color = "#FAD201", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines1, color = "#20603D", size = 4.0, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  ylim(-5, 15) +
  coord_polar() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(panel.background = element_rect(fill = "white", colour = "white")) 


# Save the plot
# ggsave("images/rwanda/rwanda_colors.png", width = 10, height = 10)