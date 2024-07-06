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
  yend=c(rep(c(3), 19), 3))

lines2 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(3.5), 19), 3.5))

lines3 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(4.5), 19), 4.5))

lines4 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(5), 19), 5))

lines5 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(7), 19), 7))

lines6 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(7.5), 19), 7.5))

lines7 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(8.5), 19), 8.5))

lines8 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(9), 19), 9))

lines9 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(12), 19), 12))

ggplot() + 
  geom_segment(data=lines9, color = "#418FDE", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines8, color = "#FFFFFF", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines7, color = "#000000", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines6, color = "#006600", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines5, color = "#FBD016", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines4, color = "#006600", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) +
  geom_segment(data=lines3, color = "#D90000", size = 3.6, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines2, color = "#FFFFFF", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines1, color = "#418FDE", size = 4.0, lineend = "round",
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
# ggsave("images/eac/eac_colors.png", width = 10, height = 10)