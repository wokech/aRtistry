# Load the required packages 

# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("tidyverse")
library(tidyverse)
library(ggthemes)

lines1 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(2.5), 19), 2.5))

# Generate the lines

lines2 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(3.5), 19), 3.5))

lines3 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(6), 19), 6))

lines4 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(7), 19), 7))

lines5 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(9.5), 19), 9.5))

ggplot() + 
  geom_segment(data=lines5, color = "#000000", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines4, color = "white", size = 4,lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) +
  geom_segment(data=lines3, color = "#BE3A34", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines2, color = "white", size = 4,lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines1, color = "#009A44", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  ylim(-5, 15)  +
  coord_polar() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(panel.background = element_rect(fill = "white", colour = "white")) 

# Save the plot
ggsave("images/kenya/kenya_colors.png", width = 10, height = 10)
