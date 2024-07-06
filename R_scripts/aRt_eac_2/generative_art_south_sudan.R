# Load the required packages 

# install.packages("ggthemes")
# install.packages("ggplot2")
# install.packages("tidyverse")
# library(tidyverse)
# library(ggplot2)
# library(ggthemes)

# Generate the lines

lines00 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(1), 19), 1))

lines0 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(2), 19), 2))

lines1 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(4.5), 19), 4.5))

lines2 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(5.5), 19), 5.5))

lines3 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(8), 19), 8))

lines4 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(9), 19), 9))

lines5 = tibble(
  x = seq(0, 38, by = 2),
  xend=x,
  y=rep(0, 20),
  yend=c(rep(c(11.5), 19), 11.5))

ggplot() + 
  geom_segment(data=lines5, color = "#000000", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines4, color = "#FFFFFF", size = 4,lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) +
  geom_segment(data=lines3, color = "#DA121A", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines2, color = "#FFFFFF", size = 4,lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines1, color = "#078930", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines0, color = "#0F47AF", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines00, color = "#FCDD09", size = 3.8, lineend = "round",
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
# ggsave("images/south_sudan/south_sudan_colors.png", width = 10, height = 10)