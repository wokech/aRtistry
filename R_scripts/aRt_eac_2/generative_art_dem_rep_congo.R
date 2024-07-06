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
  yend=c(rep(c(2.5), 19), 2.5))

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
  geom_segment(data=lines5, color = "#007FFF", size = 3, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines4, color = "#F7D518", size = 3.2, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) +
  geom_segment(data=lines3, color = "#CE1021", size = 3.4, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines2, color = "#F7D518", size = 3.6, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  geom_segment(data=lines1, color = "#007FFF", size = 3.8, lineend = "round",
               aes(x=x, xend=xend,
                   y=y, yend=yend)) + 
  ylim(-5, 10) +
  coord_polar() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(panel.background = element_rect(fill = "white", colour = "white")) 


# Save the plot
# ggsave("images/dem_rep_congo/dem_rep_congo_colors.png", width = 10, height = 10)
