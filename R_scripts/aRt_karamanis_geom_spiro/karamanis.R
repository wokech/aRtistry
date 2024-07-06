# Georgios Karamanis (rtistry)

library(ggplot2)
library(ggforce)

r=seq(1,11,0.1)

ggplot() +
  geom_spiro(aes(r=r, R=r*20, d=r^2, outer=T, color=r%%10), size=3) +
  scale_color_viridis_c(option="plasma") +
  coord_fixed() +
  theme_void() +
  theme(legend.position="none")
