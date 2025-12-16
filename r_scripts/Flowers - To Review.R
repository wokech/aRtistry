library(tidyverse)
library(ggfx)

# http://lenkiefer.com/2021/06/16/rose-chart-in-r/


## original chart ----
seq(-3, 3, by = 0.01) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (1 - x - sin(y^2)), y = (1 + y - cos(x^2)))) +
  geom_point(alpha = 0.05, shape = 20, size = 0) +
  theme_void() +
  coord_polar()


## first chart ----
seq(-3, 3, by = 0.02) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (1 - x - sin(y^2)), y = (1 + y - cos(x^2)))) +
  ggfx::with_outer_glow(
    geom_point(
      alpha = 0.05,
      shape = 20,
      size = 0,
      color = "white"
    ),
    colour = "red",
    expand = 10,
    sigma = 3
  ) +
  theme_void() +
  coord_polar() +
  labs(
    caption = paste0(
      "@lenkiefer modified from @aschinchon #rose tweet,",
      "\nhttps://twitter.com/aschinchon/status/1405136386034970630"
    )
  )


## third chart ----
seq(-3, 3, by = .01) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (1 - x - sin(y^2)), y = (1 + y - cos(x^2)))) +
  ggfx::with_inner_glow(
    colour = "dodgerblue",
    expand = 5,
    sigma = 2,
    ggfx::with_outer_glow(
      geom_point(
        alpha = .05,
        shape = 20,
        size = 0,
        color = "deeppink"
      ),
      colour = "purple",
      expand = 10,
      sigma = 2
    )
  ) +
  theme_void() +
  coord_polar() +
  labs(
    caption = paste0(
      "@lenkiefer modified from @aschinchon #rose tweet,",
      "\nhttps://twitter.com/aschinchon/status/1405136386034970630"
    ),
    title = ""
  )