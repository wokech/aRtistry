# Session 4: Wrap Up

# https://art-from-code.netlify.app/day-2/session-4/

# Lily Pad

# https://github.com/djnavarro/jasmines
library(jasmines)
library(dplyr)

dat <- use_seed(769) |>
  entity_heart(grain = 2000) |>
  mutate(ind = 1:n()) |>
  unfold_warp() |>
  unfold_worley(scatter = TRUE) |>
  unfold_breeze(
    iterations = 100,
    scale = .0005,
    drift = 0,
    fractal = ambient::billow,
    octaves = 1
  )  |>
  mutate(val = ind + id * 50)

# https://www.canva.com/colors/color-palettes/lily-leap/
shades <- c("#40c69f", "#2e765e","#d62b83","#05234c")

dat |>
  style_ribbon(
    palette = palette_manual(shades),
    colour = "order",
    alpha = c(.25,.02),
    background = "grey90",
    type = "segment",
    size = .4
  )