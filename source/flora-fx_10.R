library(tidyverse)
library(flametree)
library(ggforce)
library(here)
library(ggfx)

#shades <- c("#E70000", "#FF8C00", "#FFEF00",
#            "#00811F", "#0044FF", "#760089")



blend_shades <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
  return(z)
}

seeds <- 303
for(s in seeds) {

  gc()

  cat("seed", s, "\n")
  set.seed(s)
  fpath <- here("image", paste0("flora-fx_10_", s, ".png"))

  #shades <- sample(colours(distinct = TRUE), 6)

  canva <- ggthemes::canva_palettes
  shades <- sample(canva,1)[[1]]


  dat <- sample(1000, 12) %>%
    map_dfr(function(s){
      flametree_grow(
        seed = s,
        time = 14,
        scale = c(.5, .7, .9, .8),
        angle = c(-15, 15, 10, -5),
        prune = .1
      ) %>%
        mutate(
          id = s,
          x = coord_x + runif(1, min = -.3, max = .3),
          y = coord_y + runif(1, min = -.02, max = .02)
        ) %>%
        filter(
          id_path %in% sample(max(id_path), 0.5 * max(id_path)),
          id_time > 2
        )
    }) %>%
    mutate(
      id_branch = id_path + (id * max(id_path))
    ) %>%
    arrange(id)

  leaf <- dat %>%
    filter(id_time == max(id_time), id_step == 2)

  np <- 6
  bg <- blend_shades(shades[1], "black", .25)
  trans_density <- function(x) {x}

  pic <- dat %>%
    ggplot(aes(
      x = x,
      y = y,
      group = id_branch,
      colour = id_branch
    )) +

    as_reference(
      geom_point(
        data = tibble(
          x = rnorm(np, -.5, 1.2),
          y = rnorm(np, 3.8, 1.2)
        ),
        mapping = aes(x, y),
        colour = bg,
        fill = bg,
        shape = 0,
        stroke = 60,
        #alpha = 0,
        size = 200,
        show.legend = FALSE,
        inherit.aes = FALSE
      ),
      id = "behind"
    ) +

    as_group(
      geom_bezier(
        alpha = 1,
        size = 0.3,
        show.legend = FALSE,
        lineend = "round"
      ),
      geom_point(
        data = leaf,
        show.legend = FALSE,
        size = 1.8,
        stroke = 0
      ),
      id = "tree"
    ) +

    as_reference(
      with_blend("tree",
        bg_layer = "behind",
        blend_type = "in",
        flip_order = FALSE
      ),
      id = "portals"
    ) +

    with_shadow("portals") +

    theme_void() +
    scale_size_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed(xlim = c(-2.2, 1.8), ylim = c(1.8, 5.8)) +
    scale_color_gradientn(colours = shades) +
    scale_fill_gradientn(colours = shades) +
    theme(plot.background = element_rect(
      fill = bg)
    )


  ggsave(fpath, pic, width = 50/3, height = 50/3, dpi = 300)
}
