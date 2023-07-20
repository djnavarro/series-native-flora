library(tidyverse)
library(ggforce)
library(here)
library(ggfx)
library(ggthemes)
library(flametree)


# promise unaware helpers -------------------------------------------------

blend_shades <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
  return(z)
}

flametree_grow <- function(seed = NULL,
                           time = 6,
                           scale = c(.8, .9),
                           angle = c(-10, 10, 20)) {

  # parameters defining the tree
  param <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle  # possible values for redirect at each time
  )

  # set the seed for the random number generator
  if(!is.null(param$seed)) set.seed(param$seed)

  # growing the tree
  tree <- flametree_grow(seed, time, scale, angle)

  return(tree)
}

dottree_grow <- function(s) {

  tree <- flametree_grow(
    seed = NULL,
    time = 14,
    scale = c(.5, .7, .9, .8),
    angle = c(-15, 15, 10, -5)
  ) %>% mutate(
    id = s,
    x = coord_x + runif(1, min = -.3, max = .3),
    y = coord_y + runif(1, min = -.02, max = .02)
  ) %>%
    filter(
      id_path %in% sample(max(id_path), 0.5 * max(id_path)),
      id_time > 2
    )

  return(tree)
}

tell_user <- function(x, s, p) {
  cat("[", as.character(lubridate::now()), "] seed ", s, " ", p, "\n", sep = "")
  return(x)
}

tree_plot <- function(tree) {

  shades <- sample(canva_palettes,1)[[1]]
  shades <- sample(shades)

  bg <- blend_shades(shades[1], "black", .25)

  leaf <- tree %>%
    filter(id_time == max(id_time), id_step == 2)

  pic <- tree %>%
    ggplot(aes(
      x = x,
      y = y,
      group = id_branch,
      colour = id_branch
    )) +

    as_reference(
      geom_point(
        data = tibble(
          x = rnorm(12, -.5, 1.2),
          y = rnorm(12, 3.8, 1.2)
        ),
        mapping = aes(x, y),
        colour = bg,
        fill = bg,
        shape = 1,
        stroke = 40,
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

    with_blend("tree",
               bg_layer = "behind",
               blend_type = "in",
               flip_order = FALSE
    ) +

    theme_void() +
    scale_size_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_gradientn(colours = shades) +
    scale_fill_gradientn(colours = shades) +
    theme(plot.background = element_rect(fill = bg))

  return(pic)
}

img_write <- function(pic, fpath) {

  current_fpath <- gsub("XXX", "whole", fpath)
  ggsave(
    filename = current_fpath,
    plot = pic,
    width = 50/3,
    height = 50/3,
    dpi = 300
  )

  xmin <- -2
  ymin <- 1.8

  xstep <- .5
  ystep <- .5 * sqrt(2)

  for(xoff in 0:7) {
    for(yoff in 0:3) {

      current_fpath <- gsub("XXX", paste0("x", xoff, "_y", yoff), fpath)
      cat(current_fpath, "\n")

      x0 <- xmin + xoff * xstep
      y0 <- ymin + yoff * ystep

      x1 <- x0 + xstep
      y1 <- y0 + ystep

      current_pic <- pic +
        coord_fixed(xlim = c(x0, x1), ylim = c(y0, y1))

      ggsave(
        current_fpath,
        current_pic,
        width = 40/3,
        height = sqrt(2) *40/3,
        dpi = 300
      )
    }
  }
}




# actually do things ------------------------------------------------------

flora_fx <- function(s) {

  fpath <- here("image", paste0("flora-fx_19_", s, "_XXX.png"))
  tell_user(NULL, s, "process started")

  sample(1000, 12) %>%
  map_dfr(dottree_grow) %>%
    tree_plot() %>%
    img_write(fpath)

}

