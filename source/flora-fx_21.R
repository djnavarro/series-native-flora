library(tidyverse)
library(ggforce)
library(here)
library(ggfx)
library(ggthemes)
library(future)
library(promises)


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
                           angle = c(-10, 10, 20),
                           split = 2,
                           prune = 0) {

  # parameters defining the tree
  param <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    split = split,  # number of new shoots from each old shoot at each time
    prune = prune   # probability of immediately pruning a new shoot
  )

  # set the seed for the random number generator
  if(!is.null(param$seed)) set.seed(param$seed)

  # growing the tree is a 3-step process
  tree <- flametree:::grow_sapling() %>%  # sapling is the first segment
    flametree:::grow_tree(param) %>%      # grow the tree with
    flametree:::shape_tree()

  # add the leaf indicator
  tree$id_leaf <- tree$id_time == max(tree$id_time)

  return(tree)
}

dottree_grow <- function(s) {

  tree <- flametree_grow(
    seed = NULL,
    time = 14,
    scale = c(.5, .7, .9, .8),
    angle = c(-15, 15, 10, -5),
    prune = .1
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

  tree %>%
    ggplot(aes(
      x = x,
      y = y,
      group = id_branch,
      colour = id_branch
    )) +

    as_reference(
      geom_point(
        data = tibble(
          x = rnorm(4, -.5, 1.2),
          y = rnorm(4, 3.8, 1.2)
        ),
        mapping = aes(x, y),
        colour = bg,
        fill = bg,
        shape = 18,
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
    coord_fixed(xlim = c(-2.2, 1.8), ylim = c(1.8, 5.8)) +
    scale_color_gradientn(colours = shades) +
    scale_fill_gradientn(colours = shades) +
    theme(plot.background = element_rect(fill = bg))

}

img_write <- function(pic, fpath) {
  ggsave(
    filename = fpath,
    plot = pic,
    width = 50/3,
    height = 50/3,
    dpi = 300
  )
}






# promise aware functions -------------------------------------------------

future_branch <- function(s) {
  future_promise(
    expr = dottree_grow(s),
    seed = s
  )
}

future_tree <- function(s) {

  promise_map(
    .x = sample(1000, 12),
    .f = future_branch
  ) %...>%
    bind_rows() %...>%
    mutate(id_branch = id_path + (id * max(id_path))) %...>%
    arrange(id) %...>%
    tell_user(s, "tree generated")

}

future_specify <- function(tree, s) {

  future_promise(tree, seed = TRUE) %...>%
    tree_plot() %...>%
    tell_user(s, "plot specified")

}

future_write <- function(pic, fpath, s) {

  future_promise(img_write(pic, fpath), seed = TRUE) %...>%
    tell_user(s, "image written")

}


# actually do things ------------------------------------------------------

flora_fx <- function(s) {

  #set.seed(s)
  fpath <- here("image", paste0("flora-fx_21_", s, ".png"))
  tell_user(NULL, s, "process started")

  future_tree(s) %...>%
    future_specify(s) %...>%
    future_write(fpath, s)

}



plan(
  strategy = multisession,
  workers = 6
)

flora_fx_async <- function(seeds) {
  for(s in seeds) flora_fx(s)
}
