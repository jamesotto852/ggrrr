map_pwk <- function(.x, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::map(.x, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

imap_pwk <- function(.x, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::imap(.x, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

map2_pwk <- function(.x, .y, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::map2(.x, .y, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

pmap_pwk <- function(.l, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::pmap(.l, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}





# Examples ----------------------------------------------------------------
library("ggplot2")

p_fun <- function(scale_x = 1, scale_y = 1) {

  data.frame(
    x = 1:5 * scale_x,
    y = 1:5 * scale_y
  ) |>
    ggplot(aes(x, y)) +
    geom_point()

}

map_pwk(1:4, p_fun, .patchwork_options = list(ncol = 1))

map2_pwk(1:4, 11:14, p_fun, .patchwork_options = list(ncol = 1))

data.frame(
  scale_x = 1:4,
  scale_y = 11:14
) |>
  pmap_pwk(p_fun, .patchwork_options = list(ncol = 1))







# & Example ---------------------------------------------------------------

library("ggplot2"); theme_set(theme_bw())

p_fun <- function(scale_x = 1, scale_y = 1) {

  data.frame(
    x = 1:5 * scale_x,
    y = 1:5 * scale_y
  ) |>
    ggplot(aes(x, y)) +
    geom_point()

}

p <- map_pwk(1:4, p_fun, .patchwork_options = list(ncol = 1)) &
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

class(p)



# Cross-product of geoms and datasets -------------------------------------

geoms <- list(
  geom_point(),
  ggdensity::geom_hdr(),
  ggdensity::geom_hdr(method = "mvnorm")
)

datasets <- list(

  # Unimodal
  data.frame(
    x = rnorm(1e3),
    y = rnorm(1e3)
  ),

  # Bimodal
  data.frame(
    x = rnorm(1e3) + rep(c(-2, 2), length.out = 1e3),
    y = rnorm(1e3)
  ),

  # Constrained Support
  data.frame(
    x = rexp(1e3),
    y = rexp(1e3)
  )

)

p_fun <- function(geom, data) {
  ggplot(data, aes(x, y)) +
    geom
}


tidyr::expand_grid(
  geom = geoms,
  data = datasets
) |>
  pmap_pwk(p_fun, .patchwork_options = list(ncol = 3)) &
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )














