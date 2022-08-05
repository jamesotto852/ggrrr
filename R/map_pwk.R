#' Apply a plotting function, returning a patchwork
#'
#' The `*_pwk()` functions apply a function `f` element-wise across various objects,
#' converting the output into a patchwork object via `patchwork::wrap_plots()`.
#' `f` should return a `ggplot` object, or some other object which works with patchwork
#' (e.g. the results of `patchwork::wrap_elements()`).
#' For details on the use of each function, see `?purrr::map`.
#'
#' @inheritParams purrr::map
#' @param .patchwork_options A list of arguments passed to `patchwork::wrap_plots()`,
#' useful for arranging plots in the resulting patchwork object. See examples for details.
#'
#' @return A `patchwork` object
#'
#' @name map_pwk
#' @rdname map_pwk
#'
#' @import purrr
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' library("ggplot2")
#'
#' # Plot different linear models w/ geom_smooth() ----
#'
#' df <- data.frame(x = runif(100, -10, 10))
#' df$y <- with(df, 2 + x + .2 * x^2  - .05 * x^3+ rnorm(100, sd = 5))
#'
#' # plotting the data
#' ggplot(df, aes(x, y)) +
#'   geom_point()
#'
#' # f: formula -> ggplot object
#' f <- function(formula) {
#'   ggplot(df, aes(x, y)) +
#'     geom_smooth(formula = formula, method = "lm")
#' }
#'
#' # list of formulas to apply f over
#' formulas <- list(
#'   y ~ x,
#'   y ~ poly(x, 2),
#'   y ~ poly(x, 3)
#' )
#'
#' map_pwk(formulas, f)
#'
#' # can specify number of columns via .patchwork_options
#' map_pwk(formulas, f, .patchwork_options = list(ncol = 1))
#'
#'
#'# Map over data.frame rows with pmap_pwk() ----
#'
#' geoms <- list(
#'   geom_point(size = .5),
#'   geom_bin2d(),
#'   geom_density2d_filled()
#' )
#'
#' datasets <- list(
#'
#'   # Unimodal
#'   data.frame(
#'     x = rnorm(4e3),
#'     y = rnorm(4e3)
#'   ),
#'
#'   # Bimodal
#'   data.frame(
#'     x = rnorm(4e3) + rep(c(-2, 2), length.out = 1e3),
#'     y = rnorm(4e3)
#'   ),
#'
#'   # Constrained Support
#'   data.frame(
#'     x = rexp(4e3),
#'     y = rexp(4e3)
#'   )
#'
#' )
#'
#' # plot a geom + data combination
#' f <- function(geom, data) {
#'   ggplot(data, aes(x, y)) +
#'     geom
#' }
#'
#' # cross product between geoms and datasets,
#' # saved as a tibble
#' df_nested <-
#'   tidyr::expand_grid(
#'     geom = geoms,
#'     data = datasets
#'   )
#'
#' # use pmap_pwk to apply across rows of tibble or data.frame
#' # patchwork's `&` to manipulate the patchwork object
#' pmap_pwk(df_nested, f, .patchwork_options = list(ncol = 3)) &
#'   theme(
#'     legend.position = "none",
#'     axis.ticks = element_blank(),
#'     axis.title = element_blank(),
#'     axis.text = element_blank()
#'   )
#'
NULL


#' @rdname map_pwk
#' @export
map_pwk <- function(.x, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::map(.x, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

#' @rdname map_pwk
#' @export
imap_pwk <- function(.x, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::imap(.x, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

#' @rdname map_pwk
#' @export
map2_pwk <- function(.x, .y, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::map2(.x, .y, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}

#' @rdname map_pwk
#' @export
pmap_pwk <- function(.l, .f, ..., .patchwork_options = list()) {

  p_list <- purrr::pmap(.l, .f, ...)

  do.call(patchwork::wrap_plots, c(p_list, .patchwork_options))

}













