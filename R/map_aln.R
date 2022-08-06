#' Apply a plotting function, returning a list of aligned plots
#'
#' The `*_aln()` functions apply a function `f` element-wise across various objects,
#' converting the output into a list of plots with equal spacing and alignment via `patchwork::align_patches()`.
#' `f` should return a `ggplot` object, no other output is allowed.
#'
#' @inheritParams map_pwk
#'
#' @return A `patchwork` object
#'
#' @name map_aln
#' @rdname map_aln
#'
#' @import purrr
#' @importFrom patchwork align_patches
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
#' plots <- map_aln(formulas, f)
#'
#' plots[[1]]
#' plots[[2]]
#' plots[[3]]
#'
#
NULL


#' @rdname map_aln
#' @export
map_aln <- function(.x, .f, ...) {

  p_list <- purrr::map(.x, .f, ...)

  do.call(patchwork::align_plots, p_list)

}

#' @rdname map_aln
#' @export
imap_aln <- function(.x, .f, ...) {

  p_list <- purrr::imap(.x, .f, ...)

  do.call(patchwork::align_plots, p_list)

}

#' @rdname map_aln
#' @export
map2_aln <- function(.x, .y, .f, ...) {

  p_list <- purrr::map2(.x, .y, .f, ...)

  do.call(patchwork::align_plots, p_list)

}

#' @rdname map_aln
#' @export
pmap_aln <- function(.l, .f, ...) {

  p_list <- purrr::pmap(.l, .f, ...)

  do.call(patchwork::align_plots, p_list)

}


