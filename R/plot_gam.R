#' Round to nearest multiple
#'
#' Internal function used inside `plot_gam()` function.
#' It takes a `i` number and rounds it to the near `j` multiple.
#'
#' @param i Numeric. Number to be rounded.
#' @param j Numeric. Multiple to round.
#'
#' @export

near <- function(i, j) {
  stopifnot("'i' must be a number" = is.numeric(i),
            "'j' must be a number" = is.numeric(j))

  i_hat = i / j
  output = round(i_hat) * j
  return(output)
}

#' Plot GAM model of bivariate model
#'
#' Display graphical representation of a gam model for bivariate
#' relationships. Model specification can be set with the ellipsis
#' argument (dot-dot-dot).
#'
#' @param data Dataset from where to extract x and y variables.
#' @param x Numeric. Variable for the x-axis.
#' @param y Numeric. Variable for the y-axis.
#' @param xlab Character. Label for x in the plot.
#' @param ylab Character. Label for y in the plot.
#' @param round_to Numeric. Intervals for mean and SE calculations.
#' @param ... Further arguments for `method.args` in `geom_smooth`.
#'
#' @export

plot_gam <- function(data, x, y, xlab, ylab, round_to = 1, ...) {

  if (is.character(substitute(x))) x <- as.name(x)
  if (is.character(substitute(y))) y <- as.name(y)
  if (missing(xlab)) xlab <- deparse(substitute(x))
  if (missing(ylab)) ylab <- deparse(substitute(y))

  scaled = copy(data)
  if (round_to > 1) {
    j_exec = substitute(near(x, round_to))
    x_var = deparse(substitute(x))
    scaled[, (x_var) := eval(j_exec)]
  }

  ggplot2::ggplot(data, ggplot2::aes({{x}}, {{y}})) +
    ggplot2::geom_count(alpha = 0.1) +
    ggplot2::stat_summary(data = scaled, fun.data = ggplot2::mean_se) +
    ggplot2::geom_smooth(method = "gam",
                         formula = y ~ s(x),
                         col = "red4",
                         method.args = list(...)) +
    ggplot2::labs(x = xlab, y = ylab, caption = paste0("Mean ",
                                                       rawToChar(x = as.raw(x = c(0xc2, 0xb1))),
                                                       " SE in ", round_to, "-unit intervals")) +
    see::theme_lucid()
}
