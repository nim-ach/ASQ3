#' Format p-values
#'
#' Internal function to round p-values
#'
#' @param x Numeric. p-value to be formatted.

format_p <- function(x) {
  stopifnot(
    "x is not numeric" = is.numeric(x)
  )

  x <- data.table::fifelse(
    test = x < 0.001,
    yes = "< 0.001",
    no = paste("=", round(x, 3))
  )

  return(x)
}

#' Report chi-square test
#'
#' This function lets you pass a chi-square function and report it.
#'
#' @param chi A chi-square object.
#' @param effsize Logical. Whether to report effect sizes or not.
#' @param ci Numeric. Confidence interval to effect size calculation.
#'
#' @name report_chi
#' @export

report_chi <- function(chi, effsize = TRUE, ci = 0.95) {

  stopifnot(
    "Is not an object of class \"htest\"" = inherits(chi, "htest")
  )

  if (isTRUE(effsize)) {
    efs <- effectsize::pearsons_c(chi, ci = ci)
    efs <- lapply(efs, round, 2)
  }

  chi$statistic <- round(chi$statistic, 2)

  chi$p.value <- format_p(chi$p.value)

  chi$method <- data.table::fifelse(
    test = grepl("pearson", x = tolower(chi$method)),
    yes = "Pearson",
    no = "gof"
  )

  expr <- paste(
    paste0("$\\chi^2_{",chi$method,"}$ (",chi$parameter,") = ",chi$statistic),
    paste0("*p* ",chi$p.value),
    sep = ", "
  )

  if (isTRUE(effsize)) {
    expr <- paste(
      expr,
      paste0("$C_{Pearson}$ = ", efs[[1]]),
      paste0("CI~", efs$CI,"%~[", efs$CI_low,", ",efs$CI_high,"]"),
      sep = ", "
    )
  }

  return(expr)
}

#' Report GAM model overall slope
#'
#' Report the overall slopes for a GAM model fitted through the `mgcv` package.
#'
#' @param model Object of class "gam".
#' @param ci Numeric. Confidence interval for coefficients. Defaults to 0.95.
#' @param ... Further parameters to the `estimate_sloples()` function from `modelbased` package.
#'
#' @export

report_overall <- function(model, ..., ci = 0.95) {

  stopifnot(
    "Is not a GAM model" = inherits(model, "gam")
  )

  slopes <- modelbased::estimate_slopes(model, ..., ci = ci)

  slopes$df_error <- round(slopes$df_error, 2)
  slopes$t <- round(slopes$t, 2)

  expr <- paste(
    paste0("$\\beta$ = ", round(slopes$Coefficient, 2)),
    paste0("CI~",slopes$CI*100,"%~[", round(slopes$CI_low, 2), ", ", round(slopes$CI_high, 2), "]"),
    paste0("$t_{student}$ (", slopes$df_error, ") = ", slopes$t),
    paste0("*p* ", format_p(slopes$p)),
    sep = ", "
  )

  if (nrow(slopes) > 1) {
    col_ind <- colnames(slopes[seq.int(to = ncol(slopes) - 8)])
    expr <- data.table::data.table(
      slopes[col_ind],
      expr,
      key = col_ind
    )
  }

  return(expr)
}

#' Report linear slopes from a GAM model
#'
#' Report the linear combinations (i.e., slopes) for a GAM model fitted through effect derivatives.
#'
#' @param model Object of class "gam".
#' @param term Character. Term present in the model for which estimate an effect
#' @param ci Numeric. Confidence interval for coefficients. Defaults to 0.95.
#' @param length Numeric. This arguments controls the number of (equally spread) values that will be taken to represent the continuous variable.
#' @param k Numeric. Number of decimal places when printing numeric variables (Defaults to 1). Except for expression column.
#' @param ... Further parameters passed to the `estimate_sloples()` from `modelbased` package.
#'
#' @export

report_slopes <- function(model, term, ci = 0.95, length = 100, k = 1, ...) {

  stopifnot(
    "Is not a GAM model" = inherits(model, "gam"),
    "Must specify a term for which estimate an effect" = !missing(term)
  )

  slopes <- modelbased::estimate_slopes(
    model = model,
    trend = term,
    at = term,
    ci = ci,
    length = length,
    ...
  )

  slopes <- summary(slopes)

  slopes$df_error <- round(slopes$df_error, 2)
  slopes$t <- round(slopes$t, 2)

  expr <- paste(
    paste0("$\\beta$ = ", round(slopes$Coefficient, 2)),
    paste0("CI~",slopes$CI*100,"%~[", round(slopes$CI_low, 2), ", ", round(slopes$CI_high, 2), "]"),
    paste0("$t_{student}$ (", slopes$df_error, ") = ", slopes$t),
    paste0("*p* ", format_p(slopes$p)),
    sep = ", "
  )

  if (nrow(slopes) > 1) {
    col_ind <- colnames(slopes[seq.int(to = ncol(slopes) - 8)])
    expr <- data.table::data.table(
      slopes[col_ind],
      expr,
      key = col_ind
    )
    numeric_vars <- expr[, names(.SD), .SDcols = is.numeric]
    expr[, (numeric_vars) := lapply(.SD, round, digits = 1), .SDcols = numeric_vars][]
  }

  return(expr)
}

#' Report smooth terms from a GAM model
#'
#' Get the text format of the statistics from the smooth term
#' fitted from a GAM model fitted using the `mgcv` package.
#'
#' @param model Object of class "gam".
#' @param ... Currently ignored.
#'
#' @export

report_smooth <- function(model, ...) {

  stopifnot(
    "Is not a GAM model" = inherits(model, "gam")
  )

  mod_summary <- summary(model)

  tbl <- data.table::as.data.table(
    x = mod_summary$s.table,
    keep.rownames = TRUE
  )

  tbl$df_error  <- round(x = stats::df.residual(model), digits = 2)
  tbl$`p-value` <- format_p(tbl$`p-value`)
  tbl$edf       <- round(tbl$edf, 2)
  tbl$F         <- round(tbl$F, 2)

  expr <- paste(
    paste0("$F_{smooth}$ (", tbl$edf, ", ", tbl$df_error, ") = ", tbl$F),
    paste0("*p* ", tbl$`p-value`),
    sep = ", ",
    recycle0 = TRUE
  )

  if (length(tbl$rn) > 1) {
    expr <- data.table::data.table(
      term = tbl$rn,
      expr,
      key = "term"
    )
  }

  return(expr)

}
