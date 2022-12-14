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

#' Plot GAM model
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

#' Plot varying random effects structure GAM model
#'
#' Display graphical representation different gam models on the same
#' response using different random effects structure and comparing
#' the estimates.
#'
#' @param data Dataset from where to extract response and independent variables.
#' @param var Character. Response variable to test on gam models with varying random effects structures.
#' @param seed Numeric. Seed for random processes within gams.
#' @param legend Logical. Whether to include the figure legend.
#' @param var_name Character. The label for the y-axis. Default is NULL, so "Predicted score" is used.
#' @param plot Logical. Whether to return the plot. If FALSE, then the models with the varying random effects structures are returned.
#' @param ... Currently not used.
#'
#' @export

gam_confounders <- function(data, var, seed = 1234, legend = TRUE, var_name = NULL, plot = TRUE, ...) {
  models = list(
    # Simple model - only smooth term
    `Simple` = "var ~ s(edad_corregida_meses)",
    # Testing possible confounders one by one
    `Only sex` = "var ~ s(sexo_paciente, bs = \"re\") + s(edad_corregida_meses)",
    `Clinician` = "var ~ s(profesional_id, bs = \"re\") + s(edad_corregida_meses)",
    `Respondent` = "var ~ s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    # Testing combination of pairs
    `Clinician + Sex` = "var ~ s(profesional_id, bs = \"re\") + s(sexo_paciente, bs = \"re\") + s(edad_corregida_meses)",
    `Clinician + Respondent` = "var ~ s(profesional_id, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    `Sex + Respondent` = "var ~ s(sexo_paciente, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    # And full model
    `Full model` = "var ~ s(profesional_id, bs = \"re\") + s(sexo_paciente, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)"
  )

  models <- lapply(models, stats::as.formula)
  models <- lapply(models, `[[<-`, 2, as.name(var))

  set.seed(seed)
  output <- lapply(models, mgcv::gam, data = data, method = "REML")

  if (isFALSE(plot)) {
    return(output)
  }

  testdata <- expand.grid(
    profesional_id = c(2),
    sexo_paciente = c("F", "M"),
    respondedor_vinculo = c("Madre","Padre","Abuelo/a","Tio/a"),
    edad_corregida_meses = seq(0, 48, 0.05)
  )

  testdata <- data.table::as.data.table(testdata)

  predicted_response <- lapply(
    output,
    stats::predict,
    type = "response",
    newdata = testdata
  )

  predicted_response <- data.table::as.data.table(predicted_response)
  predicted_response <- cbind(predicted_response, testdata)
  predicted_response <- data.table::melt.data.table(
    predicted_response,
    id.vars = c("profesional_id","sexo_paciente","respondedor_vinculo","edad_corregida_meses"),
    variable.name = "Adjusted for:",
    value.name = "fit"
  )

  ylab <- "Predicted score"
  if (!is.null(var_name) && is.character(var_name)) {
    ylab <- paste("Predicted", var_name, "score")
  }

  plot <- ggplot2::ggplot(predicted_response, ggplot2::aes(edad_corregida_meses, fit)) +
    ggplot2::facet_grid(rows = ggplot2::vars(sexo_paciente), cols = ggplot2::vars(respondedor_vinculo),
                        labeller = ggplot2::labeller(
                          sexo_paciente = c(F = "Female", M = "Male"),
                          respondedor_vinculo = c(Madre = "Mother", Padre = "Father", `Abuelo/a` = "Grandparent", `Tio/a` = "Uncle")
                        )) +
    ggplot2::geom_line(ggplot2::aes(col = `Adjusted for:`, lty = `Adjusted for:`)) +
    ggplot2::labs(x = "Corrected age (in months)", y = ylab) +
    ggdist::theme_ggdist()

  if (isFALSE(legend)) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }

  return(plot)
}

# Global variables
utils::globalVariables(
  names = c("edad_corregida_meses",
            "respondedor_vinculo",
            "sexo_paciente",
            "fit",
            "Adjusted for:"),
  add = TRUE
)


#' Plot logistic GAM
#'
#' Display graphical representation of logistic regression gam models
#' on the response using the random effects structure and comparing
#' the estimates.
#'
#' @param data Dataset from where to extract response and independent variables.
#' @param var Character. Response variable to test on gam models with varying random effects structures.
#' @param seed Numeric. Seed for random processes within gams.
#' @param legend Logical. Whether to include the figure legend.
#' @param var_name Character. The label for the y-axis. Default is NULL, so "Predicted score" is used.
#' @param plot Logical. Whether to return the plot. If FALSE, then the models with the varying random effects structures are returned.
#' @param ... Currently not used.
#'
#' @export

gam_binomial <- function(data, var, seed = 1234, legend = TRUE, var_name = NULL, plot = TRUE, ...) {
  model <- "%var% ~ s(profesional_id, bs = \"re\") + s(sexo_paciente, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)"
  model <- gsub("%var%", replacement = var, model)
  model <- stats::as.formula(model)

  set.seed(seed)
  output <- mgcv::gam(model, family = stats::binomial(link = "logit"), data = data,
                      method = "REML")

  if (isFALSE(plot)) {
    return(output)
  }

  p_val <- paste("p", format_p(x = summary(output)$s.table[4L, 4L]))

  testdata <- expand.grid(
    profesional_id = c(2),
    sexo_paciente = c("F"),
    respondedor_vinculo = c("Madre"),
    edad_corregida_meses = seq(0, 48, 0.05)
  )

  predicted_response <- as.data.table(x = stats::predict(output, testdata, se.fit = TRUE, type = "response"))
  predicted_response <- cbind(predicted_response, testdata)

  names(predicted_response)[1:2] <- c("Fit", "SE")

  ylab <- "P(Category | Corrected Age)"
  if (!is.null(var_name) && is.character(var_name)) {
    ylab <- paste0("P(", paste("Delay in", var_name), " | Corrected Age)")
  }

  ci_prob <- function(p) stats::qnorm((p+1)/2, lower.tail = TRUE)

  plot <- ggplot2::ggplot(predicted_response, ggplot2::aes(edad_corregida_meses, Fit)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = ifelse((Fit - SE * ci_prob(p = .95)) < 0, 0, (Fit - SE * ci_prob(p = .95))),
        ymax = ifelse(Fit + SE * ci_prob(p = .95) > 1, 1, Fit + SE * ci_prob(p = .95)),
        fill = "95%"
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ifelse((Fit - SE * ci_prob(p = .80)) < 0, 0, (Fit - SE * ci_prob(p = .80))),
        ymax = ifelse(Fit + SE * ci_prob(p = .80) > 1, 1, Fit + SE * ci_prob(p = .80)),
        fill = "80%"
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ifelse(Fit - SE * ci_prob(p = .5) < 0, 0, Fit - SE * ci_prob(p = .5)),
        ymax = ifelse(Fit + SE * ci_prob(p = .5) > 1, 1, Fit + SE * ci_prob(p = .5)),
        fill = "50%"
      )
    ) +
    ggplot2::geom_line(col = "white") +
    ggplot2::scale_fill_manual(values = c("95%" = "#FEE0D2", "80%" = "#FC9272", "50%" = "#DE2D26"), aesthetics = c("col", "fill")) +
    ggdist::theme_ggdist() +
    ggplot2::geom_label(ggplot2::aes(x = 10, y = .8, label = p_val)) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::labs(x = "Corrected age (in months)", y = ylab, fill = "CI:", col = "CI:") +
    ggplot2::theme(legend.position = "top")

  if (isFALSE(legend)) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }

  return(plot)
}

# Global variables
utils::globalVariables(
  names = c("edad_corregida_meses",
            "respondedor_vinculo",
            "sexo_paciente",
            "fit", "Fit",
            "SE",
            "variable",
            "Adjusted for:"),
  add = TRUE
)
