
# Versión en español de las funciones en el paquete ASQ3

gam_confounders_es <- function(data, var, seed = 1234, legend = TRUE, var_name = NULL, plot = TRUE, ...) {
  models = list(
    # Simple model - only smooth term
    `Simple` = "var ~ s(edad_corregida_meses)",
    # Testing possible confounders one by one
    `Solo sexo` = "var ~ s(sexo_paciente, bs = \"re\") + s(edad_corregida_meses)",
    `Clínico` = "var ~ s(profesional_id, bs = \"re\") + s(edad_corregida_meses)",
    `Vínculo` = "var ~ s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    # Testing combination of pairs
    `Clínico + Sexo` = "var ~ s(profesional_id, bs = \"re\") + s(sexo_paciente, bs = \"re\") + s(edad_corregida_meses)",
    `Clínico + Vínculo` = "var ~ s(profesional_id, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    `Sexo + Vínculo` = "var ~ s(sexo_paciente, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)",
    # And full model
    `Ajuste completo` = "var ~ s(profesional_id, bs = \"re\") + s(sexo_paciente, bs = \"re\") + s(respondedor_vinculo, bs = \"re\") + s(edad_corregida_meses)"
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
    variable.name = "Ajustado por:",
    value.name = "fit"
  )

  ylab <- "Puntaje estimado"
  if (!is.null(var_name) && is.character(var_name)) {
    ylab <- paste("Puntaje de", var_name, "estimado")
  }

  plot <- ggplot2::ggplot(predicted_response, ggplot2::aes(edad_corregida_meses, fit)) +
    ggplot2::facet_grid(rows = ggplot2::vars(sexo_paciente), cols = ggplot2::vars(respondedor_vinculo),
                        labeller = ggplot2::labeller(
                          sexo_paciente = c(F = "Mujer", M = "Hombre"),
                        )) +
    ggplot2::geom_line(ggplot2::aes(col = `Ajustado por:`, lty = `Ajustado por:`)) +
    ggplot2::labs(x = "Edad corregida (en meses)", y = ylab) +
    ggdist::theme_ggdist() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  if (isFALSE(legend)) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }

  return(plot)
}

# Gráfico de densidad

density_plot <- function(var, label) {
  ggplot(dataset, aes({{var}})) +
    geom_density(fill = "lightblue") +
    labs(y = NULL, x = label) +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean,
                 geom = "vline", orientation = "y", col = "red") +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = median,
                 geom = "vline", orientation = "y", col = "blue") +
    theme_classic()
}

# Entrega valor p de la prueba de shapiro

shapiro_test <- function(x) {
  p <- shapiro.test(x)$p.value
  paste("p", ASQ3:::format_p(p))
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
