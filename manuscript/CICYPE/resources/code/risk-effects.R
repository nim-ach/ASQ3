# Preparamos el ambiente de trabajo ---------------------------------------

## Script de preparación
source("manuscript/CICYPE/prep-workspace.R")


# Preparamos los datos ----------------------------------------------------

ind <- grep("interpretacion", names(dataset), value = TRUE)
names(ind) <- c("CM", "GM", "FM", "CG", "PS")

plot_risk_effects <- function(var, var_name = var, save = FALSE) {
  form <- as.formula(
    paste0(ind[[var]], " ~ ",
           "s(profesional_id, bs = \"re\") +
            s(sexo_paciente, bs = \"re\") +
            s(respondedor_vinculo, bs = \"re\") +",
           "s(edad_corregida_meses)")
  )

  set.seed(1234)
  output <- mgcv::gam(form, family = stats::binomial(link = "logit"), data = dataset,
                      method = "REML")

  format_p <- function(x) {
    x <- data.table::fifelse(
      test = x < 0.001,
      yes = "< 0.001",
      no = paste("=", round(x, 3))
    )

    return(x)
  }

  p_val <- paste("p", format_p(x = stats::anova(output)$s.table[4L, 4L]))

  testdata <- expand.grid(
    profesional_id = c(2),
    sexo_paciente = c("F"),
    respondedor_vinculo = c("Madre"),
    edad_corregida_meses = seq(0, 48, 0.05)
  )

  predicted_response <- as.data.table(x = stats::predict(output, testdata, se.fit = TRUE, type = "response"))
  predicted_response <- cbind(predicted_response, testdata)

  names(predicted_response)[1:2] <- c("Fit", "SE")

  ylab <- "P(Categoría | Edad corregida)"
  if (!is.null(var_name) && is.character(var_name)) {
    ylab <- paste0("P(", paste("Retraso en", var_name), " | Edad corregida)")
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
    ggplot2::labs(x = "Edad corregida (meses)", y = ylab, fill = "IC:", col = "IC:") +
    ggplot2::theme(legend.position = "top")

  if (isFALSE(legend)) {
    plot <- plot + ggplot2::theme(legend.position = "none")
  }

  if (save) {
    path <- gsub("%var%", var, "manuscript/CICYPE/resources/results_risk_%var%.svg", fixed = TRUE)
    svglite::svglite(filename = path, height = 4, width = 8, always_valid = TRUE)
    print(plot)
    dev.off()
  } else {
    return(plot)
  }
}

for (i in names(ind)) {
  plot_risk_effects(i, save = TRUE)
}


