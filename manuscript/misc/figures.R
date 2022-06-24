
# Load packages and scripts -------------------------------------------------------------------

# Setup script
source("manuscript/misc/setup.R")

# R Packages
library(ggplot2)
library(see)

# Figure 1 ------------------------------------------------------------------------------------

# Enviromental variable
.save_plot <- TRUE

# Effect of corrected age on developmental skills (many smooths)
# Communication score ~ corrected age
# Gross motor score ~ corrected age
# Fine motor score ~ corrected age
# Problem solving score ~ corrected age
# Socio-individual score ~ corrected age

y_vars <- c("Communication" = "comunicacion_total"
            , "Fine motor" = "motora_fina_total"
            , "Gross motor" = "motora_gruesa_total"
            , "Problem solving" = "resolucion_problemas_total"
          # , "Socio-individual" = "socio_individual_total"
          )

fig_data <- melt(
  data = dataset,
  measure.vars = y_vars
)

fig_data[, variable := `levels<-`(variable, names(y_vars))][]

scaled <- copy(fig_data)
scaled[, edad_corregida_meses := ASQ3:::near(edad_corregida_meses, 5)][]

fig_1 <- ggplot(fig_data, aes(x = edad_corregida_meses, y = value)) +
  facet_wrap(~variable, ncol = 2) +
  geom_count(alpha = 0.1, na.rm = TRUE) +
  stat_summary(data = scaled, fun.data = mean_se, na.rm = TRUE) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              col = "red4",
              method.args = list(method = "REML"),
              na.rm = TRUE) +
  labs(x = "Corrected age (months)", y = "Score",
       caption = "Mean ± SE in 5-unit intervals") +
  ggpubr::theme_pubr()

if (isTRUE(x = .save_plot)) {
  pdf("manuscript/fig-1.pdf", width = 7, height = 7);
  print(fig_1);
  dev.off()
}

# Figure 2 ------------------------------------------------------------------------------------

models <- list("Communication" = NULL,
               "Gross motor" = NULL,
               "Fine motor" = NULL,
               "Problem solving" = NULL)

models[[1]] <- gam(
  comunicacion_total ~
    s(profesional_id, sexo_paciente, respondedor_vinculo, bs = "re") +
    s(edad_corregida_meses),
  data = dataset,
  method = "REML"
  )

models[[2]] <- gam(
  motora_gruesa_total ~
    s(profesional_id, sexo_paciente, respondedor_vinculo, bs = "re") +
    s(edad_corregida_meses),
  data = dataset,
  method = "REML"
  )

models[[3]] <- gam(
  motora_fina_total ~
    s(profesional_id, sexo_paciente, respondedor_vinculo, bs = "re") +
    s(edad_corregida_meses),
  data = dataset,
  method = "REML"
  )

models[[4]] <- gam(
  resolucion_problemas_total ~
    s(profesional_id, sexo_paciente, respondedor_vinculo, bs = "re") +
    s(edad_corregida_meses),
  data = dataset,
  method = "REML"
  )

slopes <- sapply(
  X = models,
  FUN = modelbased::estimate_slopes,
  trend = "edad_corregida_meses",
  at = "edad_corregida_meses",
  length = 300,
  simplify = FALSE
)

fig2_data <- rbindlist(slopes, idcol = "response")
fig2_data[, Confidence := fifelse(p < 0.05, "Significant", "Not significant")
          ][, grp := rleid(Confidence)][]

fig_2 <- ggplot(fig2_data, aes(edad_corregida_meses, Coefficient)) +
  facet_wrap(~response) +
  labs(x = "Corrected age (months)",
       y = "Effect of Corrected age") +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(aes(group = 1)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, group = grp, fill = Confidence), alpha = 0.4) +
  ggpubr::theme_pubr()

if (isTRUE(x = .save_plot)) {
  pdf("manuscript/fig-2.pdf", width = 7, height = 7);
  print(fig_2);
  dev.off()
}
