
# Load packages and scripts -------------------------------------------------------------------

# Setup script
source("manuscript/misc/setup.R")

# R Packages
library(ggplot2)
library(see)

# Figure 1a ------------------------------------------------------------------------------------

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
            , "Socio-individual" = "socio_individual_total")

fig_data <- melt(
  data = dataset,
  measure.vars = y_vars
)

fig_data[, variable := `levels<-`(variable, names(y_vars))][]

scaled <- copy(fig_data)
scaled[, edad_corregida_meses := ASQ3:::near(edad_corregida_meses, 5)][]

fig_1a <- ggplot(fig_data, aes(x = edad_corregida_meses, y = value)) +
  facet_grid(variable ~ ., scale = "free_y") +
  geom_count(alpha = 0.1, na.rm = TRUE) +
  stat_summary(data = scaled, fun.data = mean_se, na.rm = TRUE) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              col = "red3",
              method.args = list(method = "REML"),
              na.rm = TRUE) +
  labs(x = "Corrected age (months)", y = "Score") +
  ggpubr::theme_pubr()

# Figure 1b ------------------------------------------------------------------------------------

models <- list("Communication" = NULL,
               "Gross motor" = NULL,
               "Fine motor" = NULL,
               "Problem solving" = NULL,
               "Socio-individual" = NULL)

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

models[[5]] <- gam(
  socio_individual_total ~
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

fig_1b_data <- rbindlist(slopes, idcol = "response")
fig_1b_data[, Confidence := fifelse(p < 0.05, "Significant", "Not significant")
          ][, grp := rleid(Confidence)][]

fig_1b <- ggplot(fig_1b_data, aes(edad_corregida_meses, Coefficient)) +
  facet_grid(response ~ ., scales = "free_y") +
  labs(x = "Corrected age (months)",
       y = "Effect of Corrected age",) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(aes(group = 1, col = Confidence), lwd = 1) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high, group = grp, fill = Confidence), alpha = 0.4) +
  ggpubr::theme_pubr() +
  scale_color_manual(values = ggsci::pal_lancet()(2), aesthetics = c("fill", "col"))

fig_1 <- ggpubr::ggarrange(fig_1a, fig_1b, ncol = 2)

if (isTRUE(x = .save_plot)) {
  pdf("manuscript/fig-1.pdf", width = 12, height = 10);
  print(fig_1);
  dev.off()
}

