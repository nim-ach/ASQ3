
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

## communication (CM), gross motor (GM), fine motor (FM), problem-solving (CG), and personal-social (PS)

y_vars <- c("CM" = "comunicacion_total"
            , "GM" = "motora_gruesa_total"
            , "FM" = "motora_fina_total"
            , "CG" = "resolucion_problemas_total"
            , "PS" = "socio_individual_total")

fig_data <- melt(
  data = dataset,
  measure.vars = y_vars
)

fig_data[, variable := `levels<-`(variable, names(y_vars))][]

# Scaled data for mean ± SE plotting
scaled <- copy(fig_data)
scaled[, edad_corregida_meses := ASQ3:::near(edad_corregida_meses, 5)][]

# 300 evenly-spaced data points of `edad_corregida_meses`
pred_data <- fig_data[, data.frame(
  edad_corregida_meses = seq.default(
    from = min(edad_corregida_meses, na.rm = TRUE),
    to = max(edad_corregida_meses, na.rm = TRUE),
    length.out = 300
  )
)]

# Bootstrapping with 100 replicates
boots <- fig_data[, {
  boot <- vapply(1:100, function(i) { # Vectorized "looping"

    # ith Model fitting
    mod <- gam(value ~ s(edad_corregida_meses), method = "REML", data = .SD[sample(.N, replace = TRUE)])

    # Estimation from bootstrapped model
    predict(
      object = mod,
      newdata = pred_data
    )
  }, FUN.VALUE = numeric(length = nrow(pred_data)))

  # Join the exposure and response
  cbind(pred_data, boot)

}, by = variable] # Grouped by variable

# Melt the wide data into long data (for compatibility with ggplot)
boots <- melt(boots, id.vars = 1:2, variable.name = "boot", value.name = "predicted")

# Plot the data
fig_1a <- ggplot(fig_data, aes(x = edad_corregida_meses, y = value)) +
  facet_grid(variable ~ ., scale = "free_y") +
  geom_count(alpha = 0.1, na.rm = TRUE) +
  geom_line(data = boots, aes(group = boot, y = predicted), alpha = 0.1) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              col = "red3",
              lwd = 0.7,
              method.args = list(method = "REML"),
              na.rm = TRUE,
              se = FALSE) +
  stat_summary(data = scaled, fun.data = mean_se, na.rm = TRUE) +
  labs(x = "Corrected age (months)", y = "Score") +
  ggpubr::theme_pubr()

# Figure 1b ------------------------------------------------------------------------------------

## communication (CM), gross motor (GM), fine motor (FM), problem-solving (CG), and personal-social (PS)

models <- list("CM" = NULL,
               "GM" = NULL,
               "FM" = NULL,
               "CG" = NULL,
               "PS" = NULL)

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
          ][, grp := rleid(Confidence)
          ][, response := factor(response, levels = names(models))]

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

