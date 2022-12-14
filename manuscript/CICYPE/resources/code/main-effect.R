
# Load packages and scripts -------------------------------------------------------------------

# Setup script
source("manuscript/CICYPE/prep-workspace.R")

# R Packages
library(ggplot2)
library(see)

# Figure 1a ------------------------------------------------------------------------------------

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

fig_a_data <- melt(
  data = dataset,
  measure.vars = y_vars
)

fig_a_data[, variable := `levels<-`(variable, names(y_vars))][]

# Scaled data for mean ± SE plotting
scaled <- copy(fig_a_data)
scaled[, edad_corregida_meses := ASQ3:::near(edad_corregida_meses, 5)][]

# 300 evenly-spaced data points of `edad_corregida_meses`
pred_data <- fig_a_data[, data.frame(
  edad_corregida_meses = seq.default(
    from = min(edad_corregida_meses, na.rm = TRUE),
    to = max(edad_corregida_meses, na.rm = TRUE),
    length.out = 300
  )
)]

# Bootstrapping with 100 replicates
set.seed(123)
boots <- fig_a_data[, {
  boot <- vapply(1:200, function(i) { # Vectorized "looping"

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
plot_fig_a <- function(var) {
  ggplot(fig_a_data[variable == var], aes(x = edad_corregida_meses, y = value)) +
    geom_count(alpha = 0.1, na.rm = TRUE) +
    geom_line(data = boots[variable == var], aes(group = boot, y = predicted), alpha = 0.02, col = "red") +
    geom_smooth(method = "gam",
                formula = y ~ s(x),
                col = "#ED0000FF",
                method.args = list(method = "REML"),
                na.rm = TRUE,
                se = FALSE) +
    stat_summary(data = scaled[variable == var], fun.data = mean_se, na.rm = TRUE, fatten = 3, col = "red4") +
    labs(x = "Edad corregida (meses)", y = paste("Puntaje", var)) +
    ggpubr::theme_pubr()
}

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

fig_b_data <- rbindlist(slopes, idcol = "response")
fig_b_data[, IC := fifelse(p < 0.05, "Significativo", "No significativo")
][, grp := rleid(IC)
][, response := factor(response, levels = names(models))]

plot_fig_b <- function(var) {
  ggplot(fig_b_data[response == var], aes(edad_corregida_meses, Coefficient)) +
    labs(x = "Edad corregida (meses)",
         y = "Efecto de la edad",) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_line(aes(group = 1, col = IC), linewidth = 1) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high, group = grp, fill = IC), alpha = 0.4, na.rm = TRUE) +
    ggpubr::theme_pubr() +
    scale_color_manual(values = ggsci::pal_lancet()(2), aesthetics = c("fill", "col"))
}

# Save plots --------------------------------------------------------------

plot_main_effect <- function(var, save = FALSE) {
  fig <- ggpubr::ggarrange(plot_fig_a(var), plot_fig_b(var), ncol = 2)
  if (save) {
    path <- gsub("%var%", var, "manuscript/CICYPE/resources/results_main_%var%.svg", fixed = TRUE)
    svglite::svglite(filename = path, height = 4, width = 8, always_valid = TRUE)
    print(fig)
    dev.off()
  } else {
    return(fig)
  }
}

for (i in names(y_vars)) {
  plot_main_effect(i, save = TRUE)
}
