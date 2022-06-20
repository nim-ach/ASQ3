
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
            , "Gross motor" = "motora_gruesa_total"
            , "Fine motor" = "motora_fina_total"
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
  theme_lucid(legend.position = "top")

if (isTRUE(x = .save_plot)) {
  pdf("manuscript/fig-1.pdf", width = 7, height = 7);
  print(fig_1);
  dev.off()
}
