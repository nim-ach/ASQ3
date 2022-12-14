
# Load packages and scripts -------------------------------------------------------------------

source("manuscript/CICYPE/prep-workspace.R")

# Table 1 -------------------------------------------------------------------------------------

tbl_data <- copy(dataset)[, sexo_paciente := `levels<-`(sexo_paciente, c("Mujer", "Hombre"))]

gtsummary::theme_gtsummary_language("es")

tbl1 <- tbl_summary(tbl_data,
                    by = "sexo_paciente",
                    include = c(
                      "semanas_prematurez",
                      "edad_cronologica_meses",
                      "edad_corregida_meses",
                      "comunicacion_total",
                      "motora_gruesa_total",
                      "motora_fina_total",
                      "resolucion_problemas_total",
                      "socio_individual_total",
                    ),
                    label = list(
                      semanas_prematurez ~ "Prematuridad (semanas)",
                      edad_cronologica_meses ~ "Edad cronológica",
                      edad_corregida_meses ~ "Edad corregida",
                      comunicacion_total ~ "Puntaje CM",
                      motora_gruesa_total ~ "Puntaje GM",
                      motora_fina_total ~ "Puntaje FM",
                      resolucion_problemas_total ~ "Puntaje CG",
                      socio_individual_total ~ "Puntaje PS"
                    ), digits = everything() ~ 1)

tbl1 <- add_overall(x = tbl1)

tbl1 <- add_p(
  x = tbl1,
  test = everything() ~ "wilcox.test",
  pvalue_fun = function(x) round(x, digits = 3)
)

# tbl1 <- modify_header(
#   x = tbl1,
#   all_stat_cols(stat_0 = FALSE) ~ "**{level}**<br>N = {n}",
#   stat_0 = "**Overall**<br>N = {N}"
# )

tbl1 <- as_gt(tbl1)

saveRDS(tbl1, file = "manuscript/CICYPE/resources/table-1.RDS")
