
# Table 1 -------------------------------------------------------------------------------------

tbl_data <- copy(dataset)[, sexo_paciente := `levels<-`(sexo_paciente, c("Female", "Male"))]

tbl1 <- tbl_summary(tbl_data,
                    by = "sexo_paciente",
                    include = c(
                      "semanas_prematurez",
                      "edad_cronologica_meses",
                      "edad_corregida_meses",
                      "asq3_meses",
                      "comunicacion_total",
                      "motora_gruesa_total",
                      "motora_fina_total",
                      "resolucion_problemas_total",
                      "socio_individual_total",
                    ),
                    label = list(
                      semanas_prematurez ~ "Prematurity weeks",
                      edad_cronologica_meses ~ "Chronological age",
                      edad_corregida_meses ~ "Corrected age",
                      asq3_meses ~ "Developmental age (ASQ-3)",
                      comunicacion_total ~ "Communication score",
                      motora_gruesa_total ~ "Gross motor score",
                      motora_fina_total ~ "Fine motor score",
                      resolucion_problemas_total ~ "Problem solving score",
                      socio_individual_total ~ "Socio-individual score"
                    ), digits = everything() ~ 1)

tbl1 <- add_overall(x = tbl1)

tbl1 <- add_p(
  x = tbl1,
  test = everything() ~ "wilcox.test",
  pvalue_fun = function(x) style_pvalue(x, digits = 3)
)

tbl1 <- modify_header(
  x = tbl1,
  all_stat_cols(stat_0 = FALSE) ~ "**{level}**<br>N = {n}",
  stat_0 = "**Overall**<br>N = {N}"
)

