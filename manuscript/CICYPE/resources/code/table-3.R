# Load packages and scripts -------------------------------------------------------------------

source("manuscript/CICYPE/prep-workspace.R")

y_vars <- c("CM" = "comunicacion_total"
            , "FM" = "motora_fina_total"
            , "GM" = "motora_gruesa_total"
            , "CG" = "resolucion_problemas_total"
            , "PS" = "socio_individual_total")

many_models <- sapply(y_vars, function(i) {
  form <- as.formula(
    paste0(i, " ~ ",
           "s(profesional_id, bs = \"re\") +
            s(sexo_paciente, bs = \"re\") +
            s(respondedor_vinculo, bs = \"re\") +",
           "s(edad_corregida_meses)")
  )
  model <- gam(
    form,
    data = dataset,
    method = "REML"
  )
  report_smooth(model)
}, simplify = FALSE)

tbl_data <- rbindlist(many_models, idcol = "Variable")

tbl_data <- tbl_data[term %like% "edad_corregida_meses", list(Variable, Estadístico = expr)]

tbl <- knitr::kable(tbl_data, format = "pipe")


saveRDS(tbl, file = "manuscript/CICYPE/resources/table-3.RDS")
