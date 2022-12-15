# Load packages and scripts -------------------------------------------------------------------


# Cargamos paquetes -------------------------------------------------------

source("manuscript/CICYPE/prep-workspace.R")


# Ajustamos modelos -------------------------------------------------------

table_main_effects <- function(var) {
  y_vars <- c("CM" = "comunicacion_total"
              , "GM" = "motora_gruesa_total"
              , "FM" = "motora_fina_total"
              , "CG" = "resolucion_problemas_total"
              , "PS" = "socio_individual_total")
  form <- as.formula(
    paste0(y_vars[[var]], " ~ ",
           "s(profesional_id, bs = \"re\") +
            s(sexo_paciente, bs = \"re\") +
            s(respondedor_vinculo, bs = \"re\") +",
           "s(edad_corregida_meses)")
  )
  set.seed(123)
  model <- gam(
    form,
    data = dataset,
    method = "REML"
  )
  slopes <- report_slopes(model, "edad_corregida_meses")
  if(is.atomic(slopes)) {
    slopes <- data.table(Estadístico = slopes)
  } else{
    slopes <- slopes[i = as.numeric(substrRight(expr, 5)) < 0.05,
                     j = list(Inicio = Start, Termino = End, Estadístico = expr)]
  }

  slopes[, exp(as.numeric(gsub(",", "", substr(Estadístico, 10, 16))))]
  tbl <- knitr::kable(slopes, format = "pipe")
  return(tbl)
}

for (i in names(y_vars)) {
  saveRDS(table_main_effects(i),
          file = paste0("manuscript/CICYPE/resources/table-main-",i,".RDS"))
}

