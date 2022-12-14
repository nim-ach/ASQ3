# Cargamos paquetes -------------------------------------------------------

source("manuscript/CICYPE/prep-workspace.R")

# Términos Smooth -------------------------------------------------------

y_vars <- grep("interpretacion", names(dataset), value = TRUE)
names(y_vars) <- c("CM", "GM", "FM", "CG", "PS")

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
    family = stats::binomial(link = "logit"),
    method = "REML"
  )
  report_smooth(model)
}, simplify = FALSE)

tbl_data <- rbindlist(many_models, idcol = "Variable")
tbl_data <- tbl_data[term %like% "edad_corregida_meses", list(Variable, Estadístico = expr)]

tbl <- knitr::kable(tbl_data, format = "pipe")
saveRDS(tbl, file = "manuscript/CICYPE/resources/table-risk-smooth.RDS")


# Slopes ------------------------------------------------------------------

table_risk_effects <- function(var) {
  ind <- grep("interpretacion", names(dataset), value = TRUE)
  names(ind) <- c("CM", "GM", "FM", "CG", "PS")
  form <- as.formula(
    paste0(ind[[var]], " ~ ",
           "s(profesional_id, bs = \"re\") +
            s(sexo_paciente, bs = \"re\") +
            s(respondedor_vinculo, bs = \"re\") +",
           "s(edad_corregida_meses)")
  )

  set.seed(1234)
  model <- mgcv::gam(form, family = stats::binomial(link = "logit"), data = dataset)

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
  saveRDS(table_risk_effects(i),
          file = paste0("manuscript/CICYPE/resources/table-risk-",i,".RDS"))
}
