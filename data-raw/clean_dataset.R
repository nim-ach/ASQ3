# Load libraries
library(readxl)
library(data.table)

# Import data ---------------------------------------------------------------------------------

  # Import the data directly from Excel
  dataset <- read_excel(path = "data-raw/raw-data.xlsx", sheet = 2)[,-1L]

  ## Expect warning message:
  ## Expecting date in B1415 / R1415C2: got '2/15/0222'
  ##
  ## Note: Expected and assumed as a data-entry error.
  ## These error were corrected in helpers/fix_dates.R

  # Modify variable names
  names(dataset) <- readLines("data-raw/helpers/colnames")

  # Set data to data.table
  setDT(dataset)


# Treat data ----------------------------------------------------------------------------------


  col_names <- names(dataset) # for later use

  # Labels to "title-case"
  ind <- grep("nombre|interpretacion|especialidad", col_names, value = TRUE)
  dataset[, (ind) := lapply(.SD, stringr::str_to_title), .SDcols = ind]

  # Labels to "lower-case"
  ind <- grep("agrupacion|diagnostico", col_names, value = TRUE)
  dataset[, (ind) := lapply(.SD, tolower), .SDcols = ind]

  # numerical variables
  ind <- grep("total", col_names, value = TRUE)
  dataset[, (ind) := lapply(.SD, as.numeric), .SDcols = ind]

  # asq3_meses to numeric
  dataset[, asq3_meses := as.numeric(x = gsub("MESES", "", asq3_meses))]

  # Processing dates
  ind <- grep("fecha", col_names, value = TRUE)

  # Fix individual cases for dates issues (data-entry errors)
  source("data-raw/helpers/fix_dates.R")

  dataset[, fecha_nacimiento := as.numeric(fecha_nacimiento)]
  dataset[, (ind) := lapply(.SD, data.table::as.IDate, origin = "1899-12-30"), .SDcols = ind]

  # Removing ind and col_names objects
  rm(ind, col_names)


# Subject anonymisation -----------------------------------------------------------------------


  # Drop missing values
  dataset <- dataset[!is.na(rut_paciente) & !is.na(nombre_paciente)]

  # ID for each professional
  dataset[, profesional_id := as.numeric(x = factor(profesional_nombre))]
  # and then we eliminate the column with their name
  dataset[, profesional_nombre := NULL]

  # ID for each patient
  dataset[, paciente_id := as.numeric(x = factor(nombre_paciente))]
  # and then we eliminate the column with their name
  dataset[, nombre_paciente := NULL]
  # and the the column with their RUT
  dataset[, rut_paciente := NULL]

  # Also eliminate the column with the responder's name
  dataset[, nombre_respondedor := NULL]


# Corrected age -------------------------------------------------------------------------------


  # Custom function
  edad_corregida <- function(x, y) {
    # standardize number of weeks in a month
    weeks_in_a_month <- 4.34524
    # Months old to weeks old
    weeks_old <- x * weeks_in_a_month
    # Correction as [weeks old] - [preterm weeks]
    corrected_weeks <- weeks_old - y
    # Then change back to months the corrected age
    corrected_monts <- corrected_weeks / weeks_in_a_month
    # Finally, return the rounded values
    round(corrected_monts)
  }

  # Compute the column with the corrected age using previous function
  dataset[, edad_corregida_meses := edad_corregida(edad_cronologica_meses, semanas_prematurez)]

  # Removing function already used
  rm(edad_corregida)


# Set factor variables ------------------------------------------------------------------------

  dataset[, `:=`(
    profesional_id = factor(profesional_id),
    profesional_especialidad = factor(profesional_especialidad),
    paciente_id = factor(paciente_id),
    respondedor_vinculo = factor(respondedor_vinculo),
    evaluacion_tipo = factor(evaluacion_tipo),
    comunicacion_interpretacion = factor(comunicacion_interpretacion),
    motora_fina_interpretacion = factor(motora_fina_interpretacion),
    motora_gruesa_interpretacion = factor(motora_gruesa_interpretacion),
    socio_individual_interpretacion = factor(socio_individual_interpretacion),
    resolucion_problemas_interpretacion = factor(resolucion_problemas_interpretacion)
  )]

# Reorder columns -----------------------------------------------------------------------------

  setcolorder(dataset, neworder = readLines("data-raw/helpers/colorder"))

# Export dataset ------------------------------------------------------------------------------

  use_data(dataset, overwrite = TRUE)
  fwrite(dataset, file = "data-raw/dataset.csv")
