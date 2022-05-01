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
  ## These errors will be corrected later on.

  # Modify variable names
  names(dataset) <- readLines("data-raw/helpers/colnames")

  # Set data to data.table
  setDT(dataset)


# Fix data-entry errors -----------------------------------------------------------------------

  corrections <- read.csv("data-raw/helpers/corrections.csv")
  n <- nrow(corrections)

  # `fecha_evaluacion` as character (latter on will be transformed to date format)
  dataset[, fecha_evaluacion := as.character(fecha_evaluacion)]

  for (i in seq_len(n)) {
    i_var <- corrections[i, "param_in"]

    ind <- like(
      vector = dataset[[i_var]],
      pattern = corrections[i, "search_for"],
      ignore.case = TRUE
    )

    set(
      x = dataset,
      i = which(ind),
      j = corrections[i, "param_out"],
      value = corrections[i, "replace"]
    )
  }

  rm(corrections, i, i_var, ind, n)

# Treat data ----------------------------------------------------------------------------------

  col_names <- names(dataset) # for later use

  # Processing date variables
  ind <- grep("fecha", col_names, value = TRUE)
  dataset[, fecha_nacimiento := as.numeric(fecha_nacimiento)]
  dataset[, (ind) := lapply(.SD, data.table::as.IDate, origin = "1899-12-30"), .SDcols = ind]

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

  # Add patient sex
  dataset <- merge(
    x = dataset,
    y = fread("data-raw/helpers/rut_sex.csv"),
    by = "rut_paciente",
  )

  # Removing ind and col_names objects
  rm(ind, col_names)


# Subject anonymisation -----------------------------------------------------------------------


  # Check if any RUT is missing
  if (any(is.na(dataset$rut_paciente))) stop("Hay un rut perdido, explorarlo")

  # ID for each professional
  dataset[, profesional_id := as.numeric(x = factor(profesional_nombre))]

  # and then we eliminate the column with their name
  dataset[, profesional_nombre := NULL]

  # ID for each patient
  dataset[, paciente_id := as.numeric(x = factor(rut_paciente))]

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

  ind <- grep(
    pattern = "profesional|paciente|vinculo|tipo|interpret",
    x = names(dataset),
    value = TRUE
  )

  dataset[, (ind) := lapply(.SD, factor), .SDcols = ind]

  rm(ind)

# Reorder columns -----------------------------------------------------------------------------

  setcolorder(dataset, neworder = readLines("data-raw/helpers/colorder"))
  setkey(dataset, paciente_id, fecha_evaluacion)

# Export dataset ------------------------------------------------------------------------------

  use_data(dataset, overwrite = TRUE)
  fwrite(dataset, file = "data-raw/dataset.csv")
