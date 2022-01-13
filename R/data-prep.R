#' Data preparation process
#'
#' @description
#'
#' The \link[=dataset]{dataset} preparation
#' process occurs in several steps, some of which involve manual processing of the
#' data using Excel and programmatic preparation using R. The programmatic part and
#' the steps used for data treatment are detailed here.
#'
#' @section From Excel to R:
#'
#' Once the data entry using Excel was complete, it was imported into R using the
#' `read_excel` function of the [readxl](https://readxl.tidyverse.org/) package.
#'
#' ```r
#' dataset <- readxl::read_excel("path/to/excel-file.xlsx")
#' ```
#'
#' Subsequently, the column names were changed to syntactically valid versions, in
#' this case in camel case (e.g. "Column A" to "column_a").
#'
#' ```r
#' names(dataset) <- c("evaluacion_numero", "evaluacion_fecha", "respondedor_nombre", ...)
#' ```
#'
#' Once completed, the data was transformed into a `data.table' format, which allows
#' for the efficient implementation of a function (or functions) across multiple columns
#' (using selection criteria for greater precision).
#'
#' ```r
#' dataset <- data.table::as.data.table(dataset)
#' ```
#'
#' @section Formatting variables:
#'
#' Once the import was complete, we proceeded to standardise the formatting of the
#' columns by using regular expressions (RegEx) to select the columns, starting with
#' the modification of the text variables.
#'
#' ```r
#' col_names <- names(dataset) # for later use
#'
#' # Labels to "title-case"
#' ind <- grep("nombre|interpretacion|especialidad", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, stringr::str_to_title), .SDcols = ind]
#'
#' # Labels to "lower-case"
#' ind <- grep("agrupacion|diagnostico", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, tolower), .SDcols = ind]
#' ```
#'
#' We perform a similar process with the numeric and date variables, assigning them
#' the necessary properties to be analysed later.
#'
#' ```r
#' # numerical variables
#' ind <- grep("total", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, as.numeric), .SDcols = ind]
#'
#' # asq3_meses to numeric
#' dataset[, asq3_meses := as.numeric(x = gsub("MESES", "", asq3_meses))]
#'
#' # Processing dates
#' ind <- grep("fecha", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, data.table::as.IDate), .SDcols = ind]
#' ```
#'
#' @section Subject anonymisation:
#'
#' In order to treat and maintain the confidentiality of the identity of the
#' patients, as well as that of the evaluating professionals, an identifying
#' number was assigned to each individual, excluding from this process those
#' who had their [RUT](https://www.sii.cl/contribuyentes/contribuyentes_individuales/chilenos_extranjero/rol_unico_tributario.htm)
#' or name missing (i.e. missing data).
#'
#' ```r
#' # Drop missing values
#' dataset <- dataset[!is.na(rut_2) & !is.na(paciente_nombre)]
#'
#' # ID for each professional
#' dataset[, profesional_id := as.numeric(x = factor(profesional_nombre))]
#' # and then we eliminate the column with their name
#' dataset[, profesional_nombre := NULL]
#'
#' # ID for each patient
#' dataset[, paciente_id := as.numeric(x = factor(paciente_nombre))]
#' # and then we eliminate the column with their name
#' dataset[, paciente_nombre := NULL]
#' # and the the column with their RUT
#' dataset[, `:=`(rut_1 = NULL, rut_2 = NULL)]
#'
#' # Also eliminate the column with the responder's name
#' dataset[, respondedor_nombre := NULL]
#' ```
#'
#' @section Corrected age:
#'
#' In the raw data, we have the age of the patients in months and the
#' weeks of prematurity, which allows us to calculate the corrected age
#' of the patients in months, the latter being calculated using an auxiliary
#' function.
#'
#' ```r
#' # Custom function
#' edad_corregida <- function(x, y) {
#'   # standardize number of weeks in a month
#'   weeks_in_a_month <- 4.34524
#'
#'   # Months old to weeks old
#'   weeks_old <- x * weeks_in_a_month
#'
#'   # Correction as [weeks old] - [preterm weeks]
#'   corrected_weeks <- weeks_old - y
#'
#'   # Then change back to months the corrected age
#'   corrected_monts <- corrected_weeks / weeks_in_a_month
#'
#'   # Finally, return the rounded values
#'   round(corrected_monts)
#' }
#'
#' # Compute the column with the corrected age using previous function
#' dataset[, edad_corregida_meses := edad_corregida(edad_cronologica_meses, semanas_prematurez)]
#' ```
#'
#' @note
#'
#' The original data is not available in the package, given the sensitive nature
#' of the information, so any requests for it will be ignored or rejected.
#'
#' @seealso
#' - \link[=dataset]{dataset}
#'
#' @name data-prep
NULL
