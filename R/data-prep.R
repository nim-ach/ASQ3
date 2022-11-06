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
#' dataset <- readxl::read_excel("data-raw/raw-data.xlsx")
#' ```
#'
#' Subsequently, the column names were changed to syntactically valid versions, in
#' this case in camel case (e.g. "Column A" to "column_a").
#'
#' ```r
#' names(dataset) <- readLines("data-raw/helpers/colnames")
#' ```
#'
#' Once completed, the data was transformed into a `data.table' format, which allows
#' for the efficient implementation of a function (or functions) across multiple columns
#' (using selection criteria for greater precision).
#'
#' ```r
#' dataset <- data.table::setDT(dataset)
#' ```
#'
#' @section Fixing data-entry errors:
#'
#' As any real world dataset, data entry errors are common. In this context, being able
#' to handle them is crucial in the cleaning step when we first import the dataset.
#' For this, a separate csv file was used, which had four columns:
#' 1. `param_in`: The variable we need to filter on to get to the exact case were the
#' error was introduced.
#' 2. `search_for`: The query passed on `param_in` as the filter for subsetting.
#' 3. `param_out`: The variable on which the correction is needed.
#' 4. `replace`: The correction itself.
#'
#' And with this structure, the data entry errors were handled as follow:
#'
#' For every row in the csv file with the corrections, we filter on `param_in` using
#' `search_for` to select the case(s) where the error occur; then, we replace de value
#' of `param_out` using `replace`. The code with the aforementioned details can be seen
#' below:
#'
#' ```r
#' corrections <- read.csv("data-raw/helpers/corrections.csv")
#' n <- nrow(corrections)
#'
#' # `fecha_evaluacion` as character (latter on will be transformed to date format)
#' dataset[, fecha_evaluacion := as.character(fecha_evaluacion)]
#'
#' for (i in seq_len(n)) {
#'   i_var <- corrections[i, "param_in"]
#'
#'   ind <- data.table::like(
#'     vector = dataset[[i_var]],
#'     pattern = corrections[i, "search_for"],
#'     ignore.case = TRUE
#'   )
#'
#'   data.table::set(
#'     x = dataset,
#'     i = which(ind),
#'     j = corrections[i, "param_out"],
#'     value = corrections[i, "replace"]
#'   )
#' }
#' ```
#'
#' @section Formatting variables:
#'
#' Once the import was complete, we proceeded to standardise the formatting of the
#' columns by using regular expressions (RegEx) to select the columns, starting with
#' the modification of the text variables.
#'
#' ```r
#' col_names <- names(dataset) # getting the column names
#'
#' # Processing date variables
#' ind <- grep("fecha", col_names, value = TRUE)
#' dataset[, fecha_nacimiento := as.numeric(fecha_nacimiento)]
#' dataset[, (ind) := lapply(.SD, data.table::as.IDate, origin = "1899-12-30"), .SDcols = ind]
#'
#' # Labels to "title-case"
#' ind <- grep("nombre|interpretacion|especialidad", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, stringr::str_to_title), .SDcols = ind]
#'
#' # Labels to "lower-case"
#' ind <- grep("agrupacion|diagnostico", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, tolower), .SDcols = ind]
#'
#' # numerical variables
#' ind <- grep("total", col_names, value = TRUE)
#' dataset[, (ind) := lapply(.SD, as.numeric), .SDcols = ind]
#'
#' # asq3_meses to numeric
#' dataset[, asq3_meses := as.numeric(x = gsub("MESES", "", asq3_meses))]
#'
#' # Add patient sex
#' dataset <- merge(
#'   x = dataset,
#'   y = fread("data-raw/helpers/rut_sex.csv"),
#'   by = "rut_paciente",
#' )
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
#' # ID for each professional
#' dataset[, profesional_id := as.numeric(x = factor(profesional_nombre))]
#'
#' # and then we eliminate the column with their name
#' dataset[, profesional_nombre := NULL]
#'
#' # ID for each patient
#' dataset[, paciente_id := as.numeric(x = factor(rut_paciente))]
#'
#' # and then we eliminate the column with their name
#' dataset[, nombre_paciente := NULL]
#'
#' # and the the column with their RUT
#' dataset[, rut_paciente := NULL]
#'
#' # Also eliminate the column with the responder's name
#' dataset[, nombre_respondedor := NULL]
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
#' of the information, so any requests for it will be evaluated.
#'
#' @seealso
#' - \link[=dataset]{dataset}
#'
#' @name data_prep
NULL
