#' Data from the ASQ-3 study from CADI-UMAG
#'
#'
#' @format A data frame with 1523 rows y 55 variables:
#'
#' \itemize{
#'   \item{\code{profesional_id}: Factor. Identifying number of the assessing professional.}
#'   \item{\code{profesional_especialidad}: Factor. Profession of the professional evaluator.}
#'   \item{\code{paciente_id}: Factor. Patient identifier number.}
#'   \item{\code{sexo_paciente}: Factor. Patient's sex.}
#'   \item{\code{respondedor_vinculo}: Factor. Type of relationship between the respondent and the patient.}
#'   \item{\code{n_evaluacion}: Factor. Number of assessment for each subject.}
#'   \item{\code{evaluacion_tipo}: Factor. Type of assessment.}
#'   \item{\code{fecha_evaluacion}: Date. Date of patient assessment.}
#'   \item{\code{fecha_nacimiento}: Date. Patient's date of birth.}
#'   \item{\code{edad_cronologica_meses}: Numeric. Chronological age of the patient in months.}
#'   \item{\code{semanas_prematurez}: Numeric. Weeks of prematurity.}
#'   \item{\code{edad_corregida_meses}: Numeric. Chronological age of the patient in months corrected for weeks of prematurity.}
#'   \item{\code{diagnostico}: Character. Patient's diagnostic group.}
#'   \item{\code{alteracion}: Character. Patient's alteration group.}
#'   \item{\code{asq3_meses}: Numeric. Age in months of patient development according to ASQ 3.}
#'   \item{\code{comunicacion_q1 - comunicacion_q6}: Numeric. Scoring of the individual questions of the communication item.}
#'   \item{\code{comunicacion_total}: Numeric. Total score of the communication item.}
#'   \item{\code{comunicacion_interpretacion}: Factor. Interpretation of the communication item.}
#'   \item{\code{motora_gruesa_q1 - motora_gruesa_q6}: Numeric. Scoring of the individual questions of the gross motor skills item.}
#'   \item{\code{motora_gruesa_total}: Numeric. Total score of the gross motor skills item.}
#'   \item{\code{motora_gruesa_interpretacion}: Factor. Interpretation of the gross motor skills item.}
#'   \item{\code{motora_fina_q1 - motora_fina_q6}: Numeric. Scoring of the individual questions of the fine motor skills item.}
#'   \item{\code{motora_fina_total}: Numeric. Total score of the fine motor skills item.}
#'   \item{\code{motora_fina_interpretacion}: Factor. Interpretation of the fine motor skills item.}
#'   \item{\code{resolucion_problemas_q1 - resolucion_problemas_q6}: Numeric. Scoring of the individual questions of the problem solving item.}
#'   \item{\code{resolucion_problemas_total}: Numeric. Total score of the problem solving item.}
#'   \item{\code{resolucion_problemas_interpretacion}: Factor. Interpretation of the problem solving item.}
#'   \item{\code{socio_individual_q1 - socio_individual_q6}: Numeric. Scoring of the individual questions of the socio-individual skills item.}
#'   \item{\code{socio_individual_total}: Numeric. Total score of the socio-individual skills item.}
#'   \item{\code{socio_individual_interpretacion}: Factor. Interpretation of the socio-individual skills item.}
#' }
#'
#' @section Ages and Stages Questionnaire:
#'
#'   The Ages & Stages Questionnaires, Third Edition (ASQ-3) is a developmental screening tool that pinpoints developmental
#'   progress in children between the ages of one month to 5.5 years. For more information please see the homepage of the
#'   questionnaire [ASQ-3](https://agesandstages.com/products-pricing/asq3/).
#'
#' @seealso
#' - \link[=data_prep]{Data preparation process}
#'
#' @source CADI-UMAG, Punta Arenas, Chile.
"dataset"

