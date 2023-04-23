## Cargamos librerias
library(rmarkdown)
library(officer)

## Generamos los documentos
files <- list.files(
  path = "manuscript/",
  pattern = "\\.Rmd$",
  ignore.case = TRUE,
  full.names = TRUE
)

## Usamos un bucle
for (file in files) {
  render(input = file)
}

## Adjuntamos la tabla al final
read_docx("manuscript/manuscript.docx") |>
  body_add_break() |>
  body_add_docx(src = "manuscript/tables/table-1.docx") |>
  body_add_break() |>
  body_add_docx(src = "manuscript/captions.docx") |>
  print(target = "manuscript/manuscript.docx")

## Volvemos a generar los gráficos
source("manuscript/figures/make_plots.R")

zip::zip(
  zipfile = "manuscript/submission_docs/ASQ3-files.zip",
  files = c(
    "manuscript/manuscript.docx",
    "manuscript/supplementary.docx",
    "manuscript/figures/fig-1.pdf",
    "manuscript/figures/fig-1.tiff",
    "manuscript/figures/fig-2.pdf",
    "manuscript/figures/fig-2.tiff",
    "manuscript/figures/fig-3.pdf",
    "manuscript/figures/fig-3.tiff",
    "manuscript/misc/STROBE checklist.doc"
  ),
  include_directories = FALSE
)
