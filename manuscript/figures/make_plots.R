files <- grep(
  pattern = "make_plots",
  x = list.files(
    path = "manuscript/figures",
    pattern = "\\.R$",
    full.names = TRUE
  ),
  invert = TRUE,
  value = TRUE
)

for (file in files) {
  source(file)
}
