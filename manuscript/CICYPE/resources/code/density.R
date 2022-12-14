source("manuscript/CICYPE/prep-workspace.R")

fig <- ggarrange(
  density_plot(comunicacion_total, "CM"),
  density_plot(motora_gruesa_total, "GM"),
  density_plot(motora_fina_total, "FM"),
  density_plot(resolucion_problemas_total, "CG"),
  density_plot(socio_individual_total, "PS")
)

svglite::svglite(filename = "manuscript/CICYPE/resources/density.svg", width = 8, height = 5)
print(fig)
dev.off()
