# Preparamos el ambiente de trabajo ---------------------------------------

## Script de preparación
source("manuscript/misc/setup.R")

## Cargamos paquetes
library(ggplot2)

# Preparamos los datos ----------------------------------------------------

ind <- grep("interpretacion", names(dataset), value = TRUE)
names(ind) <- c("CM", "GM", "FM", "CG", "PS")


plots <- mapply(
  FUN = gam_binomial,
  data = list(dataset),
  var = ind,
  var_name = names(ind),
  legend = c(F,T,T,F,F),
  SIMPLIFY = FALSE
)

f0 <- ggpubr::ggarrange(plotlist = plots[2:3], nrow = 1, common.legend = TRUE)
f1 <- ggpubr::ggarrange(plotlist = plots[1], nrow = 1)
f2 <- ggpubr::ggarrange(plotlist = plots[4:5], nrow = 1)
figure3 <- ggpubr::ggarrange(f0, f1, f2, ncol = 1)

local({
  pdf("manuscript/figures/fig-3.pdf", width = 8, height = 10);
  print(figure3);
  dev.off()
  tiff("manuscript/figures/fig-3.tiff", width = 8, height = 10, units = "in", res = 400);
  print(figure3);
  dev.off()
})


# Evaluacion de los modelos -----------------------------------------------

models <- mapply(
  FUN = gam_binomial,
  var = ind,
  data = list(dataset),
  plot = FALSE,
  SIMPLIFY = FALSE,
  USE.NAMES = TRUE
)

lapply(models, summary)
