# Preparamos el ambiente de trabajo ---------------------------------------

## Script de preparación
source("manuscript/misc/setup.R")

## Cargamos paquetes
library(ggplot2)

# Preparamos los datos ----------------------------------------------------

ind <- grep("total", names(dataset), value = TRUE)
names(ind) <- c("CM", "GM", "FM", "CG", "PS")

plots <- mapply(
  FUN = gam_ordinal,
  data = list(dataset),
  var = ind,
  var_name = names(ind),
  legend = c(T,T,F,F,F),
  threshold = 35,
  SIMPLIFY = FALSE
)

f0 <- ggpubr::ggarrange(plotlist = plots[1:2], nrow = 1, common.legend = TRUE)
f1 <- ggpubr::ggarrange(plotlist = plots[3], nrow = 1)
f2 <- ggpubr::ggarrange(plotlist = plots[4:5], nrow = 1)
figure3 <- ggpubr::ggarrange(f0, f1, f2, ncol = 1)

local({
  pdf("manuscript/figures/fig-3.pdf", width = 10, height = 12);
  print(figure3);
  dev.off()
})


# Evaluacion de los modelos -----------------------------------------------

models <- mapply(
  FUN = gam_ordinal,
  var = ind,
  data = list(dataset),
  plot = FALSE,
  threshold = 35,
  SIMPLIFY = FALSE,
  USE.NAMES = TRUE
)



lapply(models, report_overall)
