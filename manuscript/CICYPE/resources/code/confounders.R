source("manuscript/CICYPE/prep-workspace.R")

params <- grep("total$", names(dataset), value = TRUE)

names(params) <- c("CM", "GM", "FM", "CG", "PS")

plots <- mapply(
  FUN = gam_confounders_es,
  data = list(dataset),
  var = params,
  var_name = names(params),
  legend = c(T,T,T,F,F),
  SIMPLIFY = FALSE
)

f0 <- ggpubr::ggarrange(plotlist = plots[1:3], nrow = 1, common.legend = TRUE)
f1 <- ggpubr::ggarrange(plotlist = plots[4:5], nrow = 1)
fig <- ggpubr::ggarrange(f0, f1, ncol = 1)

svglite::svglite(filename = "manuscript/CICYPE/resources/confounders.svg", width = 11.25, height = 6)
print(fig)
dev.off()
