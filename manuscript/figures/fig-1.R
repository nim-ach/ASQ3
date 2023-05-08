
# Model comparisons -------------------------------------------------------

## Choosing between simple or adjusted models

source(file = "manuscript/misc/setup.R")

params <- grep("total$", names(dataset), value = TRUE)

names(params) <- c("CM", "GM", "FM", "CG", "PS")

plots <- mapply(
  FUN = gam_confounders,
  data = list(dataset),
  var = params,
  var_name = names(params),
  legend = c(T,T,F,F,F),
  SIMPLIFY = FALSE
)

f0 <- ggpubr::ggarrange(plotlist = plots[1:2], nrow = 1, common.legend = TRUE)
f1 <- ggpubr::ggarrange(plotlist = plots[3], nrow = 1)
f2 <- ggpubr::ggarrange(plotlist = plots[4:5], nrow = 1)
figure1 <- ggpubr::ggarrange(f0, f1, f2, ncol = 1)

local({
  pdf("manuscript/figures/fig-1.pdf", width = 8, height = 10);
  print(figure1);
  dev.off()
  jpeg("manuscript/figures/fig-1.jpeg", width = 8, height = 10, units = "in", res = 400);
  print(figure1);
  dev.off()
})
