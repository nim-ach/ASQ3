
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
  legend = c(T,T,T,F,F),
  SIMPLIFY = FALSE
)

f0 <- ggpubr::ggarrange(plotlist = plots[1:3], nrow = 1, common.legend = TRUE)
f1 <- ggpubr::ggarrange(plotlist = plots[4:5], nrow = 1)
figure1 <- ggpubr::ggarrange(f0, f1, ncol = 1)

local({
  pdf("manuscript/figures/fig-1.pdf", width = 15, height = 10);
  print(figure1);
  dev.off()
})

# Comparing models
mod_comparison <- lapply(models, function(i) {
  out <- as.data.table(
    x = do.call(
    what = AIC,
    args = unname(i)
    )
  )
  out[, model := names(i)][]
})

mod_comparison <- rbindlist(mod_comparison, idcol = "Response")
best_worst_mods <- mod_comparison[, .SD[AIC %in% range(AIC), list(AIC, model)], list(Response)]

best_worst_mods[, Classification := ifelse(max(AIC) == AIC, "Worst", "Best"), Response
                ][, tapply(paste0(round(AIC), " (", model, ")"), Classification, c, simplify = FALSE), list(Response)]

# Evaluation smooth significance
lapply(models, function(i) {
  lapply(i, report_smooth)
})

models$CM$`Full model` |> plot()
