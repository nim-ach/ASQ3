library(ggplot2)

fig <- ggplot(MASS::mcycle, aes(times, accel)) +
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, aes(col = "GAM")) +
  geom_smooth(method = "lm", se = F, aes(col = "LM")) +
  labs(col = "Modelos",
       title = "Datos simulados de un accidente en motocicleta",
       subtitle = "Medidas de aceleración de la cabeza",
       caption = "Fuente: Silverman, B. W. (1985).",
       x = "Tiempo", y = "Aceleración") +
  scale_color_manual(values = c(GAM = "blue2", LM = "red3")) +
  theme_classic()

svglite::svglite(filename = "manuscript/CICYPE/resources/gams_vs_lm.svg", width = 6, height = 6)
print(fig)
dev.off()

