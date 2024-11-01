library(dplyr)
library(tidyverse)

# Datos de FavoriteCandy. Obtenidos de https://www.kaggle.com/datasets/victoriadlc/pick-your-favorite-candy-in-autumn-spooky-season

datos <- read.csv("FavoriteCandy.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


# Crear el objeto datos_dulce calculando el promedio de calificaciones y normalizando a porcentajes

datos_dulce <- datos %>%
  group_by(How.young.are.you.) %>%
  summarise(
    Snickers = mean(Snickers, na.rm = TRUE),
    KitKat = mean(KitKat, na.rm = TRUE),
    Twix = mean(Twix, na.rm = TRUE),
    Ghirardelli = mean(Ghirardelli.Squares, na.rm = TRUE),
    Reeses = mean(Reeses, na.rm = TRUE),
    JollyRancher = mean(JollyRancher, na.rm = TRUE),
    Twizzlers = mean(Twizzlers..Red.Vines, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -How.young.are.you., names_to = "Dulce", values_to = "Promedio") %>%
  group_by(How.young.are.you.) %>%
  mutate(Porcentaje = (Promedio / sum(Promedio)) * 100)

# Crear el gráfico 

ggplot(datos_dulce, aes(x = Dulce, y = Porcentaje, fill = Dulce)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ How.young.are.you., scales = "free_y") +
  scale_fill_manual(values = c("#FFA500", "#FF4500", "#551A8B", "#4B0082", "#FFD700", "#000000", "#32CD32")) +
  labs(
    title = "Preferencias de dulces por grupo de edad (en %)",
    x = "Tipo de dulce",
    y = "%",
    fill = " "
  ) +
  theme_minimal(base_family = "Comic Sans MS") +
  theme(
    plot.background = element_rect(fill = "#1A1A1A"),
    panel.background = element_rect(fill = "#1A1A1A"),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", color = "#FFA500"),
    axis.title.x = element_text(size = 16, face = "bold", color = "#FFA500"),
    axis.title.y = element_text(size = 16, face = "bold", color = "#FFA500"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "#FFFFFF", size = 12),
    axis.text.y = element_text(color = "#FFFFFF", size = 12),
    strip.text = element_text(size = 14, face = "bold", color = "#FFFFFF"),
    legend.position = "bottom",
    legend.title = element_text(size = 14, color = "#FFFFFF"),
    legend.text = element_text(size = 12, color = "#FFFFFF")
  )





