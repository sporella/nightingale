# #30díasdegráficos día 26
# Visualización Digimon
# https://www.kaggle.com/rtatman/digidb
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(janitor)
library(extrafont)
library(ggmosaic)
library(ggimage)
library(ggthemes)
# loadfonts()


# Cargar y procesar datos -------------------------------------------------


data <- read_csv("data/digimon.csv") %>% 
  clean_names()


# Visualización -----------------------------------------------------------


p <- ggplot(data = data) +
  geom_mosaic(aes(x = product(attribute), fill = type),
              colour = "black",
              show.legend = F) +
  labs(
    x = "Atributo",
    y = "Tipo",
    title = "DIGIMON",
    subtitle = "Tipos v/s Atributos",
    caption = "@sporella"
  ) +
  scale_fill_manual(values = c("#abfff5",
                               "#fcb9da",
                               "#fffac7",
                               "#ada9b9")) +
  theme_clean() +
  theme(
    text = element_text(
      face = "bold",
      family = "Bahnschrift",
      colour = "#de9be8"
    ),
    plot.title = element_text(hjust = 0.5, size = 40),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    axis.text.x = element_text(
      face = "bold",
      size = 12,
      colour = "#de9be8"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      colour = "#de9be8"
    ),
    axis.title.x = element_text(hjust = 0, size = 10),
    axis.title.y = element_text(hjust = 0, size = 10),
    panel.grid.major.x = element_line(
      size = 1,
      linetype = "dotted",
      colour = "grey90"
    ),
    panel.grid.major.y = element_line(
      size = 1,
      linetype = "dotted",
      colour = "grey90"
    )
  )


back <- "img/digimon.png"

pi <- ggimage::ggbackground(p, back)  

ggsave("plots/veintiseis/digimon.png", pi, width = 8, height = 5.5)
