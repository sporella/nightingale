# #30díasdegráficos día 21
# Visualización menú starbucks Chile
# https://www.starbucks.cl/media/Comida-Nutricional_tcm102-14772.pdf
# Autora: Stephanie Orellana (@sporella)


# Cargar librerías --------------------------------------------------------
library(tidyverse)
library(ggforce)
library(janitor)


# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/starbucks_chile.csv") %>% 
  clean_names()


# Visualización -----------------------------------------------------------


desc <- "Esta torta es muy sospechosa, ¡dice que no tiene azúcares!"
desc2 <- "Este cheesecake es una bola de azúcar y energía"
desc3 <- "Esta barrita es una opción al Cheesecake por la mitad del precio"

p <- ggplot(data,
            aes(x = energia_kc, y = azucares_totales_g)) +
  geom_point(aes(colour = categoria), size = 3) +
  scale_x_continuous(limits = c(NA, 900)) +
  scale_y_continuous(limits = c(NA, 80)) +
  scale_colour_manual(values = c("#cea2e5",
                                 "#62bab0",
                                 "#ffd49a",
                                 "#d7a076"))+
  geom_mark_circle(
    aes(
      filter = azucares_totales_g < 1 & categoria == "Tortas",
      label = producto,
      description = desc
    ),
    label.fontsize = 6
  ) +
  geom_mark_circle(
    aes(
      filter = azucares_totales_g > 55 & categoria == "Tortas",
      label = producto,
      description = desc2
    ),
    label.fontsize = 6
  ) +
  geom_mark_circle(
    aes(
      filter = producto == "Barrita de Nuez & Manjar",
      label = producto,
      description = desc3
    ),
    label.fontsize = 6
  ) +
  labs(
    title = "Menú Starbucks Chile",
    caption = "@sporella\nEs mejor hornear en casa :)",
    x = "Energía [kc]",
    y = "Azúcares totales [g]",
    colour = ""
  ) +
  guides(colour = guide_legend(reverse = T))+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20))
p

ggsave("plots/veintiuno/starbucks.png", p, width = 7.5, height = 6)
