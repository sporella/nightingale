# #30díasdegráficos día 23
# Visualización desplazamientos en Renca
# https://twitter.com/CedeusChile/status/1268198733952569345
# Autora: Stephanie Orellana (@sporella)


# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(extrafont)

# loadfonts()


# Cargar y procesar datos -------------------------------------------------


datos <- read_csv("data/viajes_renca.csv")

uno <- datos %>% 
  mutate(nivel = "2",
         fill = categoria)

dos <- datos %>% 
  group_by(categoria) %>% 
  summarise(valor = sum(valor)) %>% 
  mutate(nivel= "1",
         fill = categoria,
         tipo = categoria)

sun <- bind_rows(uno, dos)


# Visualización -----------------------------------------------------------

p <- ggplot(sun, aes(x = nivel, y = valor, fill = categoria)) +
  geom_col(
    aes(alpha = nivel),
    width = 1,
    color = "gray90",
    size = 0.25,
    position = position_stack()
  ) +
  geom_text_repel(
    aes(label = tipo),
    size = 4,
    position = position_stack(vjust = 0.5),
    family = "Bahnschrift"
  ) +
  coord_polar(theta = "y") +
  scale_x_discrete(breaks = NULL, expand = c(0, 1)) +
  scale_alpha_manual(values = c("1" = 1, "2" = 0.6), guide = F) +
  scale_fill_manual(values = c("#ff3c8b",
                               "#00c489",
                               "#f2c019")) +
  theme(line = element_blank(), rect = element_blank(), text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10)) +
  labs(title = "Desplazamientos Comuna Renca",
       subtitle = "Datos obtenidos de la captura de pantalla de @CedeusChile.\nFuente: EOD, 2012",
       caption = "@sporella")

ggsave("plots/veintitres/viajes_renca.png", width = 6, height = 6)
