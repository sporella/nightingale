# #30díasdegráficos día 11
# Visualización causas incendios forestales en Chile
# https://stat.ine.cl/?lang=es&SubSessionId=06710c95-0c1d-451a-9847-ffa76ec9080d#
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------


library(tidyverse)


# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/incendios_numero.csv", locale = locale("es")) %>%
  pivot_longer(cols = -1,
               names_to = "causa",
               values_to = "valor")



# Visualización -----------------------------------------------------------


sub <- "En Chile la principal causa de incendios forestales es el actuar humano, tanto intencional como accidental.
En 2015, 3273 incendios forestales de carácter intencional quemaron alrededor de 60.000 hectáreas. En 2017, fueron quemadas más de 152.000 hectáreas por esta causa"

p <- ggplot(data, aes(x = factor(year), y = causa, fill = valor)) +
  geom_tile(colour = "white", show.legend = F) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  geom_text(aes(label = valor, colour = valor), show.legend = F) +
  scale_color_gradient2(
    low = "purple",
    mid = "#40e0d0",
    high = "lavender",
    midpoint = 1620
  ) +
  labs(
    y = "",
    x = "",
    title = toupper("Número de Incendios Forestales en Chile Según Causa"),
    subtitle = str_wrap(sub, width = 100),
    caption = "@sporella\nDatos: stat.ine.cl"
  ) +
  theme(axis.text.x = element_text(size=12, face="bold", colour="#a80c58"),
        axis.text.y = element_text(size=12, colour="#001d85"),
        plot.title.position = "plot",
        plot.title = element_text(size=30, hjust=0.5, color = "#a80c58"),
        plot.subtitle = element_text(hjust=1, size= 12),
        panel.background = element_rect(fill="white")
  )


ggsave("plots/once/incendios.png", p, width = 13, height = 6)
