# #30díasdegráficos día 7
# Visualización distribución temperaturas estación Quinta Normal
# http://explorador.cr2.cl/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggridges)
library(ggthemes)


# Cargar y procesar datos -------------------------------------------------

data_min <- read_csv("data/quintanormal_min.csv") %>% rename(tmin = valor)
data_max <- read_csv("data/quintanormal_max.csv") %>% rename(tmax = valor)

data <- left_join(data_min, data_max) %>%
  mutate(tmean = (tmax + tmin) / 2) %>%
  filter(!is.na(tmean)) %>% 
  pivot_longer(cols = starts_with("t"), names_to = "variable") %>%
  mutate(
    variable = factor(
      variable,
      levels = c("tmin", "tmean", "tmax"),
      labels = c("Mínimas", "Medias", "Máximas")
    ),
    mes = factor(
      mes,
      levels = rev(unique(mes)),
      labels = rev(locale("es")$date_names$mon)
    )
  )


data2020 <- data %>% filter(agno == 2020)


# Visualización -----------------------------------------------------------

p <- ggplot() +
  geom_density_ridges_gradient(
    data = data,
    show.legend = F,
    aes(x = value,
        y = mes,
        fill = stat(x)),
    colour = "grey20"
  ) +
  geom_density_ridges_gradient(
    data = data2020,
    aes(x = value, y = mes, colour = "Año 2020"),
    show.legend = T,
    linetype = "dashed",
    size = 0.55,
    fill = "transparent"
  ) +
  facet_grid(~ variable) +
  scale_fill_gradientn(
    colours = c(
      '#00429d',
      '#64c0d0',
      '#b9ffd5',
      '#ffffaa',
      '#ffc339',
      '#ff6e00',
      '#ff0000'
    )
  ) +
  scale_colour_manual(values = c("Año 2020" = "#de78cb")) +
  scale_y_discrete(expand = c(0.2, 0.15)) +
  labs(
    y = "Mes",
    x = "Temperatura °C",
    colour = "",
    title = "ESTACIÓN QUINTA NORMAL",
    subtitle = "Distribución de temperaturas 1950-2020",
    caption = "Datos: explorador climático (CR)2\n@sporella"
  ) +
  theme_hc() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0, 0),
    panel.grid.major.x = element_line(colour = "#D8D8D8"),
    plot.title = element_text(size = 20),
    axis.title = element_text(colour = "grey10"),
    strip.background = element_rect(fill = "#03fcdf")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 1)))

ggsave("plots/siete/temps.png", p, width = 6, height = 6)
