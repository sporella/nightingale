# #30díasdegráficos día 25
# Visualización Seguridad de Contraseñas
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)


# Cargar y procesar datos -------------------------------------------------


passwords <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv'
  ) %>%
  filter(!is.na(category))


# Visualización -----------------------------------------------------------


p <- ggplot(passwords, aes(x = category, y = strength, fill = category)) +
  geom_violin(trim = F) +
  scale_x_discrete(
    labels = function(x)
      gsub("-", "\n", x)
  ) +
  scale_fill_manual(
    values = c(
      "#ffa96b",
      "#45bdff",
      "#92f214",
      "#b89cff",
      "#ffe564",
      "#f679c8",
      "#3cf2ff",
      "#84af68",
      "#dcdbff",
      "#c0ffdb"
    )
  ) +
  labs(
    title = "Seguridad de Contraseñas Según Categoría",
    subtitle = "Datos de tidytuesday.\nFuente: Knowledge is Beautiful",
    y = "Seguridad",
    x = "",
    caption = "@sporella"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave("plots/veinticinco/contraseñas.png", p, width = 7, height = 5)
