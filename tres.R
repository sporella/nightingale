# #30díasdegráficos día 3
# Visualización datos FIFA19
# https://www.kaggle.com/karangadiya/fifa19
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggthemes)
library(ggforce)

# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/fifa19.csv") %>%
  clean_names()


equipos <- c("Real Madrid", "FC Bayern München", "FC Barcelona", "Atlético Madrid",
             "Liverpool", "Paris Saint-Germain")

data_equipos <- data %>%
  mutate(wage2 = as.numeric(str_extract(wage, pattern = "[:digit:]+"))) %>%
  filter(club %in% equipos, position != "GK")


# Modificar tema de ggplot ------------------------------------------------
# basado en ggthemes::theme_fivethirtyeight()

theme_modi <- function (base_size = 12,
                        base_family = "sans")
{
  colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  (
    theme_foundation(base_size = base_size, base_family = base_family) +
      theme(
        line = element_line(colour = "black"),
        rect = element_rect(
          fill = colors["Light Gray"],
          linetype = 0,
          colour = NA
        ),
        text = element_text(colour = colors["Dark Gray"]),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.background = element_rect(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        panel.grid = element_line(colour = NULL),
        panel.grid.major = element_line(colour = colors["Medium Gray"]),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          hjust = 0,
          size = rel(1.5),
          face = "bold"
        ),
        plot.margin = unit(c(1,
                             1, 1, 1), "lines"),
        strip.background = element_rect()
      )
  )
}



# Gráfico en ggplot2 y anotaciones con ggforce ----------------------------

p <- ggplot(data_equipos,
            aes(
              y = age,
              x = overall,
              size = wage2,
              colour = club,
              fill = club
            )) +
  geom_point(alpha = 0.7,
             shape = 21,
             colour = "grey33") +
  scale_size(
    breaks = floor(seq(1, 600, length.out = 5)),
    limits = c(1, 600),
    range = c(2, 15),
    labels = function(x) {
      paste0("€", x, "K")
    }
  ) +
  scale_fill_manual(values = c(
    "#7f62b8",
    "#b84c7d",
    "#b8553c",
    "#00a6b5",
    "#bd9d3c",
    "#46c19a"
  )) +
  theme_modi() +
  labs(
    x = "Rendimiento general",
    y = "Edad",
    fill = "Club",
    size = "Salario",
    colour = "Club",
    title = "Estadísticas juego FIFA19",
    caption = "@sporella"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 8)),
         size = guide_legend(order = 1))
p <- p+
  geom_mark_circle(aes(filter = age<20 & overall>80, label = name), 
                   colour="black",fill="transparent",size=1,
                   show.legend = F)+
  geom_mark_circle(aes(filter = age>30 & overall>93, label = name), 
                   colour="black",fill="transparent",size=1,
                   show.legend = F)
p

ggsave("plots/tres/fifa.png",p, width = 6.59, height = 5.46)
