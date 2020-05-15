# #30díasdegráficos día 4
# Visualización habilidades pokemon set de Datos de miércoles
# https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggimage)
library(extrafont)

# Instalar fuente pokemon:
# https://fontmeme.com/fuentes/fuente-pokmon/

extrafont::loadfonts()


# Cargar y procesar datos -------------------------------------------------

pokemon <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

pokemon_long <- pokemon %>% 
  pivot_longer(cols = puntos_vida:velocidad, names_to = "habilidad") %>% 
  group_by(tipo_1, habilidad) %>% 
  summarise(total = mean(value)) %>% 
  mutate(habilidad = str_to_title(str_replace_all(string = habilidad, pattern = "_", replacement = " ")))



# Visualización -----------------------------------------------------------

p <-ggplot(pokemon_long, aes(x=habilidad, y=total, fill=habilidad))+
  geom_col(alpha=0.9) +
  facet_wrap(~tipo_1) +
  scale_x_discrete(expand = c(0,0))+
  labs(title = "Pokémon",
       subtitle = "Cuadro de Habilidades",
       x = "",
       y = "Valor",
       fill = "",
       caption = "@sporella")+
  scale_fill_manual(values = c("#205500",
                               "#73356b",
                               "#b248b8",
                               "#ff68a9",
                               "#ffaa52",
                               "#00d8b2"))+
  theme_dark()+
  theme(text = element_text(family = "Bahnschrift"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.subtitle = element_text(
          colour = "grey15",
          family = "Bahnschrift",
          size = 20,
          hjust = 0.5,
          vjust = 0.5
        ),
        plot.title = element_text(
          colour = "grey15",
          family = "Pokemon Solid",
          size = 50,
          hjust = 0.5,
          vjust = 0.5
        ),
        legend.position = "bottom"
  )

# * Incluir fondo con paquete ggimage -------------------------------------

back <- ("img/pokemon.png")
pf <- ggbackground(p, back)  

ggsave("plots/cuatro/habilidades_pokemon.png", pf, width = 7.31, height = 5.46)
