# #30díasdegráficos día 29
# Visualización nómina selección chilena
# http://www.anfp.cl/noticia/34618/nomina-de-la-seleccion-chilena-para-amistoso-con-peru
# https://www.kaggle.com/sagunsh/fifa-20-complete-player-dataset
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggrepel)


# Cargar y procesar datos -------------------------------------------------


fifa <- read_csv("data/fifa20_data.csv")

nomina <- c("Miiko Albornoz",
            "Charles Aránguiz",
            "Gabriel Arias",
            "Claudio Baeza",
            "Christian Bravo",
            "Claudio Bravo",
            "Nicolás Castillo",
            "Luis Felipe Gallegos",
            "Mauricio Isla",
            "Guillermo Maripán",
            "Gary Medel",
            "Jean Meneses",
            "Felipe Mora",
            "Esteban Pavez",
            "Erick Pulgar",
            "Lorenzo Reyes",
            "Francisco Sierralta",
            "Diego Valdés",
            "Sebastián Vegas",
            "Arturo Vidal",
            "Alexis Sánchez")
parametros <- c("Attacking", "Movement", "Power", "Goalkeeping", "Mentality", "Defending", "Skill")
parametrost <- c("Ataque", "Movimiento", "Poder", "Portería", "Mentalidad", "Defensa", "Habilidad")

data_chile <- fifa %>% 
  filter(Country == "Chile", Name %in% nomina) %>% 
  pivot_longer(cols = all_of(parametros), names_to = "Parametro", values_to = "Valor") %>% 
  select(Name, Parametro, Valor) %>% 
  mutate(Parametro = factor(Parametro, levels = parametros, labels = parametrost))



# Visualización -----------------------------------------------------------

cols <- c("#d944c5",
          "#86d7d2",
          "#9c59e7",
          "#d6d74b",
          "#7678d9",
          "#799b38",
          "#cd75c1",
          "#79e09c",
          "#74d94e",
          "#de467c",
          "#5f935f",
          "#679dcd",
          "#c78b3a",
          "#96769e",
          "#d3d19d",
          "#cf7173",
          "#628f89",
          "#d5b8d2",
          "#af886e",
          "#e15336")

sub <- "El partido nunca se jugó debido a que #Chiledespertó y esta fue la última nómina dada la suspensión de 2020. El partido se jugaba en Perú, pero los jugadores recicibieron críticas por querer jugar igual considerando la situación del país, por lo que en definitiva decidieron no jugar. Alexis estaba lesionado pero lo incluí igual."

p <- ggplot(data_chile, aes(x = Parametro, y = Valor, group = Name, colour = Name))+
  geom_line(size = 0.6) +
  geom_point(size = 2)+
  scale_colour_manual(values = cols)+
  scale_x_discrete(expand = expansion(add = c(0.5, 3)))+
  geom_text_repel(seed = 5432,
                  data = data_chile %>% filter(Parametro == "Habilidad"),
                  aes(label = Name), segment.size = 0.4,
                  fontface = "bold",
                  size = 2,
                  direction = "x",
                  nudge_x = 0.5,
                  nudge_y = 0.1
  ) +
  labs(title = "Estadísticas de la selección chilena en FIFA20\nNómina del partido amistoso con Perú en noviembre de 2019 (+ Alexis)",
       subtitle = str_wrap(sub, width = 135) ,
       caption = "@sporella",
       x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.subtitle = element_text(size =8))

ggsave("plots/veintinueve/seleccion_chile.png", p, width = 7.5, height = 5.5)
