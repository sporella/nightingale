# #30díasdegráficos día 9
# Visualización idiomas en películas
# https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset?select=IMDb+movies.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggforce)
library(ggimage)


# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/IMDb movies.csv") %>% 
  filter(!is.na(language)) %>% 
  mutate(first_lan = str_extract(language, "\\w*"),
         first_lan_f = fct_lump(first_lan, n = 9, other_level = "Otro" )) %>% 
  group_by(year, lan = first_lan_f) %>% 
  summarise(n=n())


# Visualización -----------------------------------------------------------

p <- ggplot(data, aes(x=year, y= n, fill= lan))+
  geom_area()+
  scale_x_continuous(breaks = seq(1910, 2020, by = 10), 
                     limits=c(1911, 2019))+
  scale_fill_manual(values = c("#b4e070",
                               "#fba849",
                               "#fffba4",
                               "#ff5090",
                               "#77b8fb",
                               "#88fff0",
                               "#f1c1d7",
                               "#00c2c3",
                               "#ffd19c",
                               "#b8a0e7"
  ))+
  theme(legend.position="right",
        plot.background = element_rect(fill= "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size=30, hjust = 0.5, vjust = 0.5, colour="#01c0d6"),
        legend.title.align = 0.5,
        panel.grid.major.y = element_line(linetype = "dashed", colour="grey33"),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 1),
        plot.caption.position = "plot"
  )+
  labs(fill= "IDIOMA\nPRINCIPAL", x="", y="Número de películas",
       title = "LENGUAJES DE PELÍCULAS",
       caption= "@sporella")



p <- p + ggforce::geom_mark_rect(
  aes(filter = year < 1929, label = "Cine mudo"),
  fill = "transparent",
  expand = unit(3, "mm"),
  radius = unit(3, "mm")
)


back <- ("img/cine.png")
pf <- ggbackground(p, back)  

ggsave("plots/nueve/peliculas.png", width = 7, height = 5)
