# #30díasdegráficos día 20
# Visualización redes Harry Potter
# https://data.world/harishkgarg/harry-potter-universe
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------


library(igraph)
library(ggraph)
library(tidyverse)
library(extrafont)

# Instalar https://www.dafont.com/es/harry-p.font
# loadfonts()


# Cargar y procesar datos -------------------------------------------------
tres <- c("Harry Potter", "Hermione Granger", "Ron Weasley")
nodes <- read_csv("data/characters_hp.csv") 
links <- read_csv("data/relations_hp.csv") %>% 
  filter(source %in% nodes$id[nodes$name%in%tres]|
           target %in% nodes$id[nodes$name%in%tres]) %>% 
  mutate(type = factor(type, levels = c("-", "+"), labels = c("Negativa", "Positiva")))

seleccionar <- unique(c(links$target, links$source))
nodes <- nodes %>% 
  filter(id %in% seleccionar)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)


# Visualización -----------------------------------------------------------


p <- ggraph(net, 'drl') +
  geom_edge_link(aes(colour = factor(type))) +
  scale_edge_colour_manual(values = c("#20cee5", "#ffbdf0")) +
  geom_node_point(colour = "#8ff4cd", size = 3.5) +
  geom_node_label(aes(label = name), family = "Brush Script MT", colour = "grey10",
                  repel = T,
                  size = 4.5,
                  label.r = 0) +
  labs(title = "Harry Potter",
       subtitle = "Redes del trío dorado",
       caption = "@sporella",
       edge_colour = "Interacción") +
  theme(text = element_text(size=16, family = "Brush Script MT"),
        plot.title = element_text(size=80, family = "Harry P", colour = "gold", hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(size=40, family = "Brush Script MT", colour = "firebrick1", hjust = 0.5 ),
        plot.caption = element_text(colour = "grey90"),
        plot.background = element_rect(fill="grey30"),
        legend.key = element_rect(fill = NA, color = NA),    
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        panel.background = element_rect(fill = "grey30")
  ) +
  guides(edge_colour = guide_legend(
    title.position = "top",
    override.aes = list(edge_width = 3)
  ))

ggsave("plots/veinte/hp.png",p ,width = 10, height = 10)


