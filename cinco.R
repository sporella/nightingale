# #30díasdegráficos día 5
# Visualización de redes Libro 1 Juego de Tronos 
# kaggle.com/moradnejad/interaction-networks-for-game-of-thrones-saga
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(igraph)
library(ggraph)
library(tidyverse)
library(extrafont)

# loadfonts()


# Cargar y procesar datos -------------------------------------------------

links <- read.csv("data/asoiaf-book1-edges.csv", header=T, as.is=T) %>% 
  filter(str_detect(Source, "Stark"), weight>20)
nodes <- read.csv("data/asoiaf-book1-nodes.csv", header=T, as.is=T) %>% 
  filter(Id %in% unique(c(links$Source, links$Target)))


net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)


# Visualización -----------------------------------------------------------

colores <- c("#bd9400", "#b40026", "#00d7ca", "#9f50d8", "#3d4f8d", "#39d65e")

p <- ggraph(net, layout = 'linear') +
  geom_edge_arc(
    aes(colour = factor(from), edge_alpha= weight),
    edge_width = 1.3
  ) +
  geom_edge_point(aes(size = weight, edge_colour = factor(from), edge_alpha=weight)) +
  geom_node_text(
    aes(label = Label),
    family = "Century Gothic",
    colour = "grey42",
    size = 3, 
    angle = 90,
    hjust = 1.2,
    vjust = 0.5
  ) +
  scale_y_continuous(limits = c(-3, NA))+
  scale_edge_alpha(range=c(0.4, 1)) +
  scale_edge_color_manual(values = colores )+
  labs(title = "REDES DE LOS STARK",
       subtitle = "Canción de hielo y fuego\nLIBRO 1",
       caption = "*Solo las más importantes\n@sporella")

pf <-  p +  theme(
  text = element_text(family = "Century Gothic", colour = "grey42"),
  plot.title = element_text(face = "bold", size = 40, hjust = 0.5),
  plot.subtitle = element_text(size = 15, hjust = 0.5),
  plot.tag.position = "bottomright",
  legend.position = "none",
  panel.background = element_rect(fill = "grey10"),
  plot.background = element_rect(fill = "grey10")
)

ggsave("plots/cinco/GOT.png", pf, width = 7, height = 6)
