# #30díasdegráficos día 15
# Visualización dendrograma pokemones favoritos
# https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggdendro)
library(ggimage)


# Cargar y procesar datos -------------------------------------------------

pokemon <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

favoritos <-
  c("Bulbasaur",
    "Charmander",
    "Squirtle",
    "Pikachu",
    "Eevee",
    "Jigglypuff",
    "Abra",
    "Gastly",
    "Mew",
    "Electabuzz",
    "Kangaskhan",
    "Onix",
    "Cubone",
    "Psyduck",
    "Meowth"
    
  )

pokemon_matrix <- pokemon %>% 
  filter(nombre_ingles %in%favoritos) %>% 
  select(nombre_traducido, puntos_vida:velocidad) %>% 
  column_to_rownames(var = "nombre_traducido")


# Modelo de dendrograma ---------------------------------------------------

model <- hclust(dist(pokemon_matrix), "ave")
tree <- as.dendrogram(model)

cut <- data.frame(m=cutree(model, k = 7)) %>% 
  rownames_to_column("label") %>% 
  mutate(m = factor(m))

tree_labels<- dendro_data(tree, type = "rectangle")
tree_labels$labels <- tree_labels$labels %>% 
  left_join(cut)

color_segment <- tree_labels$segments %>%
  left_join(tree_labels$labels, by = "x") %>% 
  mutate(line = ifelse(is.na(m), "1","2"))



# Visualización -----------------------------------------------------------

p <-  ggplot() +
  geom_segment(
    data = color_segment,
    aes(
      x = x,
      y = y.x,
      xend = xend,
      yend = yend,
      color = m,
      linetype = line
    ),
    size = 1,
    show.legend = F
  ) +
  geom_text(
    data = label(tree_labels),
    aes(
      x = x,
      y = y,
      label = label,
      colour = m
    ),
    fontface = "bold",
    size = 3,
    angle = 90,
    hjust = 1.2,
    show.legend = F
  ) +
  geom_pokemon(data = tree_labels$labels,
               aes(
                 x = x,
                 y = y - 45,
                 image = tolower(label)
               ),
               size = 0.09) +
  labs(caption = "@sporella") +
  theme_dendro() +
  theme(plot.background = element_rect(fill = "#666366"),
        plot.caption = element_text(colour = "grey99"))

p <- p + scale_colour_manual(values = c("#c7ffeb",
                                        "#ec9dd1",
                                        "#ccda8e",
                                        "#81b4fa",
                                        "#ffc8a0",
                                        "#00c1d0",
                                        "#e39892"), na.value = "grey99")

ggsave("plots/quince/poke_dendro.png", p, width = 6, height = 5.5)  
