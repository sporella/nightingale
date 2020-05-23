# #30díasdegráficos día 12
# Visualización rendimientos y salarios FIFA20
# https://www.kaggle.com/sagunsh/fifa-20-complete-player-dataset
# Autora: Stephanie Orellana (@sporella)



# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggimage)


# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/fifa20_data.csv") %>% 
  clean_names()


# * Función para rescatar imágenes de web nueva ---------------------------

fix_pic <- Vectorize(function(x){
  m <- str_match(x, "(\\d{0,3})(\\d{3,4}).png")
  paste0("https://cdn.sofifa.com/players/", str_pad(m[2], width = 3, pad="0"), "/", m[3], "/20_60.png")
})


datos <- data %>%
  mutate(wage2 = as.numeric(str_extract(wage, pattern = "[:digit:]+"))) %>%
  top_n(15, wt = wage2) %>% 
  mutate(foto_fix = fix_pic(image))


# Visualización -----------------------------------------------------------

colores <- c("#b4c7ff",
             "#f1eb9d",
             "#d59abc",
             "#b2ffec",
             "#6fb8b0")

p <- ggplot(datos, aes(x = name)) +
  geom_segment( aes(xend = name, y = 0, yend = wage2), colour = "grey80", size = 1) +
  geom_segment( aes(xend = name, y = 0, yend = overall, colour = club), size = 1) +
  geom_point(aes(y = wage2), colour = "grey80", size = 8, show.legend = F) +
  geom_point(aes(y = overall, color = club, fill = club), size = 8, show.legend = F) +
  geom_text(aes(y = overall, label = overall )) +
  geom_image(aes(image = foto_fix, y = -10, size = 0.1, by = "height")) +
  scale_colour_manual(values = colores)+
  scale_y_continuous(
    sec.axis = dup_axis(name = "Salario(€K)"),
    expand = expansion(mult = c(.1, .1))
  )+
  scale_size_identity() +
  labs(title ="Salario v/s Rendimiento",
       subtitle = "Los 15 mejor pagados del juego FIFA20",
       caption = "@sporella", y = "Rendimiento General",
       x = "", colour = "Club") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom",
        axis.title.y.left = element_text(hjust = 0 ),
        axis.title.y.right = element_text(hjust = 0),
        plot.title = element_text(size = 30, face ="bold"),
        plot.subtitle = element_text(size = 20)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


ggsave("plots/doce/fifa20.png",p, width = 8, height = 8)
