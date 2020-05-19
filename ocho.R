# #30díasdegráficos día 8
# Visualización mapa de vientos estación Carriel Sur, Conpeción
# https://agrometeorologia.cl/VV
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(MASS)
library(raster)


# Cargar y procesar datos -------------------------------------------------

data_mes <- read_csv("data/carrielsur_viento_dia.csv", skip = 5) %>% 
  clean_names() %>% 
  mutate(fecha= dmy(tiempo_utc_4)) %>% 
  filter(!is.na(fecha), month(fecha)==04) %>% 
  rename(vel = velocidad_de_viento_km_h,
         dir = direccion_de_viento_º)

# * Estadísticas densidad de puntos ---------------------------------------


density <-kde2d(data_mes$dir,data_mes$vel, 
                lims = c(0, 360, 0, ceiling(max(data_mes$vel))))

# * Convierto a raster y dataframe para graficar --------------------------

r <- data.frame(rasterToPoints(raster(density)))


# Visualización -----------------------------------------------------------


# * Crear tema ------------------------------------------------------------

theme_wind <- function(x){
  theme(panel.background = element_rect(fill="white"),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust=0.8),
        axis.ticks.x = element_line(colour = "grey20", size = rel(0.5)),
        axis.text.x = element_text(face = "bold", size=10),
        panel.grid.minor.y = element_line(size = 20),
        panel.grid = element_line(colour = "grey42"), 
        panel.grid.major = element_line(size = rel(0.5)), 
        panel.grid.minor = element_line(size = rel(0.25)),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}


# * Hacer gráfico ---------------------------------------------------------

p <- ggplot() +
  geom_contour_filled(data=r, aes(x = x ,y = y, z=layer, fill=..piece..), colour="grey33")+
  geom_point(data=data_mes, aes(y = vel , x = dir), size=1, shape=0)+
  scale_fill_gradientn(colours = c('grey90', '#4b8aff', '#52b640', '#ffff00', '#ff3700', '#d5002b', '#800080'),
                       breaks = c(1,13), labels = c("-", "+")
  )+
  coord_polar()+
  scale_x_continuous(breaks = seq(0.01, 360, length.out = 9), 
                     labels = c("", "NE", "E", "SE", "S", "SO", "O", "NO", "N"))+
  labs(y = "Velocidad (km/h)", fill = "Predominante", 
       title = "Mapa de Vientos",
       subtitle = "Estación Carriel Sur, Concepción\nAbril 2020",
       caption = "@sporella\nDatos: agrometeorologia.cl")+
  theme_wind()+
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                label.theme = element_text(size = 16),
                                barwidth = unit(4, "cm")))


ggsave("plots/ocho/mapadevientos.png",p , width = 6, height = 6)
