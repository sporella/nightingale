# #30díasdegráficos día 16
# Visualización participación femenina parlamentos latinoamérica
# https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(waffle) # devtools::install_gitlab("hrbrmstr/waffle")


# Cargar y procesar datos -------------------------------------------------


paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
            "Paraguay", "Peru", "Uruguay", "Venezuela")

datos <- read_csv(
    "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv"
  ) %>%
  filter(pais %in% paises) %>%
  select(pais, camara, porcentaje_mujeres) %>%
  mutate(porcentaje_mujeres = ceiling(porcentaje_mujeres),
         porcentaje_hombres = 100 - porcentaje_mujeres) %>%
  pivot_longer(3:4,
               names_to = "genero",
               values_to = "porcentaje",
               names_prefix = "porcentaje_", values_drop_na = T) %>% 
  arrange(camara) %>% 
  mutate(pais = factor(pais, levels = unique(pais)),
         camara = str_c("Cámara ", camara),
         genero = factor(genero, levels = c("mujeres", "hombres"),
                         labels = c("Mujeres", "Hombres")))


# Visualización -----------------------------------------------------------


p <- ggplot(datos, aes(fill = genero, values = porcentaje)) +
  geom_waffle(
    n_rows = 20,
    size = 0.33,
    colour = "white",
    flip = TRUE
  ) +
  facet_wrap(pais ~ camara,
             nrow = 4,
             dir = "v",
             strip.position = "bottom") +
  scale_fill_manual(values = c("#ae9dff", "#46ecce")) +
  labs(title = "Participación Femenina en Parlamentos de Latinoamérica",
       fill = "",
       caption = "@sporella") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(
          size = 15,
          vjust = 2,
          hjust = 0.5
        ))

ggsave("plots/dieciseis/parlamentos.png",p, width = 6, height = 5)
