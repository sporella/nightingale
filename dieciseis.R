# #30díasdegráficos día 16
# Visualización participación femenina parlamentos latinoamérica
# https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(waffle) # devtools::install_gitlab("hrbrmstr/waffle")


# Cargar y procesar datos -------------------------------------------------


paises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador",
            "Paraguay", "Perú", "Uruguay", "Venezuela")

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
             ncol = 3,
             dir = "v",
             strip.position = "bottom") +
  scale_fill_manual(values = c("#ae9dff", "#46ecce")) +
  labs(title = "Participación Femenina\nParlamentos de Latinoamérica",
       fill = "",
       caption = "@sporella") +
  theme(line = element_blank(), rect = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 8, margin = margin(0,5,0,5), vjust=1),
    legend.position = "bottom",
        plot.caption.position = "panel",
        plot.title = element_text(
          size = 15,
          vjust = 2,
          hjust = 0.5
        ))

ggsave("plots/dieciseis/parlamentos.png",p, width = 5, height = 5)
