# #30díasdegráficos día 24
# Visualización temperaturas enero en Santiago
# https://lpdaac.usgs.gov/products/mod11a1v006/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(ggforce)


# Hacer raster stack y cáculos --------------------------------------------

lst <- raster("data/lst_01.tif")
lst <- (lst * 0.02) - 273.15

# Leer comunas ------------------------------------------------------------

comunas <- st_read("data/comunas_rm.shp")

# Hacer estadística zonal -------------------------------------------------

mean_ene <- comunas %>%
  dplyr::select(Comuna, Region, Provincia) %>%
  mutate(ene = raster::extract(lst$lst_01, comunas["Comuna"], fun = mean, na.rm =
                                 T))


# Hacer gráfico -----------------------------------------------------------

centroides <-
  data.frame(Comuna = mean_ene$Comuna,
             ene = mean_ene$ene,
             st_coordinates(st_centroid(mean_ene)))
desc <-
  paste0(
    "Es la comuna en donde se registra la mayor temperatura superficial promedio en enero, ",
    round(max(centroides$ene), 2),
    " [°C]"
  )

p <- ggplot() +
  geom_sf(data = mean_ene, aes(fill = ene), colour = "grey90") +
  scale_fill_viridis_c(option = "C",
                       breaks = seq(30, 42, 1),
                       guide = "legend") +
  geom_mark_circle(
    data = centroides ,
    aes(
      x = X,
      y = Y,
      filter = which.max(ene),
      label = Comuna,
      description = desc
    ),
    size = 1.5,
    colour = "#48c2b9",
    con.size = 1.2,
    con.border = "none",
    con.linetype = "dashed",
    con.colour = "#48c2b9",
    label.buffer = unit(50, "mm"),
    label.fill = "grey90",
    label.colour = "#068f85",
    con.cap = 0
  ) +
  scale_y_continuous(expand = expansion(add = c(0.6, 0.2))) +
  labs(
    x = "",
    y = "",
    title = "Temperatura Superficial Promedio Mes de Enero",
    subtitle = "Sensor MODIS (MOD11A1)",
    caption = "@sporella"
  ) +
  guides(
    fill = guide_legend(
      reverse = T,
      title = "Temperatura Superficial Enero [°C]",
      title.position = "right",
      title.theme = element_text(angle = 90)
    )
  ) +
  theme_minimal() +
  coord_sf()

ggsave("plots/veinticuatro/temperaturas_enero.png", p,  width = 7, height = 7)
