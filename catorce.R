# #30díasdegráficos día 14
# Visualización Coberturas de Suelo MODIS Región Metropolitana
# https://lpdaac.usgs.gov/products/mcd12q1v006/
# Autora: Stephanie Orellana (@sporella)


# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(highcharter)
library(raster)

# Cargar y procesar datos -------------------------------------------------

land <- raster("data/land_cover_rm.tif")
landd <- read_csv("data/land_cover_rm_descripcion_T1.csv")

land_tab <- data.frame(rasterToPoints(land)) %>%
  group_by(categoria = land_cover_rm) %>%
  summarise(n = n()) %>%
  left_join(landd) %>%
  filter(categoria != 0) %>%
  mutate(name = etiqueta,
         value = n,
         sup_ha = n * (500 * 500) * 0.0001) # conversión a hectáreas


# Visualización -----------------------------------------------------------

# * Tooltip ---------------------------------------------------------------

x <- c("Cobertura","Descripción:", "Número:", "Superficie:")
y <- c("{point.name}",
       "{point.descripcion}",
       "{point.value}",
       "~{point.sup_ha} ha"
)


# * Treemap ---------------------------------------------------------------


highchart() %>%
  hc_title(text = "Coberturas de Suelo Región Metropolitana según Clasificación IGBP Sensor MODIS(MCD12Q1) Año 2018", style = list(fontSize = "30px")) %>%
  hc_chart(type = "treemap") %>%
  hc_xAxis(categories = land_tab$name) %>%
  hc_add_series(land_tab, showInLegend = FALSE) %>%
  hc_tooltip(useHTML = TRUE, pointFormat = tooltip_table(x, y)) %>% 
  hc_add_theme(hc_theme_smpl())
