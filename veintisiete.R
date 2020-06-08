# #30díasdegráficos día 27
# Visualización animación temperaturas enero en Santiago
# https://lpdaac.usgs.gov/products/mod11a1v006/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(raster)
library(tidyverse)
library(sf)
library(gganimate)


# Cargar raster y convertir unidades --------------------------------------

lst <- raster("data/lst_01.tif")
lst <- (lst * 0.02) - 273.15

# Leer comunas ------------------------------------------------------------

comunas <- st_read("data/comunas_rm.shp")

# Hacer estadística zonal -------------------------------------------------

mean_ene <- comunas %>%
  dplyr::select(Comuna, Region, Provincia) %>%
  mutate(ene = raster::extract(lst$lst_01, comunas["Comuna"], fun = mean, na.rm =
                                 T)) %>% 
  dplyr::arrange(desc(ene))


# Hacer gráfico -----------------------------------------------------------


p <- ggplot() +
  geom_sf(data = mean_ene, aes(fill = ene), colour = "grey33", show.legend = F) +
  scale_fill_viridis_c(option = "C",
                       breaks = seq(30, 42, 1),
                       guide = "legend") +
  labs(caption = "@sporella", subtitle = "Temperatura superficial mes de enero, sensor MODIS (MOD11A1)")+
  theme_minimal() +
  theme(plot.title = element_text(size=15, face="bold"))+
  coord_sf()+
  ggtitle(paste("{current_frame}:", "{round(mean_ene$ene[mean_ene$Comuna == current_frame],2)}", "[°C]")) +
  transition_manual(factor(Comuna, levels = unique(mean_ene$Comuna)), cumulative = T)+
  view_follow()

p2 <- animate(p, fps = 2, start_pause = 1)

anim_save(filename = "plots/veintisiete/temp_ene.gif", animation = p2)
