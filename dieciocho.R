# #30díasdegráficos día 18
# Visualización datos temperatura superficial MODIS
# https://lpdaac.usgs.gov/products/mod11a1v006/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(gganimate)


raster_tab <- read_csv("data/modis_lst.csv") %>% 
  pivot_longer(starts_with("lst"), names_to = "mes", values_to = "value", names_prefix = "lst_") %>% 
  mutate( mes = factor(mes, labels = str_to_title(locale("es")$date_names$mon)))


# Visualización -----------------------------------------------------------

p <- ggplot(raster_tab, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  labs(
    title = "Temperatura Superficial Diurna (MOD11A1)",
    subtitle = "{closest_state}",
    caption = "@sporella",
    fill = "Temperatura [°C]",
    x = "",
    y = ""
  ) +
  # facet_wrap(~mes)+
  theme_minimal() +
  scale_fill_gradientn(colours = scales::brewer_pal(palette = "Spectral", direction = -1)(11)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top"))


options(gganimate.dev_args = list(width = 430, height = 430, res = 90)) 

p +
  transition_states(mes)

anim_save(filename = "plots/dieciocho/temp.gif")
