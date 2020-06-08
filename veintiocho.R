# #30díasdegráficos día 27
# Visualización animación temperaturas enero en Santiago
# https://lpdaac.usgs.gov/products/mod11a1v006/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(chorddiag) #devtools::install_github("mattflor/chorddiag")


# Cargar y procesar datos -------------------------------------------------

datos <- read_csv2("data/matriz_viajes.csv") %>% 
  filter(MediaHora == 27000) %>% 
  select(1,2,4) %>% 
  mutate_if(is.character, str_to_title)


datos_wider <- datos %>% 
  spread(value = ViajeLaboralPromedio , key= ComunaBajada, drop = F, fill = 0) %>% 
  column_to_rownames("ComunaSubida") %>% 
  as.matrix()


# Visualización -----------------------------------------------------------

name <- "Origen y Destino de viajes en Santiago. 7:30 AM"
chorddiag(
  datos_wider,
  groupnameFontsize = 8,
  ticklabelFontsize = 6,
  tickInterval = 500,
  chordedgeColor = NA,
  categoryNames = c("@sporella", name),categorynameFontsize = 10
)
