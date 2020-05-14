# #30díasdegráficos día 2
# Visualización datos INE CHile Brechas de género en personas tituladas
# en carreras de tecnología
# http://geoine-ine-chile.opendata.arcgis.com/datasets/49ab475e009a4cfbaf1a354af6016d73_29?selectedAttribute=BRECHA_2008_PROFESIONAL
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(highcharter)


# Procesar datos con tidyverse --------------------------------------------

data <- read_csv("data/genero_ine.csv")

data_pivot <- data %>% 
  pivot_longer(cols = starts_with("BRECHA"), 
               names_to = c("year", "tipo"), 
               names_pattern = "BRECHA_(.*)_(.*)")


# Visualización con highcharter -------------------------------------------

hchart(data_pivot %>%  filter(tipo =="PROFESIONAL"), "line",
       hcaes(x = year, y = value, group = NOM_REGION)) %>% 
       hc_title(text="Brecha de género entre personas tituladas en carreras del área de tecnología") %>% 
  hc_subtitle(text="Carreras profesionales") %>% 
  hc_credits(enabled = TRUE, text = "@sporella", href = "https://sporella.netlify.app/") %>% 
  hc_yAxis(title= list(text = "Brecha")) %>% 
  hc_xAxis(title= list(text = "")) %>% 
  hc_add_theme(hc_theme_darkunica(credits = list(
    style = list(
      size = 3,
      color = "white"
    ))))

hchart(data_pivot %>%  filter(tipo =="TECNICO"), "line",
       hcaes(x = year, y = value, group = NOM_REGION)) %>% 
  hc_title(text="Brecha de género entre personas tituladas en carreras del área de tecnología") %>% 
  hc_subtitle(text="Carreras técnicas") %>% 
  hc_credits(enabled = TRUE, text = "@sporella", href = "https://sporella.netlify.app/") %>% 
  hc_yAxis(title= list(text = "Brecha")) %>% 
  hc_xAxis(title= list(text = "")) %>% 
  hc_add_theme(hc_theme_darkunica(credits = list(
    style = list(
      size = 3,
      color = "white"
    ))))
