# #30díasdegráficos día 6
# Visualización de huella ecológica en sudamérica 
# https://www.kaggle.com/footprintnetwork/ecological-footprint?select=countries.csv
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(janitor)
library(patchwork)
library(animation)


# Cargar y procesar datos -------------------------------------------------

data <- read_csv("data/countries.csv") %>% 
  clean_names()

paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
            "Paraguay", "Peru", "Uruguay", "Venezuela, Bolivarian Republic of")

s <- data %>% 
  select(1:3, cropland_footprint:fish_footprint) %>% 
  filter(country %in% paises) %>% 
  pivot_longer(cols = contains("footprint"),
               names_to = "footprint", names_pattern = "(.*)_footprint") %>% 
  filter(!is.na(value)) %>% 
  group_by(country) %>% 
  mutate(total = sum(value), prop = value/total) %>% 
  ungroup() %>% 
  mutate(pais_lab = str_replace_all(country, "Venezuela, Bolivarian Republic of" ,"Venezuela"),
         footprint = factor(footprint, 
                            levels = unique(footprint),
                            labels = c("Cultivos", "Pastoreo", "Bosques", "Carbono", "Pesca")))


# Visualización -----------------------------------------------------------


saveGIF({
  for(pais in unique(s$pais_lab)){
    s2 <- s %>%  filter(pais_lab == pais)
    
    p1 <- ggplot(s2, aes(x=1, y = value, fill=footprint)) +
      geom_col(fill = "grey40", show.legend = F)+
      scale_y_continuous(limits = c(0, 5))+
      labs(y = "Hectáreas Globales", fill= "")+
      theme_classic()+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
    p2 <- ggplot(s2, aes(x = 1, y = prop, fill = footprint))+
      geom_col(colour = "white") +
      geom_text(aes(label = paste0(round(prop, 2)*100, "%")), show.legend = F, 
                position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      xlim(c(-0.5, NA)) +
      labs(fill = "")+
      theme_classic() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())+
      scale_fill_manual(values = c("#cd546b",
                                   "#7aa444",
                                   "#4cab98",
                                   "#c27f3c",
                                   "#996ec3"))
    
    
    p3 <- p1 + p2 + plot_layout(guides = 'collect', 
                                widths = c(1, 2)) +
      plot_annotation(title="HUELLA ECOLÓGICA SUDAMÉRICA",
                      subtitle = pais, caption = "Datos: Global Footprint Network\n@sporella") & 
      theme(legend.position = 'bottom',
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 25, colour="#0cbfcc"))
    
    print(p3)
  }
}, interval = 2, movie.name="huella.gif", ani.res =120, ani.height = 600, ani.width = 800)
