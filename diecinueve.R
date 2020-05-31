# #30díasdegráficos día 19
# Visualización datos búsquedas google Chile
# https://trends.google.es/trends/explore?geo=CL&q=noticias,virus,metro,mascarillas,toque%20de%20queda
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggTimeSeries)



# Cargar y procesar datos -------------------------------------------------


datos <- read_csv("data/google.csv", skip = 2) %>% 
  mutate_if(is.character, parse_number) %>% 
  pivot_longer(-1, names_to = c("termino", "pais"), values_to = "valor", names_pattern = "(.*): (.*)")


# Visualización -----------------------------------------------------------


p <- ggplot(datos, aes(x = Semana, y = valor, fill = termino)) +
  stat_steamgraph(alpha = .75) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", limits = c(as.Date("2019-05-01"), NA)) +
  scale_fill_manual(values = c("#f0bf4b",
                               "#8b66f1",
                               "#009377",
                               "#dc0049",
                               "#f4a6ff")) +
  guides(fill = guide_legend(title.position = "top")) +
  labs(fill= "Término de búsqueda", x="", y="",
       title = "Búsquedas Último Año en Chile",
       caption= "@sporella") +
  theme(legend.position="bottom",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill= "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size=20, hjust = 0.5, vjust = 0.5, colour="#01c0d6"),
        legend.title.align = 0.5,
        panel.grid.major.x = element_line(linetype = "dashed", colour="grey33"),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 1),
        plot.caption.position = "plot"
  )

ggsave("plots/diecinueve/google_chile.png", p, height = 4, width = 6.7)
