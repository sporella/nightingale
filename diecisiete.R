# #30díasdegráficos día 17
# Visualización Nacionalidades FIFA20
# https://www.kaggle.com/sagunsh/fifa-20-complete-player-dataset
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(ggalluvial)


# Cargar y procesar datos -------------------------------------------------


clubes <- c("FC Barcelona", "Real Madrid")

datos <- read_csv("data/fifa20_data.csv") %>% 
  filter(Club %in% clubes) %>% 
  group_by(Club, Country) %>% 
  summarise(n = length(unique(Name)))


# Visualización -----------------------------------------------------------


cols <- c("#f9e068",
          "#e868dc",
          "#96df98",
          "#b582ed",
          "#ffc259",
          "#eb77ad",
          "#c2ac3b",
          "#7498ec",
          "#e88539",
          "#51afdf",
          "#ef7173",
          "#43ccd7",
          "#d99a74",
          "#46a5b1",
          "#c796d4",
          "#5cbda6",
          "#d19aa7",
          "#adab6f",
          "#9ba4cd",
          "#8fb297",
          "#6b97aa",
          "#7cbecf",
          "#d85359")


ggplot(data = datos,
       aes(axis2 = Club, axis1 = Country, y = n)) +
  scale_x_discrete(limits = c("Nacionalidad", "Club"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = Country), show.legend = FALSE) +
  geom_stratum(aes(fill = Country), colour = "grey33", show.legend = FALSE) + 
  geom_text(size = 2, stat = "stratum", infer.label = TRUE) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = toupper("Nacionalidad de jugadores"),
       subtitle = "Planillas según juego FIFA20",
       caption = "@sporella")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.caption.position = "plot",
    panel.background = element_rect(size = 0, fill=NA),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.title.position = "panel",
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("plots/diecisiete/nacionalidades.png",width = 6, height = 6)
