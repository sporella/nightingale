# #30díasdegráficos día 13
# Visualización canto chucao
# https://www.xeno-canto.org/species/Scelorchilus-rubecula
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(bioacoustics)
library(tidyverse)
library(seewave)

# Cargar y procesar datos -------------------------------------------------


chucao <- read_audio("data/chucao.mp3")
chucao

data <- data.frame(val = chucao@left,
             x = seq(0, duration(chucao), length.out = length(chucao)))



# Visualización -----------------------------------------------------------

sub <- "Registro en Parque Nacional Chiloé, Chiloé, X Región, Chile. Por Étienne Leroy, 22-10-2019"
img <- "img/chucao.png"


p <- ggplot(data, aes(x=x, y=val))+
  geom_line(colour="#02ab91")+
  labs(x="Segundo", y="Valor", title = "Un chucao en el bosque",
       subtitle = str_wrap(sub, 50),
       caption="@sporella\nDatos: https://www.xeno-canto.org/")+
  scale_x_continuous(n.breaks = 10)+
  theme_classic()+
  theme(plot.title = element_text(size=30))

pf <- ggimage::ggbackground(p, img)

ggsave("plots/trece/chucao.png", pf, width = 6, height = 5)
