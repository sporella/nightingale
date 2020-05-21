# #30díasdegráficos día 10
# Visualización Guernica en paletas de colores
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(png)
library(animation)

# Cargar y procesar datos -------------------------------------------------
x <- readPNG("data/cuadro-guernica-de-picasso.png")
img_mat=x[,,1]
img_mat=t(apply(img_mat, 2, rev))


s <- "Guernica, Pablo Picasso.\n@sporella"
saveGIF({
  image(img_mat, col  = rainbow(255), main = "rainbow", sub = s)
  image(img_mat, col  = heat.colors(255), main = "heat.colors", sub = s)
  image(img_mat, col  = terrain.colors(255), main = "terrain.colors", sub = s)
  image(img_mat, col  = topo.colors(255), main = "topo.colors", sub = s)
  image(img_mat, col  = cm.colors(255), main = "cm.colors", sub = s)
  image(img_mat, col  = hcl.colors(255), main = "hlc.colors", sub = s)
  image(img_mat, col  = scales::hue_pal()(255), main = "scales::hue_pal()", sub = s)
  image(img_mat, col  = scales::viridis_pal(option = "A")(255), main = "scales::viridis_pal(option='A')", sub = s)
  image(img_mat, col  = scales::viridis_pal(option = "B")(255), main = "scales::viridis_pal(option='B')", sub = s)
  image(img_mat, col  = scales::viridis_pal(option = "C")(255), main = "scales::viridis_pal(option='C')", sub = s)
  image(img_mat, col  = scales::viridis_pal(option = "D")(255), main = "scales::viridis_pal(option='D')", sub = s)
  image(img_mat, col  = scales::viridis_pal(option = "E")(255), main = "scales::viridis_pal(option='E')", sub = s)
}, movie.name = "guernica.gif", ani.height = 400, ani.width=540)
