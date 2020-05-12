# #30díasdegráficos día 1
# Visualización de métricas de spotify
# https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/
# Autora: Stephanie Orellana (@sporella)

# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(Rspotify)
library(extrafont)

loadfonts()


# Acceso a API Spotify ----------------------------------------------------

my_oauth <- spotifyOAuth(app_id="sporella_ds",
                         client_id="e5674ff41ebe4830bc05f255b54aea9a",
                         client_secret="27e712ff7f0a4610800dc1bd36effae8")
save(my_oauth, file="my_oauth")
load("my_oauth")


# Datos Human.:||:Nature. -------------------------------------------------


hn <- getAlbum("1iSsdlURK7CGUVlcz4M5Li", token = my_oauth) %>% 
  select(-available_markets) %>% 
  unnest(cols = c(id, name, duration_ms, track_number, disc_number, preview_url)) %>% 
  left_join(map_df(.x = .$id ,.f = ~getFeatures(track_id = .x, token = my_oauth)), by = "id") %>% 
  pivot_longer(cols = c(danceability:tempo, time_signature), names_to = "feature") %>% 
  mutate( disc_number = replace(disc_number, str_detect(name, pattern = "All the Works of Nature+"), 2),
          disc_name = if_else(disc_number ==1, "HVMAN", "NATVRE"),
          name = str_replace(name, pattern = "All the Works of Nature Which Adorn the World - ", replacement = ""))


# Gráficos ----------------------------------------------------------------


# * Tema Nightwish --------------------------------------------------------


nightwish_theme <- function(x){
  theme(plot.background = element_rect(fill = "#242323"),
        panel.background = element_rect(fill="#242323"),
        strip.background.y = element_rect(fill = "#faa134"),
        strip.background.x = element_rect(fill = "#03a1a3"),
        panel.spacing.y = unit(0.5, "cm"),
        axis.text = element_text(family = "Calibri" ,size=14, colour = "#faa134"),
        strip.text.y = element_text(family = "Bell MT", size = 20),
        strip.text.x = element_text(family = "Calibri", size = 20),
        plot.title = element_text(family = "Kunstler Script", face = "bold", size = 100, colour = "white"),
        plot.subtitle = element_text(family = "Calibri" ,size=20, colour = "#faa134"),
        plot.caption = element_text(family = "Calibri", colour="white", size = 12),
        plot.tag = element_text(family = "Calibri", colour="#faa134", size = 12),
        plot.tag.position = "bottomleft" )}



logo <- magick::image_read("img/hn.png")


# * Gráficos --------------------------------------------------------------

# Energy

cap <- "Spotify API: 
Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.
For example, death metal has high energy, while a Bach prelude scores low on the scale."

p <- ggplot(hn %>% filter(feature=="energy"), aes(x=reorder(name, -track_number), y = value, fill=value)) + 
  geom_col(colour="white")+
  coord_flip()+
  scale_fill_viridis_c(option="magma", alpha = 0.9)+
  scale_y_continuous(limits = c(0,1))+
  facet_grid(disc_name~"ENERGY", scales = "free") +
  nightwish_theme()+
  theme(legend.position = "none") +
  labs(title = "Nightwish", 
       subtitle = "HVMAN.:||:NATVRE. Spotify Analysis",
       x = "", y = "", caption = cap, tag = "@sporella")



png('plots/uno/energy.png', height = 715, width = 1372, units = "px", res=100)
p
grid::grid.raster(image = logo, x = 0.07, y = 0.8,  just = c('left', 'top'), width = unit(1, 'inches'))
dev.off()

# Instrumentalness

cap <- "Spotify API: 
Predicts whether a track contains no vocals. “Ooh” and “aah” sounds are treated as instrumental in this context.
Rap or spoken word tracks are clearly “vocal”. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content."

p2 <- ggplot(hn %>% filter(feature=="instrumentalness"), aes(x=reorder(name, -track_number), y = value, fill=value)) + 
  geom_col(colour="white")+
  coord_flip()+
  scale_fill_viridis_c(option="magma", alpha = 0.9)+
  scale_y_continuous(limits = c(0,1))+
  facet_grid(disc_name~ "INSTRUMENTALNESS", scales = "free") +
  nightwish_theme()+
  theme(legend.position = "none") +
  labs(title = "Nightwish", 
       subtitle = "HVMAN.:||:NATVRE. Spotify Analysis",
       x = "", y = "", caption = cap, tag = "@sporella")


png('plots/uno/instrumentalness.png', height = 715, width = 1372, units = "px", res=100)
p2
grid::grid.raster(logo, x = 0.07, y = 0.8,  just = c('left', 'top'), width = unit(1, 'inches'))
dev.off()

# Valence

cap <- "Spotify API: 
A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. 
Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)"

p3 <- ggplot(hn %>% filter(feature=="valence"), aes(x=reorder(name, -track_number), y = value, fill=value)) + 
  geom_col(colour="white")+
  coord_flip()+
  scale_fill_viridis_c(option="magma", alpha = 0.9)+
  scale_y_continuous(limits = c(0,1))+
  facet_grid(disc_name~ "VALENCE", scales = "free") +
  nightwish_theme()+
  theme(legend.position = "none") +
  labs(title = "Nightwish", 
       subtitle = "HVMAN.:||:NATVRE. Spotify Analysis",
       x = "", y = "", caption = cap, tag = "@sporella")

png('plots/uno/valence.png', height = 715, width = 1372, units = "px", res=100)
p3
grid::grid.raster(logo, x = 0.07, y = 0.8,  just = c('left', 'top'), width = unit(1, 'inches'))
dev.off()