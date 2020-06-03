# #30díasdegráficos día 22
# Visualización paquetes espaciales
# https://cran.r-project.org/web/views/Spatial.html
# Autora: Stephanie Orellana (@sporella)


# Cargar librerías --------------------------------------------------------

library(tidyverse)
library(dlstats)
library(lubridate)
library(ggwordcloud)

# Descargar estadísticas --------------------------------------------------
# Se demora porque son muchos paquetes, más abajo dejo el dataset

# names <- c("ade4", "adehabitatHR", "adehabitatHS", "adehabitatLT", "adehabitatMA", "ads",
#            "akima", "AMOEBA", "areal", "ash",
#            "aspace", "automap", "CARBayes", "cartogram", "cartography", "classInt", "cleangeo", "CompRandFld", "constrainedKriging", "cshapes", "dbmss", "DCluster", "deldir", "DSpat", "ecespa", "ExceedanceTools", "fields", "FieldSim", "FRK", "gdalUtils", "gdistance", "geoaxe", "geogrid", "geojson", "geojsonio", "GEOmap", "geomapdata", "geometa", "geonames", "geonapi", "geoR", "georob", "geosapi", "geosphere", "geospt", "ggmap", "ggsn", "gmt", "GriegSmith", "gstat", "Guerry", "GWmodel", "gwrr", "igraph", "inlmisc", "intamap", "ipdw", "landsat", "landscapemetrics", "lawn", "leaflet", "leafletR", "lwgeom", "magclass", "mapdata", "mapedit", "mapproj", "maps", "maptools", "mapview", "marmap", "MBA", "McSpatial", "micromap", "ModelMap", "ncdf4", "ncf", "ngspatial", "nlme", "OpenStreetMap", "osmar", "ows4R", "pastecs", "PBSmapping", "PBSmodelling", "plotKML", "postGIStools", "PReMiuM", "ProbitSpatial", "qualmap", "quickmapr", "ramps", "RandomFields", "rangeMapper", "raster", "rasterVis", "RColorBrewer", "recmap", "rgbif", "rgdal", "rgeos", "RgoogleMaps", "rgrass7", "rnaturalearth", "RNetCDF", "rpostgis", "RPostgreSQL", "RPyGeo", "RSAGA", "RSurvey", "rtop", "rworldmap", "rworldxtra", "S2sls", "seg", "sf", "sgeostat", "shapefiles", "shp2graph", "siplab", "smacpod", "smerc", "sp", "spacetime", "spaMM", "spanel", "sparr", "spatgraphs", "spatial", "spatialCovariance", "SpatialEpi", "SpatialPosition", "spatialprobit", "spatialreg", "spatialsegregation", "SpatialTools", "spatstat", "spatsurv", "spBayes", "spBayesSurv", "spcosa", "spdep", "sperrorest", "spgrass6", "spgwr", "sphet", "spind", "splancs", "splm", "spm", "spmoran", "spsann", "spselect", "spsurvey", "spTimer", "SSN", "starma", "stars", "statebins", "Stem", "stplanr", "taRifx", "tgp", "tidycensus", "tigris", "tmap", "trip", "tripack", "tripEstimation", "UScensus2000cdp", "UScensus2000tract", "vardiag", "vec2dtransf", "vegan", "viridis", "Watersheds", "wkb")
# 
# 
# x <- do.call("rbind",(lapply(names, function (l)
#   cran_stats(l)
#   )))
# write.csv(x, "data/sppackages_stats.csv", row.names = F)


# Cargar y procesar datos -------------------------------------------------

datos <- read_csv("data/sppackages_stats.csv")

x_sum <- datos %>% 
  mutate(year=year(start)) %>% 
  filter(year >=2015) %>% 
  group_by(package) %>% 
  summarise(n = sum(downloads), number = n(), pon = n / number) %>% 
  ungroup() %>%   
  top_n(n = 50, wt = pon) %>% 
  mutate(size = pon / sum(pon)*100)



# Visualización -----------------------------------------------------------

desc <- "Paquetes que se encuentran listados en CRAN Task View: Analysis of Spatial Data.\nDescargas desde 2015."
set.seed(42)
p1 <- ggplot(x_sum, aes(label = package, size = size, colour=package)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_fivethirtyeight()+
  labs(caption = "@sporella", title = "Paquetes espaciales más descargados",
       subtitle = desc) +
  theme(plot.subtitle = element_text(size = 10))

ggsave("plots/veintidos/sppaquetes.png", p1, width = 6, height = 4, device = "png")

