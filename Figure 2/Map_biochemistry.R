
library(maptiles)
#install.packages("maptiles")
library(tidyterra)
#install.packages(c("sf","terra","tidyterra","maptiles"))
library(sf)
library(ggplot2)
library(openxlsx)
library(maptiles)
library(ggspatial)

#Rversion 4.5.2.

#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"

#работающие варианты карт
#"Esri.WorldStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldTerrain", "Esri.WorldShadedRelief", "Esri.OceanBasemap", "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas",
#"CartoDB.Positron", "CartoDB.PositronNoLabels", "CartoDB.PositronOnlyLabels", "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels", "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager", "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels",


#Angara
bb <- c(left = 104.2, bottom = 51.8, right = 105.0, top = 52.3)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 12, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.PositronNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm")) 

source_map

ggsave(dpi = 300, filename = "Biochemistry 3.svg")


#Baikal
bb <- c(left = 103.6, bottom = 51.4, right = 110.6, top = 55.9)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 9, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.PositronNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'CartoDB.VoyagerNoLabels')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 
source_map


ggsave(dpi = 300, filename = "Baika2 3.svg")