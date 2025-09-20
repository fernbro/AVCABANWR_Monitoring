library(tidyverse)
library(sf)
library(terra)
library(mapview)

tr <- st_read("data/Treatment_Plots_50.shp")
co <- st_read("data/Control_Plots_50.shp")

co$type = "control"
tr$type = "treatment"

plots <- bind_rows(co, tr)

ggplot()+
  geom_sf(data = tr, color = "orange")+
  geom_sf(data = co, color = "blue")+
  theme_light()

mapView(plots, map.types = "Esri.WorldImagery", zcol = "type")
