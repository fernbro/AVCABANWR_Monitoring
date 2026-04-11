library(tidyverse)
library(sf)
library(terra)
library(mapview)

tr <- st_read("data/Treatment_Plots_50.shp")
co <- st_read("data/Control_Plots_50.shp")
cult <- st_read("data/Puertocito_Cultural/Cultural_Avoidance_Areas_No RX Burn_No Ground Disturbance.shp")

cult <- st_transform(cult, crs = "epsg:26912")

co$type = "control"
tr$type = "treatment"

plots <- bind_rows(co, tr)

ggplot()+
  geom_sf(data = tr, color = "orange")+
  geom_sf(data = co, color = "blue")+
  geom_sf(data = cult, color = "blue", alpha = 0.4)+
  theme_light()

plots$PlotID[st_intersects(cult, plots)[[2]]]

mapView(plots, map.types = "Esri.WorldImagery", zcol = "type")

