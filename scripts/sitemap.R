library(tidyverse)
library(sf)
library(terra)
library(mapview)

# delta <- st_read("../pam review july 2026/shapefiles_delta_2026/DeltaReaches_2021_Albers.shp")
# delta_rest <- st_read("../pam review july 2026/shapefiles_delta_2026/2025_RestorationAreas.shp")

# plot(delta[2])

 ############
tr <- st_read("data/Treatment_Plots_50.shp")
co <- st_read("data/Control_Plots_50.shp")
cult <- st_read("data/Puertocito_Cultural/Cultural_Avoidance_Areas_No RX Burn_No Ground Disturbance.shp")

# spraying:

spray <- st_read("data/Spray_Area/Spray_Area_Total.shp")

mapview(spray)+mapview(tr)

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

all_plots_sf <- plots %>% 
  select(Plot, geometry, PlotID) %>% 
  mutate(Plot = case_when(is.na(Plot) ~ PlotID,
                          !is.na(Plot) ~ Plot)) %>% 
  select(-PlotID) %>% 
  st_as_sf()

st_write(all_plots_sf, "data/BANWR_Plots.shp")

# site polygons?

ctrl_site <- st_read("data/PotentialControlArea.shp")
trt_site <- st_read("data/puertocito_prj_area.shp")

plot(trt_site) # just the control
mapview(trt_site)+mapview(ctrl_site)+mapview(plots)

ggplot()+
  geom_sf(data = ctrl_site)+
  geom_sf(data = trt_site)+
  geom_sf(data = plots, aes(color = type))+
  theme_minimal()+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.5, "in"), width = unit(0.3, "in"),
    pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )




# let's look at native island points?

islands <- st_read("data/Native_Islands.kml")
islands_z <- st_zm(islands, drop = T)
islands_sf <- st_write(islands_z, "data/Native_Islands.shp")


mapview(islands_sf)

