library(tidyverse)

weather <- read_csv("../MtLemmon_PPT.csv") %>% 
  transmute(year = Date, lemmon = ppt_upper, tucson = ppt_lower) %>% 
  pivot_longer(cols = lemmon:tucson, names_to = "location", values_to = "ppt") %>% 
  mutate(location = case_when(location == "lemmon" ~ "Mount Lemmon",
                              location == "tucson" ~ "Tucson"))

ggplot(weather, aes(x = year, y = ppt*0.00328084))+
  geom_line(aes(color = location), size = 2)+
  theme_light(base_size = 26)+
  labs(x = "Year", y = "Precipitation (feet)", color = "Location")





