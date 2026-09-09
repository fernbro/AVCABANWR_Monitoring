library(tidyverse)

prism_raw <- read_csv("data/PRISM_ppt_tmean_09082026.csv")

# wrangle this:

prism <- prism_raw %>% 
  mutate(date = as.POSIXct(Date, format = "%m/%d/%y")) %>% 
  dplyr::select(-Date) %>% 
  mutate(ppt_in = ppt_mm*0.0393701, 
         tmean_f = (tmean_c * (9/5)) + 32)

ggplot(filter(prism, year(date) %in% c(2025, 2026)), aes(x = date, y = ppt_in))+
  geom_line()

# summarise june-august precip for each year?

prism_mons <- filter(prism, year(date) %in% c(2025, 2026),
                     month(date) %in% c(6, 7, 8, 9)) %>% 
  group_by(year(date)) %>% 
  summarise(ppt_tot_mm = sum(ppt_mm),
            ppt_tot_in = sum(ppt_in),
            tmean_c_avg = mean(tmean_c),
            tmean_f_avg = mean(tmean_f))






