library(tidyverse)
library(sf)
library(hms)
library(emmeans)

phys1 <- read_csv("data/Physiology_10072025.csv") %>%
  mutate(time = as.character(time)) %>% 
  mutate(time = case_when(spp == "ERALEH" & id == 5 ~ "09:00:00",
                          spp == "ERALEH" & id == 8 ~ "10:15:00",
                          spp == "ERALEH" & id == 10 ~ "10:50:00",
                          .default = time)) %>% # gap-fill missing times with best guesses
  mutate(time = as_hms(time))

#   mutate(time_since_start = seconds(max(time)) - seconds(time))

time_mod <- lm(cond ~ time, phys1)
phys1$res_con <- residuals(time_mod)

spp_mod <- lm(res_con ~ spp*site, phys1)

spp_emm <- emmeans(spp_mod, ~spp:site)
summary(spp_mod)

pairs(spp_emm)


# in the treatment, conductance was an average of 57.35 mmol/m2s lower than in the control area
# likely based on ToD measured
# i'm gonna subtract this amount from the controls and see what happens...

phys2 <- phys1 %>% 
  mutate(cond_cor = case_when(site == "control" ~ cond - 57.35,
                              .default = cond))

phys_stats <- phys1 %>% 

ggplot(phys2, aes(x = spp, y = cond_cor))+
  geom_boxplot(aes(group = interaction(spp, site), fill = site))
ggplot(phys2, aes(x = spp, y = cond_cor))+
  geom_point(aes(group = interaction(spp, site), color = site))
