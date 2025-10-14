library(tidyverse)

traits <- read_csv("data/09282025_SLAandLWC.csv") %>% 
  mutate(spp = Species, sla = SLA_mm2_mg, lwc = LWC_mgmg) %>% 
  mutate(site = case_when(Individual < 11 ~ "Treatment",
                          Individual > 10 ~ "Control")) %>% 
  mutate(sla = case_when(sla < 40 ~ sla,
                         sla >= 40 ~ NA))

anova(lm(sla ~ spp, traits))

ggplot(traits, aes(x = spp))+
  geom_boxplot(aes(y = sla, group = interaction(spp, site),
                   color = site))

ggplot(traits, aes(x = spp))+
  geom_boxplot(aes(y = lwc, group = interaction(spp, site)))

anova(lm(lwc ~ spp + site, traits))
