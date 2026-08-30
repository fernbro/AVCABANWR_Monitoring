library(tidyverse)

traits <- read_csv("data/09282025_SLAandLWC.csv") %>% 
  mutate(spp = Species, sla = SLA_mm2_mg, lwc = LWC_mgmg) %>% 
  mutate(site = case_when(Individual < 11 ~ "Treatment",
                          Individual > 10 ~ "Control")) %>% 
  mutate(sla = case_when(sla < 40 ~ sla,
                         sla >= 40 ~ NA))

TukeyHSD(aov(lm(sla ~ spp, traits)))

ggplot(filter(traits, spp != "PROVEL"), aes(x = spp))+
  geom_boxplot(aes(y = sla))+
  geom_point(aes(y = sla), position = "jitter", alpha = 0.3)+
  theme_minimal(base_size = 20)+
  labs(x = "Species", y = "SLA (g/m2)")

ggplot(traits, aes(x = spp))+
  geom_boxplot(aes(y = lwc, group = interaction(spp)))

TukeyHSD(aov(lm(lwc ~ spp, traits)))
