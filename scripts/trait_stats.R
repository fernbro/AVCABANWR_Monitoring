library(tidyverse)

traits <- read_csv("data/BANWR_leaf_traits_2026.csv") %>% 
  filter(sla_cm2_g < 1000)


ggplot(traits, aes(x = spp, y = sla_cm2_g))+
  geom_boxplot()

TukeyHSD(aov(sla_cm2_g ~ spp, traits))
TukeyHSD(aov(sla_cm2_g ~ position, traits))
TukeyHSD(aov(sla_cm2_g ~ position*spp, traits))


ggplot(traits, aes(x = spp, y = rwc))+
  geom_boxplot(aes(fill = position))

ggplot(traits, aes(x = spp, y = ldmc))+
  geom_boxplot(aes(fill = position))




# data collected in 2025:

traits <- read_csv("data/09282025_SLAandLWC.csv") %>% 
  mutate(spp = Species, sla = SLA_mm2_mg, lwc = LWC_mgmg) %>% 
  mutate(site = case_when(Individual < 11 ~ "Treatment",
                          Individual > 10 ~ "Control")) %>%
  mutate(sla = case_when(sla < 40 ~ sla,
                         sla >= 40 ~ NA),
         lwc = case_when(lwc < 0.97 ~ lwc,
                         lwc >= 0.97 ~ NA)) %>% 
  mutate(ldmc = 1-lwc)

TukeyHSD(aov(lm(sla ~ spp, traits)))
TukeyHSD(aov(lm(ldmc ~ spp, traits)))

ggplot(filter(traits, spp != "PROVEL"), aes(x = spp))+
  geom_boxplot(aes(y = sla*10))+ # dividing by 1000 converts mm2/mg --> m2/g
  geom_point(aes(y = sla*10), position = "jitter", size = 5, alpha = 0.3)+
  theme_minimal(base_size = 30)+
  labs(x = "Species", y = "Specific leaf area (cm2/g)")

ggplot(traits, aes(x = spp))+
  geom_boxplot(aes(y = lwc))+ # dividing by 1000 converts mm2/mg --> m2/g
  geom_point(aes(y = lwc), position = "jitter", size = 5, alpha = 0.3)+
  theme_minimal(base_size = 26)+
  labs(x = "Species", y = "Leaf water content (water weight:fresh weight)")

ggplot(traits, aes(x = spp))+
  geom_boxplot(aes(y = ldmc))+
  geom_point(aes(y = ldmc), position = "jitter", size = 5, alpha = 0.3)+
  theme_minimal(base_size = 26)+
  labs(x = "Species", y = "Leaf dry matter content (dry weight:fresh weight)")

TukeyHSD(aov(lm(lwc ~ spp, traits)))
summary(aov(lwc ~ spp, data = filter(traits, spp %in% c("ERALEH", "DIGCAL") & !is.na(lwc)))) # ok so means are different
with(filter(traits, !is.na(lwc)), leveneTest(lwc, as.factor(spp))) # the dispersion of Something is different!
with(filter(traits, spp %in% c("ERALEH", "DIGCAL") & !is.na(lwc)), leveneTest(lwc, as.factor(spp))) 
# the dispersion is not different btwn just ERALEH and DIGCAL

library(car)
