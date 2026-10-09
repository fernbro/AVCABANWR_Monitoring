library(tidyverse)
library(vegan)
library(sf)
library(mapview)
library(MuMIn)
library(betareg)
library(indicspecies)


# spp categorical-type traits:

traits <- read_csv('data/species_traits.csv')
provel <- read_csv("data/mesquite_canopy.csv") %>% 
  mutate(MS_canopy = 1)

# community data:
pre <- read_csv("data/community_fall2025.csv")
pre$year <- "2025"
pre[is.na(pre)] <- 0

post <- read_csv("data/community_fall2026.csv")
post$year <- "2026"
post[is.na(post)] <- 0
post$plot <- as.character(post$plot)

#indicator species analysis (on matrices without year columns):

        # vector of treatment plots:

        treat <- as.character(seq(1, 100, by = 1))

pre_plots <- data.frame(pre$plot)
names(pre_plots) <- "plot"
pre_ms <- full_join(pre_plots, provel) %>% 
  mutate(MS_canopy = as.factor(case_when(MS_canopy == 1 ~ "canopy",
                               is.na(MS_canopy) ~ "open"))) %>% 
  filter(plot %in% treat) %>% 
  dplyr::select(-plot)

pre_ms_vec <- pre_ms$MS_canopy
pre_com <- pre %>% 
  filter(plot %in% treat) %>% 
  dplyr::select(-plot)

pre_indic <- multipatt(x = pre_com, cluster = pre_ms_vec)
summary(pre_indic)
assoc_pre <- data.frame(strassoc(pre_com, pre_ms_vec, func = "IndVal.g", nboot.ci = 1000)) # still interpreting this


post_plots <- data.frame(post$plot)
names(post_plots) <- "plot"
post_ms <- full_join(post_plots, provel) %>% 
  mutate(MS_canopy = as.factor(case_when(MS_canopy == 1 ~ "canopy",
                                         is.na(MS_canopy) ~ "open"))) %>% 
  filter(plot %in% treat, plot != 80) %>% 
  dplyr::select(-plot)

post_ms_vec <- post_ms$MS_canopy
post_com <- post %>% 
  filter(plot %in% treat, plot != 80) %>% 
  dplyr::select(-plot)

post_indic <- multipatt(x = post_com, cluster = post_ms_vec)
summary(post_indic)

# ordination stuff:


com_change <- bind_rows(pre, post)
com_change[is.na(com_change)] <- 0

com_change <- filter(com_change, plot %in% treat)


# need a long dataframe for diversity things:

com_shift <- relocate(com_change, year, .before = HOPOBT)

com_long <- com_shift %>% 
  pivot_longer(cols = `HOPOBT`:`ACANEO`, names_to = "spp", values_to = "cover")

com_div <- com_long %>% 
  group_by(plot, year) %>% 
  summarise(div_sh = diversity(cover, index = "shannon"),
            rich = specnumber(cover)) %>% 
  full_join(provel) %>% 
  filter(!is.na(year)) %>% 
  mutate(MS_canopy = case_when(is.na(MS_canopy) ~ F,
                               .default = T))

ggplot(com_div, aes(x = div_sh))+
  geom_density(aes(fill = year), alpha = 0.3)

ggplot(com_div, aes(x = year, y = div_sh))+
  geom_boxplot(aes(group = interaction(year, MS_canopy), 
                   fill = as.factor(MS_canopy)), alpha = 0.3)

ggplot(com_div, aes(x = rich))+
  geom_density(aes(fill = year), alpha = 0.3)

ggplot(com_div, aes(x = year, y = rich))+
  geom_boxplot(aes(fill = year), alpha = 0.3)+
  geom_point(aes(fill = year), position = "jitter", alpha = 0.3)+
  labs(x = "Year", y = "Species richness")+
  theme_minimal(base_size = 30)

ggplot(com_div, aes(x = year, y = rich))+
  geom_boxplot(aes(fill = MS_canopy), alpha = 0.3)+
  # geom_point(position = "jitter", alpha = 0.3)+
  labs(x = "Year", y = "Species richness")+
  theme_minimal(base_size = 30)

com_sums <- com_div %>% 
  group_by(year, MS_canopy) %>% 
  summarise(rich = mean(rich))

ggplot(com_div, aes(x = year, y = div_sh))+
  geom_boxplot(aes(fill = MS_canopy), alpha = 0.3)+
  # geom_point(position = "jitter", alpha = 0.3)+
  labs(x = "Year", y = "Shannon diversity")+
  theme_minimal(base_size = 30)

summary(aov(rich ~ year + Error(plot), com_div))

# plot-wise change in diversity?

com_yr_comp <- com_div %>% 
  pivot_wider(values_from = c(div_sh, rich),
              names_from = year)

com_yr_change <- com_yr_comp %>% 
  mutate(sh_change = div_sh_2026 - div_sh_2025,
         rich_change = rich_2026 - rich_2025,
         plot = as.numeric(plot))

ggplot(com_yr_change, aes(x = MS_canopy, y = sh_change))+
  geom_boxplot(aes(fill = MS_canopy))

t.test(filter(com_yr_change, MS_canopy == T)$sh_change,
       filter(com_yr_change, MS_canopy == F)$sh_change,
       var.equal = T)
# t-test shows that plots that used to be under a mesquite canopy
# showed higher increases in shannon's diversity

ggplot(com_yr_change, aes(x = MS_canopy, y = rich_change))+
  geom_boxplot(aes(fill = MS_canopy))

t.test(filter(com_yr_change, MS_canopy == T)$rich_change,
       filter(com_yr_change, MS_canopy == F)$rich_change,
       var.equal = T)
# t-test also shows plots that were under a canopy had higher increases in richness

# let's bring in this mesquite proximity data:

mesq <- read_csv("data/mesquite_proximity_fall2026.csv")

# first, convert degrees to radians:

# equation for HLI adopted from https://doi.org/10.1111/j.1654-1103.2002.tb02087.x
# McCune and Keon (2002). Equations for potential annual direct incident radiation and heat load.
# Journal of Vegetation Science, 13(4), 603-606.

# change exact parameters since my bearings are from the plot to the mesquite, and thus
# a bearing of 180 would indicate a mesquite to the south and lower heat load

# with my formulation, heat load peaks when the mesquite is located 45º from the plot
# and heat load is minimum when the mesquite is 225º from the plot (to the SW, providing shade)

mesq_conv <- mesq %>% 
  mutate(direction_rad = (direction*pi)/180) %>% 
  mutate(hli = 1-cos(direction_rad-(5*pi/4))/2) %>% 
  mutate(hli_scaled = scale(hli, center = T)[,1]) %>% 
  mutate(plot = as.double(plot))

com_yr_change <- inner_join(com_yr_change, mesq_conv)

cor.test(com_yr_change$rich_change, com_yr_change$distance)
# richness change was not correlated with distance from mesquite

cor.test(com_yr_change$rich_change, com_yr_change$hli_scaled)
# almost correlated with direction? how to scale..?

com_yr_change$ms_tf = case_when(com_yr_change$MS_canopy == F ~ 0,
                                com_yr_change$MS_canopy == T ~ 1)

summary(lm(rich_2025 ~ ms_tf, data = com_yr_change))
summary(lm(rich_2026 ~ ms_tf, data = com_yr_change))

# MS canopy explained 7% of richness in 2025
# .....but explained 26% of richness in 2026!

# distance didn't explain much of anything lol.
summary(lm(div_sh_2025 ~ ms_tf*distance, data = com_yr_change))
summary(lm(div_sh_2026 ~ ms_tf, data = com_yr_change))
# MS canopy didn't explain the diversity in 2025, but in 2026, it explained 23% of the diversity.

# i'm sensing a strong interaction of climate and mesquite presence.
# we have had a very wet year in 2026 thus far, which may have allowed the "fertile islands" to respond more strongly

summary(lm(div_sh_2025 ~ hli_scaled*distance, data = com_yr_change))
summary(lm(div_sh_2026 ~ hli_scaled*distance, data = com_yr_change))

summary(lm(rich_change ~ hli_scaled, data = com_yr_change))
summary(lm(sh_change ~ hli_scaled, data = com_yr_change)) #???

# species-specific responses?

com_long_change <- com_long %>% 
  pivot_wider(values_from = cover, names_from = year) %>% 
  group_by(plot, spp) %>% 
  summarise(change = `2026` - `2025`)
    
ggplot(filter(com_long_change, spp %in% c("ERALEH", "DIGCAL", "HOPOBT")), 
       aes(x = spp, y = change))+
  geom_boxplot()

com_long_change <- com_long_change %>% 
  mutate(plot = as.double(plot)) %>% 
  inner_join(mesq_conv) %>% 
  mutate(plot = as.character(plot)) %>% 
  full_join(provel) %>% 
  mutate(MS_canopy = case_when(is.na(MS_canopy) ~ 0,
                               .default = 1))

ggplot(filter(com_long_change, spp == "MACTAG"), aes(x = MS_canopy, y = change))+
  geom_point()+
  geom_smooth(method = "lm")

tr_2025 <- st_read("data/Treatment_Plots_50.shp") %>% 
  mutate(plot = as.character(PlotID)) %>% 
  inner_join(com_long) %>% 
  filter(spp == "ERALEH", year == 2025)
tr_2026 <- st_read("data/Treatment_Plots_50.shp") %>% 
  mutate(plot = as.character(PlotID)) %>% 
  inner_join(com_long) %>% 
  filter(spp == "ERALEH", year == 2026)

mapview(tr, zcol = "cover")

# lehmann's testing (for 2025):

erle_25 <- tr_2025 %>% 
  dplyr::select(plot, year, spp, cover) %>% 
  mutate(plot = as.double(plot)) %>% 
  inner_join(mesq) %>% 
  mutate(plot = as.character(plot)) %>% 
  full_join(provel) %>% 
  mutate(MS_canopy = case_when(is.na(MS_canopy)~0,
                               .default = MS_canopy)) %>% 
  mutate(erle_pres = case_when(cover == 0 ~ 0,
                               cover != 0 ~ 1)) %>% 
  filter(!is.na(cover)) %>% 
  mutate(erle_rel = cover/max(cover))

erle_26 <- tr_2026 %>% 
  dplyr::select(plot, year, spp, cover) %>% 
  mutate(plot = as.double(plot)) %>% 
  inner_join(mesq) %>% 
  mutate(plot = as.character(plot)) %>% 
  full_join(provel) %>% 
  mutate(MS_canopy = case_when(is.na(MS_canopy)~0,
                               .default = MS_canopy)) %>% 
  mutate(erle_pres = case_when(cover == 0 ~ 0,
                               cover != 0 ~ 1)) %>% 
  filter(!is.na(cover)) %>% 
  mutate(erle_rel = cover/max(cover))

# let's do a glm Lol
glm25 <- glm(erle_pres ~ MS_canopy, family = "binomial", erle_25)
plot(glm25) # significant..
r.squaredGLMM(glm25)


glm25_rel <- betareg(erle_rel ~ MS_canopy, link = "logit", erle_25)
plot(glm25_rel) # significant..


expit <- function(x){ # for converting log regression outputs (log odds) to % mortality
  exp(x)/(1+exp(x))
}

erle_25$pred <- expit(predict(glm25, type = "link"))
erle_25$pred_rel <- expit(predict(glm25_rel, type = "link"))*30

ggplot(erle_25, aes(x = MS_canopy, y = erle_pres))+
  geom_point(alpha = 0.1)

glm26 <- glm(erle_pres ~ MS_canopy, family = "binomial", erle_26)
summary(glm26) # maybe only a bit significant lol
erle_26$pred <- expit(predict(glm26))



