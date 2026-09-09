library(tidyverse)
library(vegan)

pre <- read_csv("data/community_fall2025.csv")
pre$year <- "2025"
pre[is.na(pre)] <- 0

post <- read_csv("data/community_fall2026.csv")
post$year <- "2026"
post[is.na(post)] <- 0
post$plot <- as.character(post$plot)

com_change <- bind_rows(pre, post)
com_change[is.na(com_change)] <- 0

# vector of treatment plots:

treat <- as.character(seq(1, 100, by = 1))

com_change <- filter(com_change, plot %in% treat)

# spp categorical-type traits:

traits <- read_csv('data/species_traits.csv')
provel <- read_csv("data/mesquite_canopy.csv") %>% 
  mutate(MS_canopy = 1)

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
  geom_boxplot(aes(fill = year), alpha = 0.3)


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
  mutate(hli_scaled = scale(hli, center = T)[,1])

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
summary(lm(div_sh_2025 ~ ms_tf, data = com_yr_change))
summary(lm(div_sh_2026 ~ ms_tf, data = com_yr_change))
# MS canopy didn't explain the diversity in 2025, but in 2026, it explained 23% of the diversity.

# i'm sensing a strong interaction of climate and mesquite presence.
# we have had a very wet year in 2026 thus far, which may have allowed the "fertile islands" to respond more strongly

summary(lm(div_sh_2025 ~ hli_scaled, data = com_yr_change))
summary(lm(div_sh_2026 ~ hli_scaled, data = com_yr_change))

summary(lm(rich_change ~ hli_scaled, data = com_yr_change))
summary(lm(sh_change ~ hli_scaled, data = com_yr_change)) #???






