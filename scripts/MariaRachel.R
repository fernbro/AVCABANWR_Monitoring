library(tidyverse)
library(vegan)

banwr <- read_csv("data/community_fall2025.csv") # read in community matrix
provel <- read_csv("data/mesquite_canopy.csv") %>% 
  mutate(MS_canopy = 1)
banwr <- full_join(banwr, provel)
banwr[is.na(banwr)] <- 0 # turn NA values to 0
plots <- read_csv("data/plots_by_site.csv") # read in plots by site
names(plots) <- c("plot", "site") # assign names to the plots file


# plot summaries for Dr. Gallery & Maria:

sums <- banwr %>% 
  full_join(plots) %>% 
  group_by(plot, site) %>% 
  summarise(llg = ERALEH,
            ms_canopy = MS_canopy)

total_cov <- banwr %>% 
  select(-MS_canopy, -plot) %>% 
  mutate(plant_cover = rowSums(.)) %>% 
  select(plant_cover)
total_cov$plot <- banwr$plot

type_cov1 <- banwr %>% 
  select(-MS_canopy) %>% 
  pivot_longer(`HOPOBT`:`new Unk Fab`, names_to = "spp", values_to = "cover") %>% 
  filter(cover != 0) # removed completely bare plots

# species details:

species <- data.frame(unique(type_cov1$spp));names(species) <- "spp"
write_csv(species, "data/species_list.csv")

# traits:
traits <- read_csv("data/species_traits.csv")

type_cov <- full_join(type_cov1, traits) %>% 
  group_by(plot, nativity, fun) %>% 
  summarise(cover = sum(cover)) %>% 
  pivot_wider(names_from = nativity:fun, values_from = cover)
type_cov[is.na(type_cov)] <- 0 # turn NA values to 0
type_cov <- inner_join(type_cov, plots)

ggplot(type_cov, aes(x = native_grass, y = exotic_grass))+
  geom_point(aes(color = site))
  # geom_boxplot(aes(group = exotic))



output <- full_join(sums, total_cov) %>% 
  full_join(type_cov) %>% 
  mutate(ms_canopy = case_when(ms_canopy == 0 ~ F,
                               ms_canopy == 1 ~ T))
output[is.na(output)] <- 0 # turn NAs to zero

write_csv(output, "data/2025_Plot_Summaries.csv")

ggplot(output, aes(x = ms_canopy, y = log(native_grass)))+
  geom_boxplot(alpha = 0.4, aes(group = ms_canopy, fill = ms_canopy))+
  theme_minimal(base_size = 20)+
  labs(x = "Under mesquite canopy", y = "ln Native grass cover (%)")+
  theme(legend.position = "none")

t.test((filter(output, ms_canopy == T)$native_grass), (filter(output, ms_canopy == F)$native_grass))



