library(tidyverse)

banwr <- read_csv("data/community_fall2025.csv") # read in community matrix
provel <- read_csv("data/mesquite_canopy.csv") %>% 
  mutate(MS_canopy = 1)
banwr <- full_join(banwr, provel)
banwr[is.na(banwr)] <- 0 # turn NA values to 0
plots <- read_csv("data/plots_by_site.csv") # read in plots by site
names(plots) <- c("plot", "site")

write_csv(banwr, "data/community_pre.csv")


# take column sums of all species:

banwr_spp <- data.frame(colSums(dplyr::select(banwr, -plot)))



