library(tidyverse)
library(vegan)
library(ggrepel)

# comm = com analysis ;)

banwr <- read_csv("data/community_fall2025.csv") # read in community matrix
provel <- read_csv("data/mesquite_canopy.csv") %>% 
  mutate(MS_canopy = 1)
banwr <- full_join(banwr, provel)
banwr[is.na(banwr)] <- 0 # turn NA values to 0
plots <- read_csv("data/plots_by_site.csv") # read in plots by site
names(plots) <- c("plot", "site") # assign names to the plots file

mesq <- dplyr::select(banwr, plot, MS_canopy) %>% 
  mutate(MS_canopy = case_when(MS_canopy == 0 ~ F,
                               MS_canopy == 1 ~ T))

plot_sf <- st_read("data/BANWR_Plots.shp")

# # plot summaries for Dr. Gallery & Maria:
# 
# sums <- banwr %>% 
#   full_join(plots) %>% 
#   group_by(plot, site) %>% 
#   summarise(llg = ERALEH,
#             mesquite = MS_canopy)
# 
# total_cov <- banwr %>% 
#   select(-MS_canopy, -plot) %>% 
#   mutate(plant_cover = rowSums(.)) %>% 
#   select(plant_cover)
# total_cov$plot <- banwr$plot
# 
# type_cov1 <- banwr %>% 
#   select(-MS_canopy) %>% 
#   pivot_longer(`HOPOBT`:`DESCOV`, names_to = "spp", values_to = "cover") %>% 
#   filter(cover != 0)
# 
# species <- data.frame(unique(type_cov1$spp));names(species) <- "spp"
# write_csv(species, "data/species_list.csv")
# 
# # traits:
# traits <- read_csv("data/species_traits.csv")
# 
# type_cov <- full_join(type_cov1, traits) %>% 
#   group_by(plot, nativity) %>% 
#   summarise(cover = sum(cover)) %>% 
#   pivot_wider(names_from = nativity, values_from = cover)
# # type_cov[is.na(type_cov)] <- 0 # turn NA values to 0
# type_cov <- inner_join(type_cov, plots)
# 
# ggplot(type_cov, aes(x = native, y = exotic))+
#   geom_point(aes(color = site))
# 
# # make a data frame with native/nonnative??
# 
# # how many plots have mesquite canopy?
# View(banwr %>% 
#        inner_join(plots) %>% 
#   pivot_longer(`HOPOBT`:`DESCOV`, names_to = "spp", values_to = "cover") %>% 
#   select(-spp, -cover) %>% 
#   unique() %>% 
#   group_by(site, MS_canopy) %>% 
#   summarise(count = n()))
# 
# # how many plots have mesquite as a spp?
# View(banwr %>% 
#        inner_join(plots) %>% 
#        pivot_longer(`HOPOBT`:`DESCOV`, names_to = "spp", values_to = "cover") %>% 
#        filter(spp == "PROVEL") %>% 
#        mutate(pres = case_when(cover == 0 ~ F, cover != 0 ~ T)) %>% 
#        select(-MS_canopy) %>% 
#        group_by(site, pres) %>% 
#        summarise(count = n()))
# 
# # LLG?
# View(banwr %>% 
#        inner_join(plots) %>% 
#        pivot_longer(`HOPOBT`:`DESCOV`, names_to = "spp", values_to = "cover") %>% 
#        filter(spp == "ERALEH") %>% 
#        mutate(pres = case_when(cover == 0 ~ F, cover != 0 ~ T)) %>% 
#        select(-MS_canopy) %>% 
#        group_by(pres, site) %>% 
#        summarise(count = n()))

# Ordinations:
# remove rare species:
# function to figure out how many plots each spp is in? (column sum of cells != 0)

# binarize the banwr dataframe to get counts of how many plots each spp is present in
banwr_b <- banwr %>% 
  select(-plot) %>% 
  mutate(across(where(is.numeric), ~ifelse(.x >0, 1, 0)))
rares <- banwr_b %>% # make a list of how many plots each spp occurs in by summing the columns of the binary frame 
  colSums() %>% 
  data.frame()
spp_pool <- rownames(filter(rares, .>1)) # now we want to define our "species pool" which consists of species occurring on at least 5 plots across all 100


# run this chunk instead if above looking at the mesquite canopy differences (TESTING):
      # check for plots that are bare except for the mesquite:

banwr_b <- banwr %>% 
  select(-plot, -MS_canopy) %>% 
  mutate(across(where(is.numeric), ~ifelse(.x == 0, 0, 1)))
rares <- banwr_b %>% # make a list of how many plots each spp occurs in by summing the columns of the binary frame 
  colSums() %>% 
  data.frame()
spp_pool <- rownames(filter(rares, .>5)) # set this number to the threshold of # of plots to cut off rares at



##########


banwr_com <- banwr[, which(colnames(banwr) %in% spp_pool)] # filter out columns that aren't in the spp pool
banwr_com$plot <- banwr$plot # give the plot column back

com <- banwr_com %>% 
  column_to_rownames(var = "plot") %>%    # turn plot column back to row names
  wisconsin() # wisco standardization # ADD THIS BACK IN FOR THE NMDS !!
com <- com[rowSums(com) != 0, ] # get rid of plots where there are no plants.... 


com_plots <- com %>%
  mutate(plot = rownames(.)) %>% # get plot column back in
  inner_join(mesq) # add in mesquite canopy categorical variable

# com_plots <- inner_join(com_plots, plots) # then join with the plots by site df to know the different sites

dist_b <- vegdist(com, method = "bray") # create dist mat

# clusters <- kmeans(dist_b, centers = 4)$cluster
# clust_df <- data.frame(clusters)
# clust_df$plot <- rownames(clust_df)

banwr_nmds <- metaMDS(dist_b, k = 2, trymax = 200) # run NMDS
banwr_nmds$stress # check stress; < 0.2 is good!
stressplot(banwr_nmds) # view stress plot

com_env <- envfit(banwr_nmds, com) # fit the original variables to our ordination

# then some magic to turn these envfit lines into lines starting from the origin that i can put into ggplot
env_df <- data.frame(com_env$vectors$arrows) %>% # extracting the end points of each envfit vector
  mutate(spp = rownames(.)) %>% 
  filter(com_env$vectors$pvals <= 0.05)
env0 <- data.frame(NMDS1 = rep(0, nrow(env_df)),  # creating the zeroes with a spp column
                   NMDS2 = rep(0, nrow(env_df)),
                   spp = env_df$spp)
env_res <- rbind(env_df, env0) # combining them together

banwr_scores <- data.frame(cbind(scores(banwr_nmds), rownames(com))) # get scores of the NMDS and combine them with the spp names
names(banwr_scores) <- c("NMDS1", "NMDS2", "plot") # assign column names to df from previous line

banwr_scores <- full_join(banwr_scores, mesq) %>% 
  mutate(NMDS1 = as.numeric(NMDS1), NMDS2 = as.numeric(NMDS2))

banwr_centroids <- banwr_scores %>% 
  group_by(MS_canopy) %>% 
  summarise(NMDS1 = mean(NMDS1, na.rm = T), 
            NMDS2 = mean(NMDS2, na.rm = T))

ggplot(data = banwr_scores, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = as.factor(MS_canopy)), alpha = 0.3)+
  stat_ellipse(aes(color = as.factor(MS_canopy)))+
  geom_point(pch = 24, size = 4, data = banwr_centroids, aes(fill = as.factor(MS_canopy), x = NMDS1, y = NMDS2))+
  geom_line(data = env_res, aes(group = spp))+
  geom_label_repel(data = env_df, aes(label = spp),
                   size = 5, fill = NA, label.size = NA)+
  theme_light(base_size = 25)

ggplot(data = banwr_scores, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = as.factor(MS_canopy)), alpha = 0.3)+
  # geom_label(aes(label = plot))+
  stat_ellipse(aes(color = as.factor(MS_canopy)))+
  geom_point(pch = 24, size = 4,
             data = banwr_centroids, aes(fill = as.factor(MS_canopy), x = NMDS1, y = NMDS2))+
  # geom_line(data = env_res, aes(group = spp))+
  # geom_label_repel(data = env_df, aes(label = spp),
  #                  size = 5, fill = NA, label.size = NA)+
  theme_light(base_size = 25)

adonis2(com ~ as.factor(com_plots$MS_canopy), data = banwr)
anova(betadisper(vegdist(com, method  = "bray"),
                 group = as.factor(com_plots$MS_canopy), type = "centroid"))


# let's investigate these clusters
# ...with un-standardized data?


com_un <- banwr_com %>%
  inner_join(mesq)

com <- rownames_to_column(com, var = "plot")

com_un_long <- com_un %>% 
  pivot_longer(cols = `HOPOBT`:`AMBCON`, names_to = "spp",
               values_to = "cover")
  # mutate(cover = case_when(cover == 0 ~ 0.25,
  #                          cover != 0 ~ cover)) %>% 
  # mutate(cover_log = log(cover)) %>% 
  # filter(!is.na(cover_log), is.finite(cover_log))

ggplot(com_un_long, aes(x = cover, color = MS_canopy))+
  geom_density()+
  facet_wrap(~spp, scales = "free")

t.test(filter(com_un_long, spp == "ERALEH", MS_canopy == T)$cover_log,
       filter(com_un_long, spp == "ERALEH", MS_canopy == F)$cover_log,
       var.equal = T)

t.test(filter(com_un_long, spp == "HOPOBT", MS_canopy == T)$cover_log,
       filter(com_un_long, spp == "HOPOBT", MS_canopy == F)$cover_log,
       var.equal = T)

t.test(filter(com_un_long, spp == "DIGCAL", MS_canopy == T)$cover_log,
       filter(com_un_long, spp == "DIGCAL", MS_canopy == F)$cover_log,
       var.equal = T)


# 
# ggplot(com, aes(x = clusters, y = DIGCAL))+
#   geom_boxplot(aes(group = as.factor(clusters)))

# make long:

com_long <- com %>% 
  pivot_longer(cols = `HOPOBT`:`AMBCON`, names_to = "spp", values_to = "cover") %>% 
  inner_join(plots)

ggplot(com_long, aes(y = cover))+
  geom_boxplot()+
  # geom_point(alpha = 0.5)+
  facet_wrap(~spp, scales = "free")


# RDA (redundancy analysis in vegan)

pca_test <- stats::princomp(x = com)
biplot(pca_test)


pca_veg <- rda(com, scale = T)
pca_scores <- data.frame(scores(pca_veg, display = "sites"), Site = com_plots$MS_canopy)
spp_scores <- data.frame(scores(pca_veg, display = "species"))
spp_scores$variable <- rownames(spp_scores)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Site))+
  geom_point()+
  stat_ellipse(level = 0.95)+
  geom_segment(data = spp_scores, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "black")+
  geom_text(data = spp_scores, aes(x = PC1, y = PC2, label = variable), 
            color = "black", hjust = 1, vjust = -2)+
  theme_minimal()

######
ggplot(com, aes(x = BOUROT, y = ms_discrete))+
  geom_boxplot()+
  labs(x = "Rothrock's grama cover (%)",
       y = "Under mesquite canopy?")+
  theme_light()



# PCA?
#install.packages('ggbiplot')
library(ggbiplot)

pc_out <- princomp(select(com, -ms_discrete))
biplot(pc_out)

ggbiplot(pc_out, choices = 1:2)


# Diversity Indices:

sh <- diversity(banwr_com %>% 
                  column_to_rownames(var = "plot"), 
                index = "shannon") %>% 
  as_tibble() %>% 
  transmute(shannon = value)

sh$plot <- banwr_com$plot

richness <- specnumber(banwr_com %>% 
                         column_to_rownames(var = "plot")) %>% 
  as_tibble() %>% 
  transmute(rich = value)
richness$plot <- banwr_com$plot


sh <- inner_join(sh, plots) %>% 
  inner_join(richness) %>% 
  full_join(provel) %>% 
  mutate(MS_canopy = case_when(MS_canopy == 1 ~ T,
                               .default = F)) # no canopy = 0


ggplot(sh, aes(x = site, y = shannon))+
  geom_boxplot()
t.test(filter(sh, site == "Control")$shannon, filter(sh, site == "Treatment")$shannon)
# shannon diversity not significantly different btwn the control and treatment pre-burn


ggplot(sh, aes(x = MS_canopy, y = shannon))+
  geom_boxplot(alpha = 0.4, aes(group = interaction(MS_canopy), fill = MS_canopy))+
  # geom_point(alpha = 0.4, aes(shape = MS_canopy, color = site))+
  theme_minimal(base_size = 20)+
  ylim(0, 2)+
  labs(x = "Under mesquite canopy", y = "Shannon diversity")+
  # stat_summary(fun = max, fun.max = length,
  #              geom = "text", aes(group = MS_canopy, label = ..ymax..),
  #              vjust = -1)
  theme(legend.position = "none")

ggplot(sh, aes(x = MS_canopy, y = rich))+
  geom_boxplot(alpha = 0.4, aes(group = interaction(MS_canopy), fill = MS_canopy))+
  theme_minimal(base_size = 20)+
  labs(x = "Under mesquite canopy", y = "Species richness")+
  theme(legend.position = "none")

t.test(x = filter(sh, MS_canopy == 1)$shannon,
       y = filter(sh, MS_canopy == 0)$shannon,
       var.equal = T)
t.test(x = filter(sh, MS_canopy == 1)$rich,
       y = filter(sh, MS_canopy == 0)$rich,
       var.equal = T)

(mean((filter(sh, MS_canopy == 1)$rich) - mean(filter(sh, MS_canopy == 0)$rich)))/mean(filter(sh, MS_canopy == 0)$rich)
