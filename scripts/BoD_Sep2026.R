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

# remove species with no cover in the treatment area:

com_b <- com_change %>% 
  select(-plot, -year) %>% 
  mutate(across(where(is.numeric), ~ifelse(.x == 0, 0, 1)))

rares <- com_b %>% 
  colSums() %>% 
  data.frame()

spp_pool <- rownames(filter(rares, .>5))

com_use <- com_change[, which(colnames(com_change) %in% spp_pool)]
com_use$plot <- com_change$plot
com_use$year <- com_change$year
com_use$plot_year <- paste(com_use$plot, com_use$year)
com_raw <- com_use %>% 
  select(-plot, -year)

com <- com_raw %>% 
  column_to_rownames(var = "plot_year") %>%    # turn plot column back to row names
  wisconsin()

com <- com[rowSums(com) != 0, ]

com_plots <- com %>%
  mutate(plot = rownames(.)) # get plot column back in

dist_b <- vegdist(com, method = "bray")

banwr_nmds <- metaMDS(dist_b, k = 2, trymax = 200) # run NMDS
banwr_nmds$stress # check stress; < 0.2 is good!
stressplot(banwr_nmds) # view stress plot
com_env <- envfit(banwr_nmds, com)


env_df <- data.frame(com_env$vectors$arrows) %>% # extracting the end points of each envfit vector
  mutate(spp = rownames(.)) %>% 
  filter(com_env$vectors$pvals <= 0.05)
env0 <- data.frame(NMDS1 = rep(0, nrow(env_df)),  # creating the zeroes with a spp column
                   NMDS2 = rep(0, nrow(env_df)),
                   spp = env_df$spp)
env_res <- rbind(env_df, env0) # combining them together

banwr_scores <- data.frame(cbind(scores(banwr_nmds), rownames(com))) # get scores of the NMDS and combine them with the spp names
names(banwr_scores) <- c("NMDS1", "NMDS2", "plot") # assign column names to df from previous line
banwr_scores$year <- str_split_i(banwr_scores$plot, " ", 2)

banwr_scores <- banwr_scores %>% 
  mutate(NMDS1 = as.numeric(NMDS1), NMDS2 = as.numeric(NMDS2))

banwr_centroids <- banwr_scores %>% 
  group_by(year) %>% 
  summarise(NMDS1 = mean(NMDS1, na.rm = T), 
            NMDS2 = mean(NMDS2, na.rm = T))

library(ggrepel)

ggplot(data = banwr_scores, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = as.factor(year)), alpha = 0.3)+
  stat_ellipse(aes(color = as.factor(year)), level = 0.99)+
  geom_point(pch = 24, size = 4, data = banwr_centroids, 
             aes(fill = as.factor(year), x = NMDS1, y = NMDS2))+
  geom_line(data = env_res, aes(group = spp))+
  geom_label_repel(data = env_df, aes(label = spp),
                   size = 5, fill = NA, label.size = NA)+
  theme_light(base_size = 25)

adonis2(com ~ as.factor(banwr_scores$year), data = com) # anova is sig; difference in mean or dispersion?
anova(betadisper(vegdist(com, method  = "bray"),
                 group = as.factor(banwr_scores$year), type = "centroid")) # they differ in dispersion!






