# import libraries
library(tidyverse)
library(lme4)
library(janitor)
library(MuMIn)
library(sjPlot)
library(cowplot)
library(cluster)
library(vegan)

# set wd to active script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import CSVs
hrv_raw <- read_csv("../data/deap/features/hrv.csv")
mir_raw <- read_csv("../data/deap/features/mir.csv")

# clean up data
hrv_clean <- hrv_raw %>%
  clean_names() |> 
  pivot_longer(cols=-c("file", "baseline"), names_to = "song", values_to = "RMSSD") |> 
  rename(participant = file) |>
  mutate(song = str_replace(song, "audio_file_", "")) |> 
  mutate(song = str_replace(song, "_0", "")) |> 
  mutate(delta_RMSSD = RMSSD - baseline) |> 
  select(-c(RMSSD, baseline))  

mir_clean <- mir_raw %>%
  clean_names() %>%
  select(-x1) %>%
  mutate(audio_file_name = str_replace(audio_file_name, "audio-file-", "")) %>%
  mutate(audio_file_name = str_replace(audio_file_name, ".0.wav", "")) %>%
  rename(song = audio_file_name)

model_data <- hrv_clean %>%
  left_join(mir_clean, by = "song") %>%
  drop_na() %>%
  filter(participant != "s23.bdf")

# build model
global_model <- lmer(delta_RMSSD ~ mean_spectral_centroid + 
                std_spectral_centroid + 
                mean_mfcc_1 +
                std_mfcc_1 + 
                (1|participant), 
              data = model_data, na.action = na.fail)

# do model selection exercise
MuMIn::dredge(global_model)

# build most likely model from dr +edge
most_likely_model <- lmer(delta_RMSSD ~  
                            std_mfcc_2 + 
                            std_mfcc_4 + 
                            (1|participant), 
                          data = model_data)

# evaluate model
plot(global_model)
summary(global_model)

# plot partial regressions
all_pr <- plot_model(global_model, type = "eff", show.data = TRUE, vline.color = "blue")

# pull out each partial regression
pr_1 <- all_pr[[1]] +
  labs(x="Mean spectral centroid", y=paste0(expression(Delta), " HRV (RMSSD)"), title="")
pr_2 <- all_pr[[2]] +
  labs(x="Std spectral centroid", y="", title="")
pr_3 <- all_pr[[3]] +
  labs(x="Mean MFCC", y=paste0(expression(Delta), " HRV (RMSSD)"), title="")
pr_4 <- all_pr[[4]] +
  labs(x="Std MFCC", y="", title="")

# plot all partial regressions together
plot_grid(pr_1, pr_2, pr_3, pr_4, nrow = 2, ncol = 2)


# make wide format data
dist_data <- mir_clean %>%
  select(-song)

rownames(dist_data) <- mir_clean$song

# build difference matrix
difference_matrix <- daisy(dist_data, metric = "euclidean")

# run MDS
nmds <- metaMDS(difference_matrix)

nmds_points <- as.data.frame(scores(nmds)) %>%
  mutate(song_id = mir_clean$song)

# fit variables to MDS space
nmds_fit <- envfit(nmds, mir_clean)

nmds_vectors <- as.data.frame(scores(nmds_fit, display = "vectors"))
nmds_vectors_scaled <- nmds_vectors %>%
  mutate(variable = rownames(nmds_vectors),
         NMDS1 = NMDS1*800, NMDS2 = NMDS2*800)

# make clusters
# hierarchical clustering (for dendrogram)
clusters <- agnes(difference_matrix, diss = TRUE)
plot(clusters) 

# do k-means clustering
km_clust_raw <- as.data.frame(kmeans(dist_data, centers = 5, nstart = 10)$cluster)
km_clust_clean <- km_clust_raw %>%
  mutate(song_id = rownames(km_clust_raw)) %>%
  rename(cluster = 1)

nmds_clusters <- nmds_points %>%
  left_join(km_clust_clean, by = "song_id")

nmds_hulls <- nmds_clusters %>%
  group_by(cluster) %>%
  slice(chull(NMDS1, NMDS2))

ggplot() +
  # plot nmds points
  geom_point(data = nmds_clusters, aes(x = NMDS1, y = NMDS2, col = as.factor(cluster)), size = 5) +
  # add ordihulls for clusters
  geom_polygon(data = nmds_hulls, aes(x = NMDS1, y = NMDS2, group = as.factor(cluster), fill = as.factor(cluster)), alpha = 0.1) +
  coord_fixed() +
  # add vectors for variables
  geom_segment(data = nmds_vectors_scaled,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), 
               colour = "grey") +
  geom_text(data = nmds_vectors_scaled, aes(x = NMDS1, y = NMDS2, label = variable),
            size = 3) +
  theme_minimal() +
  theme(legend.position = "none")








































