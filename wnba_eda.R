# PURPOSE: EDA with WNBA 2022 data
library(tidyverse)
library(wehoop)
library(flexclust)
library(ggrepel)


# Building the dataset (using code from Prof. Yurko) ----------------------

wnba_pbp_data <- load_wnba_pbp(2022)

# Get the shots and clean this data a bit:
wnba_shots_data <- wnba_pbp_data %>%
  filter(shooting_play)
# Remove unnecessary columns:
wnba_shots_data <- wnba_shots_data %>%
  dplyr::select(-shooting_play, -id, -participants_2_athlete_id,
                -type_abbreviation, -season, -season_type, 
                -home_team_spread, -game_spread, -home_favorite)
# Save this file:
write_csv(wnba_shots_data, 
          "data/wnba_shots_2022.csv")


# Exploring properties of the dataset -------------------------------------


# Counting number of games
wnba_shots_data %>%
  distinct(game_id) %>%
  count()

# Counting number of successful and failed shot attempts
summary(wnba_shots_data$scoring_play)

# Some properties on the shot type variable
wnba_shots_data %>%
  count(type_text)


# Shot frequency by period ------------------------------------------------


# Looking at total number of shots per period
wnba_shots_data %>%
  count(period_display_value)

# Visualizing with a bar chart
wnba_shots_data %>%
  filter(period_display_value != "OT") %>%
  ggplot(aes(x = period_display_value)) +
  geom_bar() +
  labs(x = "Period", y = "Number of shots",
       title = "The fourth quarter is the most shot-friendly quarter in the WNBA") +
  theme_minimal()


# K-means++: pts per shot vs shot distance for different shot types -------------------------------------------


# Calculating shot distance to basket
wnba_shots_data <- wnba_shots_data %>% 
  filter(coordinate_x >= 0, 
         coordinate_y >= 0) %>% 
  mutate(shot_distance = sqrt((coordinate_x - 25)^2 + (coordinate_y)^2))

# Calculating avg pts return and avg shot distance for each shot type
avg_pts_distance <- wnba_shots_data %>%
  group_by(type_text) %>%
  summarize(n = n(), avgpts = mean(score_value), avgdist = mean(shot_distance)) %>%
  filter(n >= 50) %>%
  arrange(desc(avgpts))

# Scaling the variables for the purpose of clustering
avg_pts_distance <- avg_pts_distance %>%
  mutate(std_avgdist = as.numeric(scale(avgdist)),
         std_avgpts = as.numeric(scale(avgpts)))

# K-means++ clustering
kmeanspp <-
  kcca(dplyr::select(avg_pts_distance,
                     std_avgdist, std_avgpts),
       k = 3, control = list(initcent = "kmeanspp"))

avg_pts_distance %>%
  mutate(shot_clusters = as.factor(kmeanspp@cluster)) %>%
  ggplot(aes(x = avgdist, y = avgpts, color = shot_clusters)) +
  geom_point() +
  geom_text_repel(label = avg_pts_distance$type_text, size = 3.5) +
  theme_minimal()

# Elbow plot (with a lot of help from lecture example)
n_clusters_search <- 2:12
tibble(total_wss = 
         # Compute total WSS for each number by looping with sapply
         sapply(n_clusters_search,
                function(k) {
                  kmeans_results <- kmeans(dplyr::select(avg_pts_distance,
                                                         std_avgdist,
                                                         std_avgpts),
                                           centers = k, nstart = 30)
                  # Return the total WSS for choice of k
                  return(kmeans_results$tot.withinss)
                })) %>%
  mutate(k = n_clusters_search) %>%
  ggplot(aes(x = k, y = total_wss)) +
  xlim(min = 1, max = 12) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters K", y = "Total WSS") +
  theme_bw()


# H-clustering: pts per shot vs shot distance for different shot types --------


type_dist <- dist(dplyr::select(avg_pts_distance, std_avgdist, std_avgpts))

# Centroid linkage
type_hclust <- hclust(type_dist, method = "centroid")

avg_pts_distance %>%
  mutate(type_clusters = as.factor(cutree(type_hclust, k = 3))) %>%
  ggplot(aes(x = avgdist, y = avgpts, color = type_clusters)) +
  geom_point() +
  geom_text_repel(label = avg_pts_distance$type_text, size = 3.5) +
  labs(x = "Average distance from basket", y = "Average points return") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_colorblind()


# Description of dataset (text) ----------------------------------------------------

# WNBA shot dataset
# 
# Accessed through the wehoop package
# 
# Every row/observation is a singular shot attempt from the 2022 WNBA Season through 10 June, 2022
# 
# Contains 8854 shots from 51 games
# 
# We used information on the following variables:
# Coordinates and distance to basket
# Time and period
# Types
# Success and failure
