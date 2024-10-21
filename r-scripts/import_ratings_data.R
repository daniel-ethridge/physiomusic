library(tidyverse)
library(readr)
library(janitor)

# Normalize function
normalize <- function(input) {
  avg = mean(input)
  std = sd(input)
  return((input - avg) / std)
}

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ratings_raw <- read_csv("../data/deap/Metadata/metadata_csv/participant_ratings.csv")

ratings_clean <- ratings_raw |> 
  clean_names() |> 
  rename(participant = participant_id,
         stim_id = experiment_id) |> 
  select(-trial, -start_time) |> 
  arrange(participant, stim_id) |> 
  group_by(participant) |> 
  mutate(across(valence:familiarity, normalize))

ratings_clean$stim_id <- paste0("stim_", ratings_clean$stim_id)

write.csv(ratings_clean, 
          "../data/deap/Metadata/metadata_csv/clean-ratings.csv",
          row.names = F)
