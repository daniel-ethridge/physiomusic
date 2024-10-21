library(readr)
library(janitor)
library(tidyverse)
library(flexplot)

# Normalize function
normalize <- function(input) {
  avg = mean(input)
  std = sd(input)
  return((input - avg) / std)
}

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in raw CSV
mir_raw <- read_csv("../data/deap/features/audio-feature-data.csv")

# Filtering outliers
mir_filt <- mir_raw


# Clean format physio dataframe
mir_clean <- mir_filt |> 
  clean_names() |> 
  select(-x1) |> 
  rename(stim_id = file_name) |> 
  mutate(stim_id = str_replace(stim_id, "prep-audio-", "stim_")) |> 
  mutate(stim_id = str_replace(stim_id, ".wav", "")) |> 
  mutate(across(spec_cent_mean:specs15_std, normalize))

flexplot(mfcc8_mean ~ mfcc10_mean, data=mir_clean)


# Write formatted to CSV
write.csv(mir_clean, 
          "../data/deap/features/mir-formatted.csv", 
          row.names = F)


