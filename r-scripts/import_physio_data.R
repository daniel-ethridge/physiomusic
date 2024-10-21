library(readr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(flexplot)

clean <- function(physio_clean) {
  physio_clean$mean_ibi[physio_clean$mean_ibi<660] = NaN
  physio_clean$mean_ibi[physio_clean$mean_ibi>1200] = NaN
  physio_clean$hr[physio_clean$hr>90] = NaN
  physio_clean$hr[physio_clean$hr<50] = NaN
  physio_clean$rmssd[physio_clean$rmssd<10] = NaN
  physio_clean$rmssd[physio_clean$rmssd>150] = NaN
  physio_clean$sdnn[physio_clean$sdnn<10] = NaN
  physio_clean$sdnn[physio_clean$sdnn>150] = NaN
  physio_clean$gsr_std = log(physio_clean$gsr_std)
  return(physio_clean)
}

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in raw CSV
physio_raw <- read_csv("../data/deap/features/physio-feature-data.csv") |> 
  subset(selec = c(participant, stim_id, mean_ibi, hr, rmssd, sdnn, gsr_std))

# Clean format physio dataframe
physio_clean <- physio_raw |> 
  clean_names() |>
  mutate(participant = str_replace_all(participant, ".bdf", "")) |>
  mutate(participant = str_replace_all(participant, "s", "")) |> 
  mutate(participant = as.numeric(participant))

physio_clean <- clean(physio_clean)

physio_clean <- physio_clean |> 
  group_by(participant) |> 
  mutate(delta_gsr = gsr_std - gsr_std[1]) |> 
  mutate(delta_rmssd = c(NaN, diff(rmssd)))

# Write formatted to CSV
write.csv(physio_clean, 
          "../data/deap/features/physio-formatted.csv", 
          row.names = F)
660
