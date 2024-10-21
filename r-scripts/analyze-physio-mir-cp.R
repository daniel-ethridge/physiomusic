library(tidyverse)
library(readr)
library(GGally)
library(Hmisc)
library(ggmatplot)

# Normalize function
normalize <- function(input) {
  avg = mean(input)
  std = sd(input)
  return((input - avg) / std)
}

outlier <- function(x) {
  x <= 3 & x >= -3
}

# Read in  CSV
physio_data <- read_csv("Documents/atlas/phd-research/research-software-dev/mir-to-physio/data/deap/physio-feature-data.csv")

# Clean the participant column and get rid of first column
clean_physio <- physio_data |> 
  mutate(participant = gsub(".bdf", "", participant)) |> 
  mutate(participant = gsub("s", "", participant)) |> 
  select(-"...1") |> 
  filter(stim_id != "baseline")

View(clean_physio)

# Normalize the data
physio_norm <- clean_physio |>
  group_by(participant) |> 
  mutate(across(mean_ibi:gsr_kurtosis, normalize))

# Remove outliers
physio_no_out <- physio_norm |>
  filter(if_all(mean_ibi:gsr_kurtosis, outlier))

View(physio_no_out)

# Aggregate data
aggregate_data <- physio_no_out |>
  group_by(stim_id) |>
  summarize_if(is.numeric, mean)

# aggregate_data <- physio_no_out |> 
#   group_by(stim_id) |> 
#   summarize(mean())

# View Aggregate data
View(aggregate_data)

# ### MIR ### #
# Read in file and change first column name
mir_data <- read_csv("Documents/atlas/phd-research/research-software-dev/mir-to-physio/data/deap/audio-feature-data.csv")
colnames(mir_data)[colnames(mir_data) == "file_name"] <- "stim_id"
View(mir_data)

# Clean MIR
clean_mir <- mir_data |> 
  mutate(stim_id = gsub("prep-audio-", "stim_", stim_id),
         stim_id = gsub(".wav", "", stim_id)) |> 
  select(-"...1")
  
View(clean_mir)

# normalize data
mir_norm <- clean_mir |> 
  mutate(across(2:ncol(clean_mir), normalize))

View(mir_norm)

# Remove outliers
mir_no_out <- mir_norm |> 
  filter(if_all(2:ncol(clean_mir), outlier))

View(mir_no_out)



total_data = merge(x=aggregate_data, y=mir_no_out, by="stim_id")

View(total_data)

total_numeric <- total_data |> 
  select(-stim_id)

x_frame <- total_numeric |> 
  select()

correlations <- rcorr(as.matrix(total_numeric))
View(correlations$P)
View(correlations$r[1:9, 10:ncol(corr_r)])
corr_r <- data.frame(correlations$r)[1:9, 10:ncol(corr_r)]
corr_p <- data.frame(correlations$P)[1:9, 10:ncol(corr_p)]

View(corr_r)
View(corr_p)

ggplot(total_numeric, aes(x=hr,
                          y=zero_cross_std)) +
  geom_point()

# physio_sm |> 
#   # filter(participant == "07") |> 
#   ggplot(aes(x = z_rmssd,
#              y = z_sdnn,
#              color = participant)) +
#   geom_point()
# 
# ggpairs(physio_no_out, columns = 3:ncol(physio_no_out))
