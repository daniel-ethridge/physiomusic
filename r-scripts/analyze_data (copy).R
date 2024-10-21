library(tidyverse)
library(flexplot)
library(party)
library(lme4)

# outlier <- function(x) {
#   x <= 3 & x >= -3
# }

# Set wd to active script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in  CSV files
physio_formatted <- read_csv("../data/deap/features/physio-formatted.csv")
mir_formatted <- read_csv("../data/deap/features/mir-formatted.csv")
ratings_formatted <- read_csv("../data/deap/Metadata/metadata_csv/clean-ratings.csv")

# Select physio column of interest
outcome_variable <- "gsr_mean"

flexplot(gsr_mean ~ 1, data=physio_formatted)

# normalize variables
# physio_formatted$rmssd = log(physio_formatted$rmssd)
# physio_formatted$sdnn = log(physio_formatted$sdnn)
physio_formatted$gsr_mean = log(physio_formatted$gsr_mean)
# physio_formatted$gsr_std = log(physio_formatted$gsr_std)

flexplot(gsr_mean ~ 1, data=physio_formatted)

# Create 3 column data frame with column of interest
reduced_physio <- physio_formatted |> 
  select(c("participant", "stim_id", outcome_variable)) |> 
  rename(outcome = outcome_variable) |> 
  filter(participant != "18",
         stim_id != "baseline")

# Create Model data
model_data <- reduced_physio |> 
  left_join(mir_formatted, by = "stim_id") |> 
  left_join(ratings_formatted, by = c("participant"="participant",
                                      "stim_id"="stim_id"))

full = lmer(outcome ~ zero_cross_std + mfcc6_std + valence + (valence|participant), data=model_data)
reduced = lmer(outcome ~ zero_cross_std + valence + (valence|participant), data=model_data)
compare.fits(outcome ~ zero_cross_std | valence + mfcc6_std, data=model_data, full, reduced)
model.comparison(full, reduced)


# Normalize participant ratings of stimuli
# normalized_data <- model_data |>
#   group_by(participant) |>
#   mutate(valence = normalize(valence),
#          arousal = normalize(arousal),
#          dominance = normalize(dominance),
#          liking = normalize(liking),
#          familiarity = normalize(familiarity))

# sm_data <- model_data |> 
#   group_by(participant) |> 
#   mutate(outcome = normalize(outcome)) |> 
#   ungroup() |> 
#   select(-participant)
# 
# forest <- cforest(outcome ~ ., data=sm_data)
# estimates(forest)

# Model 1: Std dev of Zero Crossing rate as a fixed effect vs data mean
full <- lmer(outcome ~ familiarity + liking + (1|participant), data = model_data)
reduced <- lmer(outcome ~ 1 + (1|participant), data = model_data)
compare.fits(outcome ~ liking | familiarity, data=model_data, full, reduced)
compare.fits(outcome ~ familiarity | liking, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

# Model 2: Std dev of Zero Crossing rate as fixed and random effect vs only as fixed effect (ie fixed slopes vs variable slopes)
full <- lmer(outcome ~ zero_cross_std + (zero_cross_std|participant), data = model_data)
reduced <- lmer(outcome ~ zero_cross_std + (1|participant), data = model_data)
compare.fits(outcome ~ zero_cross_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

# Model 3: Std Dev of MFCC 6 as a fixed effect vs data mean
full <- lmer(outcome ~ mfcc6_std + (1|participant), data = model_data)
reduced <- lmer(outcome ~ 1 + (1|participant), data = model_data)
compare.fits(outcome ~ mfcc6_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

# Model 4: Std Dev of MFCC 6 as a fixed effect and random effect vs only as fixed effect (ie fixed slopes vs variable slopes)
full <- lmer(outcome ~ mfcc6_std + (mfcc6_std|participant), data = model_data)
reduced <- lmer(outcome ~ mfcc6_std + (1|participant), data = model_data)
compare.fits(outcome ~ mfcc6_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

# For GSR RMS
full <- lmer(outcome ~ zero_cross_std + mfcc6_std + (zero_cross_std|participant), data = model_data)
reduced <- lmer(outcome ~ zero_cross_std + (zero_cross_std|participant), data=model_data)
compare.fits(outcome ~ zero_cross_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full, formula = outcome ~ mfcc6_std | zero_cross_std)

# For GSR std
full <- lmer(outcome ~ zero_cross_std + (1|participant), data=model_data)
reduced <- lmer(outcome ~ zero_cross_std + (1|participant), data=model_data)
model.comparison(full, reduced)

# For Mean IBI
full <- lmer(outcome ~ zero_cross_std + (1|participant), data = model_data)
reduced <- lmer(outcome ~ 1 + (1|participant), data = model_data)
compare.fits(outcome ~ mfcc6_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full, plot="model")

# For RMSSD
full <- lmer(outcome ~ zero_cross_std + (zero_cross_std|participant), data = model_data)
reduced <- lmer(outcome ~ zero_cross_std + (1|participant), data = model_data)
compare.fits(outcome ~ zero_cross_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

cor(sm_data$mfcc6_std, sm_data$zero_cross_std)

df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df) <- c("first", "second", "third", "fourth")

# Iterative random forest over each participant individually
for (n in 1:100) {
  print(n)
  for (i in 1:20) {
    partial <- model_data |>
      filter(as.numeric(participant) == i) |>
      select(-participant)
    rf_model <- cforest(outcome~., data=partial)
    est <- estimates(rf_model)

    df[nrow(df) + 1,] = c(names(est$importance[1]), 
                          names(est$importance[2]), 
                          names(est$importance[3]), 
                          names(est$importance[4]))
  }
}

write.csv(df, 
          "../data/deap/features/rf-rmssd.csv",
          row.names = F)

df <- read.csv("../data/deap/features/rf-gsr-std.csv")

flexplot(first~1, data=df) + theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=1))
flexplot(second~1, data=df) + theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=1))
flexplot(third~1, data=df) + theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=1))
flexplot(fourth~1, data=df) + theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=1))

# Collective random forest
df <- read.csv("../data/deap/features/rf-model-comp.csv")
flexplot(mir_feature~1, data=df)
