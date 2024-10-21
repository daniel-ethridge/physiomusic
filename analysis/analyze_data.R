# Import packages
library(tidyverse)
library(flexplot)
library(party)
library(lme4)
library(car)
library(MuMIn)
library(report)

# Set wd to active script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in  CSV files (Change these paths to whatever they need to be)
physio_formatted <- read_csv("physio-formatted.csv")
mir_formatted <- read_csv("mir-formatted.csv")

mir_org <- mir_formatted |> 
  arrange(tempo_mean)

View(mir_org)

# Select physio column of interest. (gsr_mean and mean_ibi seem to show the largest effects) 
outcome_variable <- "rmssd"

flexplot(rmssd ~ 1, data=physio_formatted)

# normalize gsr_mean (only necessary if outcome is 'gsr_mean', but doesn't hurt
# regardless)
physio_formatted$gsr_mean = log(physio_formatted$gsr_mean)
physio_formatted$rmssd = log(physio_formatted$rmssd)

# Create 3 column data frame with column of interest
reduced_physio <- physio_formatted |> 
  select(c("participant", "stim_id", outcome_variable)) |> 
  rename(outcome = outcome_variable) |> 
  filter(participant != "18",
         stim_id != "baseline")

flexplot(outcome ~ 1, data=reduced_physio)

# Create Model data
model_data <- reduced_physio |> 
  left_join(mir_formatted, by = "stim_id")

View(model_data)

flexplot(outcome ~ zero_cross_std, data=model_data, method)

new_mod <- lm(outcome ~ zero_cross_std, data=model_data)
full <- lmer(outcome ~ zero_cross_std + (1|participant), data = model_data)
AIC(logLik(new_mod))
AIC(logLik(full))

# Model 1: Std dev of Zero Crossing rate as a fixed effect vs data mean
full <- lmer(outcome ~ zero_cross_std + (1|participant), data = model_data)
reduced <- lmer(outcome ~ 1 + (1|participant), data = model_data)
compare.fits(outcome ~ zero_cross_std, data=model_data, full, reduced)
model.comparison(full, reduced)
summary(full)
estimates(full)
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
estimates(full)
visualize(full)

test_mod = lmer(outcome ~ mfcc6_std + zero_cross_std + (1|participant), data=model_data)
visualize(test_mod)
vif(test_mod)

global_model = lmer(outcome ~ 
                      mfcc6_std + 
                      zero_cross_std + 
                      tempo_mean +
                      spec_cent_mean + 
                      spec_cent_std +
                      (1|participant), data=model_data, na.action = na.fail)

dredge(global_model)

report(global_model)

# Emphasize correlation, not causation
# Directed acyclic graph
