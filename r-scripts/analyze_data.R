library(tidyverse)
library(flexplot)
library(party)
library(lme4)
library(MuMIn)
library(janitor)
library(ggplot2)
library(cowplot)
library(ggeffects)
library(sjPlot)
library(car)

# Set wd to active script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in  CSV files (Change these paths to whatever they need to be)
physio_formatted <- read_csv("../data/deap/features/physio-formatted.csv")
mir_formatted <- read_csv("../data/deap/features/mir-formatted.csv")
# ratings_formatted <- read_csv("../data/deap/Metadata/metadata_csv/clean-ratings.csv")

# Create 3 column data frame with column of interest
reduced_physio <- physio_formatted |> 
  select(c("participant", "stim_id", gsr_std, hr, mean_ibi, delta_rmssd)) |> 
  filter(stim_id != "baseline")

# Create Model data
model_data <- reduced_physio |> 
  left_join(mir_formatted, by = "stim_id") |> 
  # left_join(ratings_formatted, by = c("participant" = "participant",
  #                                     "stim_id" = "stim_id")) |>
  drop_na() |> 
  mutate(participant = as.factor(participant))

write.csv(model_data, 
          "../data/deap/features/model_data.csv", 
          row.names = F)

# flexplot(zero_cross_std ~ mfcc6_std, data=model_data)

flexplot(specs9_mean ~ dominance, data=model_data)

flexplot(gsr_std~1, data=model_data)

# Create models

summary(full)
report::report(full)

visualize(full)


pr <- sjPlot::plot_model(full, terms=c("mfcc6_std"), type="eff", show.data=TRUE, jitter = 0.2)  # eff ==> effects

pr[[1]]

?plot_model

?predict_response

full <- lmer(gsr_std ~ 
                   zero_cross_std + 
                   mfcc6_std + 
                   (zero_cross_std|participant) +
                   (mfcc6_std|participant),
                 data=model_data)

report::report(full)

df <- predict_response(full, c("mfcc6_std", "participant"))

part_vec <- as.array(sample(unique(model_data$participant), 5))
part_vec <- c(13, 15, 19, 24, 30)

get_partial <- function(mod_data, idx) {
  part_data <- model_data |> 
    filter(participant == idx)

  partial_model = lm(gsr_std ~ mfcc6_std, data=part_data)
  return(partial_model)
}

p1 <- predict_response(get_partial(model_data, part_vec[1]), c("mfcc6_std"))
p1 <- predict_response(get_partial(model_data, part_vec[1]), c("mfcc6_std"))
p1 <- predict_response(get_partial(model_data, part_vec[1]), c("mfcc6_std"))
p1 <- predict_response(get_partial(model_data, part_vec[1]), c("mfcc6_std"))
p1 <- predict_response(get_partial(model_data, part_vec[1]), c("mfcc6_std"))

ggplot(data=filter(model_data, participant %in% part_vec)) +
  geom_point(aes(x = mfcc6_std, y = gsr_std, color=participant)) +
  geom_line(data=df, aes(x=x, y=predicted)) + 
  geom_line(data=p1, aes(x=x, y=predicted))

test_groups <- model_data %>%
  group_by(participant) %>%
  mutate(fit = map(., ~lm(.$gsr_std ~ 
                                .$zero_cross_std + 
                                .$mfcc6_std)),
         predictions = map(fit, predict_response)) %>%
  ungroup(predictions)


ggplot(data = df, aes(x=x, y = predicted, color=group)) + 
  geom_line()+ 
  geom_point(data=model_data, aes(x=mfcc6_std, y=gsr_std, color=participant)) + 
  facet_wrap(~group, ncol=7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = )) + 
  labs(title="Individual Participant Data") +
  xlab("Std Dev of MFCC6") +
  ylab("ln(Std Dev of GSR)")

ggplot()
ggplot(df, aes(x=x,
               y=predicted)) +
  geom_line()


null <- lmer(mean_ibi ~ zero_cross_std + (1|participant), data=model_data)
compare.fits(mean_ibi ~ zero_cross_std | mfcc6_std, data=model_data, full, null)
model.comparison(full, null)
visualize(full, plot="model")

dredge(full)

report::report(full)

full <- lmer(gsr_std ~ mfcc6_std + (mfcc6_std|participant), data=model_data)
summary(full)

reduced <- lmer(gsr_std ~ mfcc6_std + (1|participant), data=model_data)

model.comparison(full, reduced)


visualize(full, plot="model")
visualize(reduced, plot="model")

AIC(full)
AIC(reduced)

report::report(full)
estimates(full)
reduced <- lmer(gsr_std ~ zero_cross_std + (1|participant), data=model_data)
model.comparison(full, reduced)
visualize(full, plot="residuals")
estimates(full)

report::report(full)

car::vif(full)
model.comparison(full, reduced)
car::vif(full)

visualize(full)

flexplot(delta_gsr ~ gsr_std, data=model_data)

testing <- lm(gsr_std ~ zero_cross_std + mfcc6_std, data=model_data)

part_vec <- sample(unique(model_data$participant), 5)

mfcc_gsr_plot <- ggplot(filter(model_data, participant %in% part_vec), 
                        aes(x = mfcc6_std,
                            y= gsr_std)) +
  geom_point(aes(color=participant)) + 
  geom_smooth(data=model_data, method = "lm", se=FALSE, aes(col = "Global Best Fit")) +
  stat_smooth(geom="line", 
              alpha = 0.4, 
              method = "lm", 
              se=FALSE, 
              linewidth=0.5, 
              linetype=2, 
              aes(color=participant)) +
  xlab("") + 
  ylab("") + 
  theme_bw() + 
  theme(legend.position = "none") +
  annotate(geom = "text", label="p < 0.001", x = 2, y = -1.5)
mfcc_gsr_plot
        
zcr_gsr_plot <- ggplot(filter(model_data, participant %in% part_vec), aes(x = zero_cross_std,
                                                                           y = gsr_std)) +
  geom_point(aes(color=participant)) + 
  geom_smooth(data=model_data, method = "lm", se=FALSE, aes(col = "Global Best Fit")) +
  stat_smooth(geom="line", 
              alpha = 0.4, 
              method = "lm", 
              se=FALSE, 
              linewidth=0.5, 
              linetype=2, 
              aes(color=participant)) +
  xlab("") + 
  ylab("ln(Std Dev of GSR)") + 
  theme_bw() + 
  theme(legend.position = "none") +
  annotate(geom = "text", label="p < 0.001", x = 2.5, y = -1.5)
zcr_gsr_plot

ibi_meanzcr <- ggplot(filter(model_data, participant %in% part_vec), aes(x = mfcc6_std,
                                                                           y = mean_ibi)) +
  geom_point(aes(color=participant)) + 
  geom_smooth(data=model_data, method = "lm", se=FALSE, aes(col = "Global Best Fit")) +
  stat_smooth(geom="line", 
              alpha = 0.4, 
              method = "lm", 
              se=FALSE, 
              linewidth=0.5, 
              linetype=2, 
              aes(color=participant)) +
  xlab("Std Dev of MFCC 6") + 
  ylab("") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  annotate(geom = "text", label="p < 0.001", x = 2, y = 1025)
ibi_meanzcr

ibi_stdzcr_plot <- ggplot(filter(model_data, participant %in% part_vec), aes(x = zero_cross_std,
                                                                           y= mean_ibi)) +
  geom_point(aes(color=participant)) + 
  geom_smooth(data=model_data, method = "lm", se=FALSE, aes(col = "Global Best Fit")) +
  stat_smooth(geom="line", 
              alpha = 0.4, 
              method = "lm", 
              se=FALSE, 
              linewidth=0.5, 
              linetype=2, 
              aes(color=participant)) +
  xlab("Std Dev of Zero Crossing Rate") + 
  ylab("Mean Interbeat Interval (ms)") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  annotate(geom = "text", label="p < 0.001", x = 2.5, y = 1025)
ibi_stdzcr_plot

ibi_stdzcr_plot_legend <- ggplot(filter(model_data, participant %in% part_vec), aes(x = zero_cross_std,
                                                                             y= mean_ibi)) +
  geom_point(aes(color=participant)) + 
  geom_smooth(data=model_data, method = "lm", se=FALSE, aes(col = "Global Best Fit")) +
  stat_smooth(geom="line", 
              alpha = 0.4, 
              method = "lm", 
              se=FALSE, 
              linewidth=0.5, 
              linetype=2, 
              aes(color=participant)) +
  labs(color="Participant") +
  xlab("Std Dev of MFCC 6") + 
  ylab("ln(Std Dev of GSR)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

legend_plot <- get_legend(ibi_stdzcr_plot_legend)

title_plot <- ggdraw() + draw_label("Physiological Features Correlated to Audio Features")

plot_grid_1 <- plot_grid(zcr_gsr_plot, mfcc_gsr_plot, ibi_stdzcr_plot, ibi_meanzcr, nrow=2)

plot_grid_2 <- plot_grid(title_plot, plot_grid_1, nrow=2, rel_heights = c(0.05, 0.95))

plot_grid(plot_grid_2, legend_plot, rel_widths = c(0.8, 0.2))

visualize(full, plot="residuals")  + 
  xlab("Std Dev of MFCC6") +
  ylab("ln(Std Dev of GSR)")



?theme
 
?geom_label

  
  geom_smooth(method="lm", se=FALSE) +
  geom_smooth(method="lm", se=FALSE, aes(color=as.factor(participant)))

compare.fits(gsr_std ~ mfcc6_std, data=model_data, full, reduced) + 
  xlab("Std Dev of MFCC 6") + 
  ylab("ln(Std Dev of GSR)")

model.comparison(full, reduced)
report::report(full)

?visualize

model_data$sdnn = log(model_data$sdnn)
flexplot(sdnn ~ 1, data=model_data)

forest_data <- model_data |> 
  group_by(participant) |> 
  mutate(valence = normalize(valence)) |> 
  ungroup() |> 
  select(-c(stim_id, participant))

rf <- cforest(gsr_std ~ ., data=forest_data)
estimates(rf)

global_model = lmer(valence ~ 
                      specs8_mean + 
                      specs9_mean + 
                      specs10_mean + 
                      specs11_mean + 
                      specs12_mean + 
                      specs13_mean + 
                      specs14_mean + 
                      (1|participant),
                    data=model_data,
                    na.action = na.fail, 
                    REML = FALSE)

dredge(global_model)

global_model = lmer(gsr_std ~ 
                      zero_cross_mean +
                      zero_cross_std +	
                      (1|participant),
                    data=model_data,
                    na.action = na.fail)

dredge(global_model)


model

full <- lmer(sdnn ~ tempo_mean + (1|participant), data=model_data)
reduced <- lmer(sdnn ~ 1 + (1|participant), data=model_data)
compare.fits(sdnn ~ tempo_mean, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

flexplot(zero_cross_std ~ mfcc6_std, data=model_data)

global_model = lmer(mean_ibi ~ 
                      valence +
                      arousal +
                      familiarity +
                      liking +
                      dominance +
                      (1|participant),
                    data=model_data,
                    na.action = na.fail)
dredge(global_model)

full <- lmer(mean_ibi ~ dominance + (1|participant), data=model_data)
reduced <- lmer(mean_ibi ~ 1 + (1|participant), data=model_data)
model.comparison(full, reduced)

global_model = lmer(mean_ibi ~ 
                      spec_cent_mean + 
                      spec_cent_std + 
                      spec_bandwidth_mean + 
                      spec_bandwidth_std +
                      spec_contrast_mean +
                      spec_contrast_std +
                      (1|participant),
                    data=model_data,
                    na.action = na.fail)
dredge(global_model)

full <- lmer(mean_ibi ~ spec_bandwidth_mean + spec_cent_mean + spec_cent_std + (1|participant), data=model_data)
reduced <- lmer(mean_ibi ~ mfcc6_std + (1|participant), data=model_data)
model.comparison(full, reduced)

global_model = lmer(mean_ibi ~ 
                    spec_flatness_mean +
                    spec_flatness_std	+ 
                    spec_rolloff_mean +
                    spec_rolloff_std +
                    zero_cross_mean +
                    zero_cross_std +	
                    rms_mean +
                    rms_std	+
                    (1|participant),
                  data=model_data,
                  na.action = na.fail)
dredge(global_model)

global_model = lmer(gsr_std ~ 
                      mfcc1_mean +
                      mfcc1_std + 
                      mfcc2_mean +
                      mfcc2_std + 
                      mfcc3_mean +
                      mfcc3_std + 
                      mfcc4_mean +
                      mfcc4_std + 
                      mfcc5_mean +
                      mfcc5_std + 
                      (1|participant),
                    data=model_data,
                    na.action = na.fail)
dredge(global_model)



flexplot(zero_cross_std ~ zero_cross_std + zero_cross_mean, data=model_data)

full <- lmer(mean_ibi ~ zero_cross_mean + (1|participant), data=model_data)
reduced <- lmer(mean_ibi ~ zero_cross_mean + zero_cross_std + (1|participant), data=model_data)
# compare.fits(gsr_std ~ mfcc1_mean, data=model_data, full, reduced)
model.comparison(full, reduced)

global_model = lmer(mean_ibi ~ 
                      mfcc6_mean +
                      mfcc6_std + 
                      mfcc7_mean +
                      mfcc7_std + 
                      mfcc8_mean +
                      mfcc8_std + 
                      mfcc9_mean +
                      mfcc9_std + 
                      mfcc10_mean +
                      mfcc10_std + 
                      (1|participant),
                    data=model_data,
                    na.action = na.fail)
dredge(global_model)

full <- lmer(gsr_std ~ mfcc6_std + (1|participant), data=model_data)
reduced <- lmer(gsr_std ~ 1 + (1|participant), data=model_data)
compare.fits(gsr_std ~ mfcc10_mean, data=model_data, full, reduced)
model.comparison(full, reduced)

# Model 1: Std dev of Zero Crossing rate as a fixed effect vs data mean
full <- lmer(gsr ~ mfcc6_std + (mfcc6_std|participant), data=model_data)
reduced <- lmer(gsr ~ 1 + (1|participant), data=model_data)
compare.fits(gsr ~ mfcc6_std, data=model_data, full, reduced)

model.comparison(full, reduced)
coef(summary(full))
coef(full)
estimates(full)
?estimates
visualize(full, plot="model")
visualize(full, plot="residuals")

?summary

# Model 1: Std dev of Zero Crossing rate as a fixed effect vs data mean
full <- lm(outcome ~ zero_cross_std, data=forest_data)
reduced <- lm(outcome ~ 1, data=forest_data)
compare.fits(outcome ~ zero_cross_std, data=forest_data, full, reduced)
model.comparison(full, reduced)
visualize(full, plot="model")

?flexplot



?compare.fits

# Create Forest data
forest_data <- reduced_physio |>
  left_join(mir_formatted, by = "stim_id") |>
  left_join(ratings_formatted, by = c("participant" = "participant",
                                      "stim_id" = "stim_id"))

# Normalize participant ratings of stimuli
normalized_data <- forest_data |>
  group_by(participant) |>
  mutate(valence = normalize(valence),
         arousal = normalize(arousal),
         dominance = normalize(dominance),
         liking = normalize(liking),
         familiarity = normalize(familiarity),
         outcome = normalize(outcome))
# 
# flexplot(outcome ~ 1, data=normalized_data)
# 
# sm_data <- normalized_data |>
#   group_by(stim_id) |> 
#   mutate(outcome = mean(outcome),
#          valence = mean(valence),
#          arousal = mean(arousal),
#          dominance = mean(dominance),
#          liking = mean(liking),
#          familiarity = mean(familiarity)) |> 
#   ungroup() |> 
#   mutate(outcome <= 2) |> 
#   select(-participant, -stim_id) |> 
#   slice(1:40)
# 
# flexplot(outcome ~ 1, data=sm_data)
#   
# forest <- cforest(outcome ~ ., data=sm_data)
# estimates(forest)

reduced_physio <- reduced |> 
  mutate(outcome = log(outcome)) |> 
  mutate(outcome = normalize(outcome)) |> 
  filter(outcome <= 3,
         outcome >= -3)

flexplot(outcome ~ 1, data=reduced_physio)

# Create Model data
model_data <- reduced_physio |> 
  left_join(mir_formatted, by = "stim_id") |> 
  left_join(ratings_formatted, by = c("participant" = "participant",
                                      "stim_id" = "stim_id"))

global_model = lm(outcome ~ 
                      valence +
                      arousal +
                      familiarity + 
                      liking + 
                      dominance + 
                      mfcc6_std +
                      zero_cross_std,
                    data=sm_data,
                    na.action = na.fail)

dredge(global_model)

# # Model 0: testing
# full <- lmer(outcome ~ valence + (1|participant), data = model_data)
# reduced <- lmer(outcome ~ 1 + (1|participant), data = model_data)
# compare.fits(outcome ~ valence, data=model_data, full, reduced)
# model.comparison(full, reduced)
# visualize(full)

# Model 1: Std dev of Zero Crossing rate as a fixed effect vs data mean
full <- lmer(outcome ~ zero_cross_std + (1|participant), data=model_data)
reduced <- lmer(outcome ~ 1 + (1|participant), data=model_data)
compare.fits(outcome ~ zero_cross_std, data=model_data, full, reduced)
model.comparison(full, reduced)
visualize(full)

# Model 2: Std dev of Zero Crossing rate as fixed and random effect vs only as fixed effect (ie fixed slopes vs variable slopes)
full <- lmer(outcome ~ zero_cross_std + mfcc6_std + (1|participant), data = model_data)
reduced <- lmer(outcome ~ mfcc6_std + (1|participant), data = model_data)
compare.fits(outcome ~ zero_cross_std | mfcc6_std, data=model_data, full, reduced)
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
