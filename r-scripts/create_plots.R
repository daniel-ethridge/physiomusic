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
library(report)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

new_parts = F

# Import model data
model_data <- read_csv("../data/deap/features/model_data.csv") |> 
  mutate(participant = as.factor(participant))

# Write functions
partial_gsr <- function(mod_data, idx) {
  part_data <- model_data |> 
    filter(participant == idx)
  
  return(lm(gsr_std ~ mfcc6_std + zero_cross_std, data=part_data))
}

partial_ibi <- function(mod_data, idx) {
  part_data <- model_data |> 
    filter(participant == idx)
  
  return(lm(mean_ibi ~ mfcc6_std + zero_cross_std, data=part_data))
}

# Create models
gsr_full <- lmer(gsr_std ~ zero_cross_std + mfcc6_std + 
                   (zero_cross_std|participant) + (mfcc6_std|participant),
                 data=model_data)

resids <- visualize(gsr_full, plot="residuals")
resids_title <- ggdraw() + draw_label("Residual plots Model 1", size = 20)
cowplot::plot_grid(resids_title, resids, nrow = 2, rel_heights = c(0.05, 0.95))

ibi_full <- lmer(mean_ibi ~ zero_cross_std + mfcc6_std + (1|participant),
                 data=model_data)

# Create partial regressions
df_gsr_mfcc <- predict_response(gsr_full, c("mfcc6_std", "participant"))
df_gsr_zcr <- predict_response(gsr_full, c("zero_cross_std", "participant"))
df_ibi_mfcc <- predict_response(ibi_full, c("mfcc6_std", "participant"))
df_ibi_zcr <- predict_response(ibi_full, c("zero_cross_std", "participant"))

# Partial participant vector
# if (new_parts) {
#   part_vec <- sort(sample(unique(model_data$participant), 5))
# }
part_vec <- c(13, 15, 19, 24, 30)

# Create individual participant partial regressions
p1_gsr_mfcc <- predict_response(partial_gsr(model_data, part_vec[1]), c("mfcc6_std"))
p2_gsr_mfcc <- predict_response(partial_gsr(model_data, part_vec[2]), c("mfcc6_std"))
p3_gsr_mfcc <- predict_response(partial_gsr(model_data, part_vec[3]), c("mfcc6_std"))
p4_gsr_mfcc <- predict_response(partial_gsr(model_data, part_vec[4]), c("mfcc6_std"))
p5_gsr_mfcc <- predict_response(partial_gsr(model_data, part_vec[5]), c("mfcc6_std"))

# Create individual participant partial regressions
p1_gsr_zcr <- predict_response(partial_gsr(model_data, part_vec[1]), c("zero_cross_std"))
p2_gsr_zcr <- predict_response(partial_gsr(model_data, part_vec[2]), c("zero_cross_std"))
p3_gsr_zcr <- predict_response(partial_gsr(model_data, part_vec[3]), c("zero_cross_std"))
p4_gsr_zcr <- predict_response(partial_gsr(model_data, part_vec[4]), c("zero_cross_std"))
p5_gsr_zcr <- predict_response(partial_gsr(model_data, part_vec[5]), c("zero_cross_std"))

p1_ibi_mfcc <- predict_response(partial_ibi(model_data, part_vec[1]), c("mfcc6_std"))
p2_ibi_mfcc <- predict_response(partial_ibi(model_data, part_vec[2]), c("mfcc6_std"))
p3_ibi_mfcc <- predict_response(partial_ibi(model_data, part_vec[3]), c("mfcc6_std"))
p4_ibi_mfcc <- predict_response(partial_ibi(model_data, part_vec[4]), c("mfcc6_std"))
p5_ibi_mfcc <- predict_response(partial_ibi(model_data, part_vec[5]), c("mfcc6_std"))

p1_ibi_zcr <- predict_response(partial_ibi(model_data, part_vec[1]), c("zero_cross_std"))
p2_ibi_zcr <- predict_response(partial_ibi(model_data, part_vec[2]), c("zero_cross_std"))
p3_ibi_zcr <- predict_response(partial_ibi(model_data, part_vec[3]), c("zero_cross_std"))
p4_ibi_zcr <- predict_response(partial_ibi(model_data, part_vec[4]), c("zero_cross_std"))
p5_ibi_zcr <- predict_response(partial_ibi(model_data, part_vec[5]), c("zero_cross_std"))

partial_effects <- bind_rows(lapply(1:length(unique(model_data$participant)), function(i){
  part_data <- filter(model_data, participant == as.character(i))
  if(nrow(part_data) > 0){
    curr_model <- lm(gsr_std ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- predict_response(curr_model, c("mfcc6_std"))
    slope <- (curr_response[2,2] - curr_response[1,2])/(curr_response[2,1] - curr_response[1,1])
    data.frame(participant = as.character(i), slope = as.character(slope))
  } else {
    data.frame(participant = NA, slope = NA)
  }
})) %>%
  mutate(slope = as.numeric(slope))

summary(partial_effects)

ggplot(data = partial_effects, aes(x = slope)) +
  geom_histogram() +
  labs(title = "Partial Effects of σ(MFCC 6) on ln(σ(GSR)) by Participant",
       x = "Effect",
       y = "Count") +
  theme_bw() +
  # theme(panel.grid = element_blank())
  theme(axis.title = element_text(size=16, hjust=0.5),
        title = element_text(size=20),
        axis.text = element_text(size=14))

facet_data <- bind_rows(lapply(unique(model_data$participant), function(i){
  part_data <- filter(model_data, participant == i)
  if(nrow(part_data) > 0){
    curr_model <- lm(gsr_std ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- as.data.frame(predict_response(curr_model, c("mfcc6_std"))) %>%
      select(c(x, predicted)) %>%
      mutate(participant = i,
             x = as.character(x),
             predicted = as.character(predicted))
  } else {
    data.frame(participant = NA, x = NA, predicted = NA)
  }
})) %>%
  mutate(x = as.numeric(x),
         predicted = as.numeric(predicted))

sjPlot::


mutate(participant = factor(participant,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                          11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                          21, 22, 24, 25, 26, 27, 29, 30)))

# ggplot() +
#   geom_point(data=model_data, aes(x = mfcc6_std, y = gsr_std), size = 1, alpha = 0.3) +
#   geom_line(data=facet_data, aes(x = x, y =predicted), col = "blue") +
#   facet_wrap(~participant, ncol = 7) +
#   theme_bw() +
#   # theme(panel.grid = element_blank())
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size=16, hjust=0.5), 
#         title = element_text(size=20),
#         axis.text = element_text(size=14))





# ### Plot for Mean IBI ~ MFCC 6 partial regression ### #
ibi_mfcc_plot <- ggplot() +
  geom_point(
    data=filter(model_data, participant %in% part_vec), 
    aes(x = mfcc6_std, 
        y = mean_ibi, 
        color = participant)) + 
  scale_color_manual(values=c("blue", "orange", "red", "purple", "green", "black")) + 
  geom_line(data=df_ibi_mfcc, aes(x=x, y=predicted), linewidth=2) +
  geom_line(data=p1_ibi_mfcc, aes(x=x, y=predicted), color="blue") +
  geom_line(data=p2_ibi_mfcc, aes(x=x, y=predicted), color="orange") +
  geom_line(data=p3_ibi_mfcc, aes(x=x, y=predicted), color="red") + 
  geom_line(data=p4_ibi_mfcc, aes(x=x, y=predicted), color="purple") + 
  geom_line(data=p5_ibi_mfcc, aes(x=x, y=predicted), color="green") + 
  xlab("") +
  ylab("Mean Interbeat Interval (ms)") +
  theme_bw() +
  theme(legend.position = "none") +
  annotate(geom = "text", label="p = 0.013, β = -2.66", x = 1.6, y = 1050, size=5) + 
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size = 16))


# ### Plot for Mean IBI ~ ZCR partial regression ### #
ibi_zcr_plot <- ggplot() +
  geom_point(
    data=filter(model_data, participant %in% part_vec), 
    aes(x = zero_cross_std, 
        y = mean_ibi, 
        color = participant)) + 
  scale_color_manual(values=c("blue", "orange", "red", "purple", "green", "black")) + 
  geom_line(data=df_ibi_zcr, aes(x=x, y=predicted, col="Global Best Fit"), linewidth=2) +
  geom_line(data=p1_ibi_zcr, aes(x=x, y=predicted), color="blue") +
  geom_line(data=p2_ibi_zcr, aes(x=x, y=predicted), color="orange") +
  geom_line(data=p3_ibi_zcr, aes(x=x, y=predicted), color="red") + 
  geom_line(data=p4_ibi_zcr, aes(x=x, y=predicted), color="purple") + 
  geom_line(data=p5_ibi_zcr, aes(x=x, y=predicted), color="green") + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") + 
  annotate(geom = "text", label="p = < 0.001, β = -3.74", x = 2.1, y = 1050, size=5) + 
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size = 16))

# ### Plot for GSR ~ ZCR partial regression ### #
gsr_zcr_plot <- ggplot() +
  geom_point(
    data=filter(model_data, participant %in% part_vec), 
    aes(x = zero_cross_std, 
        y = gsr_std, 
        color = participant)) + 
  scale_color_manual(values=c("blue", "orange", "red", "purple", "green", "black")) + 
  geom_line(data=df_gsr_zcr, aes(x=x, y=predicted), linewidth=2) +
  geom_line(data=p1_gsr_zcr, aes(x=x, y=predicted), color="blue") +
  geom_line(data=p2_gsr_zcr, aes(x=x, y=predicted), color="orange") +
  geom_line(data=p3_gsr_zcr, aes(x=x, y=predicted), color="red") + 
  geom_line(data=p4_gsr_zcr, aes(x=x, y=predicted), color="purple") + 
  geom_line(data=p5_gsr_zcr, aes(x=x, y=predicted), color="green") + 
  xlab("Standard Deviation of Zero Crossing Rate") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") + 
  annotate(geom = "text", label="p = 0.316, β = 0.04", x = 2.2, y = -1.5, size=5) + 
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size = 16))
gsr_zcr_plot

# ### Plot for GSR ~ MFCC 6 partial regression ### #
gsr_mfcc_plot <- ggplot() +
  geom_point(
    data=filter(model_data, participant %in% part_vec), 
    aes(x = mfcc6_std, 
        y = gsr_std, 
        color = participant)) + 
  scale_color_manual(values=c("blue", "orange", "red", "purple", "green", "black")) + 
  geom_line(data=df_gsr_mfcc, aes(x=x, y=predicted), linewidth=2) +
  geom_line(data=p1_gsr_mfcc, aes(x=x, y=predicted), color="blue") +
  geom_line(data=p2_gsr_mfcc, aes(x=x, y=predicted), color="orange") +
  geom_line(data=p3_gsr_mfcc, aes(x=x, y=predicted), color="red") + 
  geom_line(data=p4_gsr_mfcc, aes(x=x, y=predicted), color="purple") + 
  geom_line(data=p5_gsr_mfcc, aes(x=x, y=predicted), color="green") + 
  xlab("Standard Deviation of MFCC 6") +
  ylab("ln(Std Dev of GSR)") +
  theme_bw() +
  theme(legend.position = "none") + 
  annotate(geom = "text", label="p = 0.021, β = 0.12", x = 1.7, y = -1.5, size=5) + 
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size = 16))

# ### Legend Plot ### #
legend_plot <- ggplot() +
  geom_point(
    data=filter(model_data, participant %in% part_vec), 
    aes(x = zero_cross_std, 
        y = mean_ibi, 
        color = participant)) + 
  scale_color_manual(values=c("blue", "orange", "red", "purple", "green", "black")) + 
  geom_line(data=df_ibi_zcr, aes(x=x, y=predicted, col="Global Best Fit"), linewidth=2) +
  geom_line(data=p1_ibi_zcr, aes(x=x, y=predicted), color="blue") +
  geom_line(data=p2_ibi_zcr, aes(x=x, y=predicted), color="orange") +
  geom_line(data=p3_ibi_zcr, aes(x=x, y=predicted), color="red") + 
  geom_line(data=p4_ibi_zcr, aes(x=x, y=predicted), color="purple") + 
  geom_line(data=p5_ibi_zcr, aes(x=x, y=predicted), color="green") + 
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size=16))

# Take legend from ibi_zcr_plot
legend_p <- get_legend(legend_plot)

title_plot <- ggdraw() + 
  draw_label("Partial Regressions of Physiological and Audio Features", size = 20)

plots <- cowplot::plot_grid(ibi_mfcc_plot, ibi_zcr_plot, gsr_mfcc_plot, gsr_zcr_plot, nrow=2)
plots_w_title <- cowplot::plot_grid(title_plot, plots, nrow=2, rel_heights = c(0.1, 0.9))
cowplot::plot_grid(plots_w_title, legend_p, ncol=2, rel_widths=c(0.85, 0.15))

# ### PARTIAL EFFECTS ### #
# # IBI - MFCC # #
partial_effects_ibi_mfcc <- bind_rows(lapply(1:length(unique(model_data$participant)), function(i){
  part_data <- filter(model_data, participant == as.character(i))
  if(nrow(part_data) > 0){
    curr_model <- lm(mean_ibi ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- predict_response(curr_model, c("mfcc6_std"))
    slope <- (curr_response[2,2] - curr_response[1,2])/(curr_response[2,1] - curr_response[1,1])
    data.frame(participant = as.character(i), slope = as.character(slope))
  } else {
    data.frame(participant = NA, slope = NA)
  }
})) %>%
  mutate(slope = as.numeric(slope))

partial_ibi_mfcc <- ggplot(data = partial_effects_ibi_mfcc, aes(x = slope)) +
  geom_histogram() +
  labs(x = "Estimates for Mean IBI ~ σ(MFCC 6)", y = "Count") +
  theme_bw() +
  # theme(panel.grid = element_blank())
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size=16))
partial_ibi_mfcc

# # IBI - ZCR # #
partial_effects_ibi_zcr <- bind_rows(lapply(1:length(unique(model_data$participant)), function(i){
  part_data <- filter(model_data, participant == as.character(i))
  if(nrow(part_data) > 0){
    curr_model <- lm(mean_ibi ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- predict_response(curr_model, c("zero_cross_std"))
    slope <- (curr_response[2,2] - curr_response[1,2])/(curr_response[2,1] - curr_response[1,1])
    data.frame(participant = as.character(i), slope = as.character(slope))
  } else {
    data.frame(participant = NA, slope = NA)
  }
})) %>%
  mutate(slope = as.numeric(slope))

partial_ibi_zcr <- ggplot(data = partial_effects_ibi_zcr, aes(x = slope)) +
  geom_histogram() +
  labs(x = "Estimates for Mean IBI ~ σ(ZCR)", y = "") +
  theme_bw() +
  # theme(panel.grid = element_blank())
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size=16))
partial_ibi_zcr

# # GSR - MFCC # #
partial_effects_gsr_mfcc <- bind_rows(lapply(1:length(unique(model_data$participant)), function(i){
  part_data <- filter(model_data, participant == as.character(i))
  if(nrow(part_data) > 0){
    curr_model <- lm(gsr_std ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- predict_response(curr_model, c("mfcc6_std"))
    slope <- (curr_response[2,2] - curr_response[1,2])/(curr_response[2,1] - curr_response[1,1])
    data.frame(participant = as.character(i), slope = as.character(slope))
  } else {
    data.frame(participant = NA, slope = NA)
  }
})) %>%
  mutate(slope = as.numeric(slope))

partial_gsr_mfcc <- ggplot(data = partial_effects_gsr_mfcc, aes(x = slope)) +
  geom_histogram() +
  labs(x = "Estimates for ln(σ(GSR)) ~ σ(MFCC 6)", y = "Count") +
  theme_bw() +
  # theme(panel.grid = element_blank())
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size=16))
partial_gsr_mfcc

# # GSR - ZCR # #
partial_effects_gsr_zcr <- bind_rows(lapply(1:length(unique(model_data$participant)), function(i){
  part_data <- filter(model_data, participant == as.character(i))
  if(nrow(part_data) > 0){
    curr_model <- lm(gsr_std ~ mfcc6_std + zero_cross_std, data=part_data)
    curr_response <- predict_response(curr_model, c("zero_cross_std"))
    slope <- (curr_response[2,2] - curr_response[1,2])/(curr_response[2,1] - curr_response[1,1])
    data.frame(participant = as.character(i), slope = as.character(slope))
  } else {
    data.frame(participant = NA, slope = NA)
  }
})) %>%
  mutate(slope = as.numeric(slope))

partial_gsr_zcr <- ggplot(data = partial_effects_gsr_zcr, aes(x = slope)) +
  geom_histogram() +
  labs(x = "Estimates for ln(σ(GSR)) ~ σ(ZCR)", y = "") +
  theme_bw() +
  # theme(panel.grid = element_blank())
  theme(axis.title = element_text(size=20, hjust=0.5),
        axis.text = element_text(size=16))
partial_gsr_zcr

plot_1 <-cowplot::plot_grid(partial_ibi_mfcc, partial_ibi_zcr, partial_gsr_mfcc, partial_gsr_zcr, nrow=2)

partial_title <- ggdraw() + 
  draw_label("Partial Regressions for Individual Participants", size = 20)

cowplot::plot_grid(partial_title, plot_1, ncol=1, rel_heights = c(0.05, 0.95))




  
