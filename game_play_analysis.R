# load required packages
library(tidyverse)
library(here)
library(ggridges)

# set working directory properly
here::i_am("game_play_analysis.R")

# read in data containing game play information
load(here("data_files", "data", "stacked_raw_data.RData"))

# get data from the trust game
trust_data <- dffin %>%
  filter(game == "trust")

# get data from the dictator game
dict_data <- dffin %>%
  filter(game == "dict")

# get summary statistics from the trust game
trust_summary <- trust_data %>%
  group_by(PID) %>%
  summarise(num_rounds = n(),
            mean_play = mean(pl2gets))

# get summary statistics from the dictator game
dict_summary <- dict_data %>%
  group_by(PID) %>%
  summarise(num_rounds = n(),
            mean_play = mean(pl2gets))

# combine the two datasets
combined <- inner_join(trust_summary, dict_summary, by = "PID")

# make scatterplot of relationship
combined %>%
  ggplot(aes(mean_play.x, mean_play.y)) +
  geom_point(alpha = 0.025, color = "red") +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Trust Game",
       y = "Dictator Game") +
  theme_bw()

# use ggridges to get density plots for each binned trust average
rounded_trust_combined <- combined %>%
  mutate(mean_play.x = round(mean_play.x, 0))

rounded_trust_combined %>%
  ggplot(aes(x = mean_play.y, y = mean_play.x, fill = as.factor(mean_play.x))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Dictator Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

rounded_trust_combined %>%
  ggplot(aes(x = mean_play.y, y = mean_play.x, fill = as.factor(mean_play.x))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Dictator Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

rounded_trust_combined %>% inner_join(trust_data %>% select(PID, nationality, samereligion), by = "PID") %>%
  ggplot(aes(x = mean_play.y, y = mean_play.x, fill = as.factor(mean_play.x))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Trust Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game") +
  facet_wrap(~nationality)

rounded_trust_combined %>% inner_join(trust_data %>% select(PID, class, rel_belief), by = "PID") %>%
  ggplot(aes(x = mean_play.y, y = mean_play.x, fill = as.factor(mean_play.x))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Trust Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game") +
  facet_wrap(~class)

