# load required packages
library(tidyverse)
library(here)
library(ggridges)

# set working directory properly
here::i_am("game_play_analysis.R")

# run required scripts
source(here("erez.R"))
source(here("party.stance.R"))

# read in data containing game play information
load(here("data_files", "data", "aff_pol_df_analy_jan2023.RData"))

# get more information on party stance
dffin <- enrich.party.stance(dffin)

# data from people with no religion is corrupted
dffin <- filter(dffin, !is.na(rel_belief))

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
            trust_tokens_given = mean(pl2gets))

# get summary statistics from the dictator game
dict_summary <- dict_data %>%
  group_by(PID) %>%
  summarise(num_rounds = n(),
            dict_tokens_given = mean(pl2gets))

# combine the two datasets
combined <- inner_join(trust_summary, dict_summary, by = "PID")

# make scatterplot of relationship
combined %>%
  ggplot(aes(trust_tokens_given, dict_tokens_given)) +
  geom_point(alpha = 0.025, color = "red") +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Trust Game",
       y = "Dictator Game") +
  theme_bw()

# use ggridges to get density plots for each binned trust average
rounded_trust_combined <- combined %>%
  mutate(trust_tokens_given = round(trust_tokens_given, 0))

rounded_trust_combined %>%
  ggplot(aes(x = dict_tokens_given, y = trust_tokens_given,
             fill = as.factor(trust_tokens_given))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Dictator Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

rounded_trust_combined %>%
  inner_join(trust_data %>% select(PID, class,rel_belief), by = "PID") %>%
  ggplot(aes(x = dict_tokens_given, y = trust_tokens_given,
             fill = as.factor(trust_tokens_given))) +
  geom_density_ridges() +
  labs(title = "Trust Game vs. Dictator Game",
       x = "Trust Game",
       y = "Trust Game") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game") +
  facet_wrap(~rel_belief)

# ggridges trust game by political party
combined %>% inner_join(trust_data %>% select(PID, lrpos), by = "PID") %>%
  ggplot(aes(x = trust_tokens_given, y = as.factor(lrpos), fill = as.factor(lrpos))) +
  geom_density_ridges() +
  labs(title = "Trust Game by Political Party",
       x = "Trust Game",
       y = "Political Party") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

# ggridges dictator game by political party
combined %>% inner_join(dict_data %>% select(PID, lrpos), by = "PID") %>%
  ggplot(aes(x = dict_tokens_given, y = as.factor(lrpos), fill = as.factor(lrpos))) +
  geom_density_ridges() +
  labs(title = "Dictator Game by Political Party",
       x = "Dictator Game",
       y = "Political Party") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Dictator Game")

# ggridges trust game by religion
combined %>% inner_join(trust_data %>% select(PID, rel_belief), by = "PID") %>%
  ggplot(aes(x = trust_tokens_given, y = as.factor(rel_belief),
             fill = as.factor(rel_belief))) +
  geom_density_ridges() +
  labs(title = "Trust Game by Religion",
       x = "Trust Game",
       y = "Religion") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

# ggridges dictator game by religion
combined %>% inner_join(dict_data %>% select(PID, rel_belief), by = "PID") %>%
  ggplot(aes(x = dict_tokens_given, y = as.factor(rel_belief),
             fill = as.factor(rel_belief))) +
  geom_density_ridges() +
  labs(title = "Dictator Game by Religion",
       x = "Dictator Game",
       y = "Religion") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Dictator Game")

# ggridges trust game by political when religion does not align
trust_data %>%
  ggplot(aes(x = pl2gets)) +
  geom_density() +
  facet_wrap(~copartisan) +
  labs(title = "Trust Game by Religion",
       x = "Trust Game",
       y = "Religion") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "# of Tokens Given In Trust Game")

# make confidence intervals for above t tests in difference of pl2gets
trust_partisan <- t.test(filter(trust_data, copartisan == "1_partisan_outpartisan")$pl2gets,
       filter(trust_data, copartisan == "2_partisan_copartisan")$pl2gets)$conf.int

dict_partisan <- t.test(filter(dict_data, copartisan == "1_partisan_outpartisan")$pl2gets,
       filter(dict_data, copartisan == "2_partisan_copartisan")$pl2gets)$conf.int

trust_national <- t.test(filter(trust_data, nationality != "co-national")$pl2gets,
       filter(trust_data, nationality == "co-national")$pl2gets)$conf.int

dict_national <- t.test(filter(dict_data, nationality != "co-national")$pl2gets,
       filter(dict_data, nationality == "co-national")$pl2gets)$conf.int

trust_age <- t.test(filter(trust_data, sameage == "0_diff")$pl2gets,
       filter(trust_data, sameage == "1_same")$pl2gets)$conf.int

dict_age <- t.test(filter(dict_data, sameage == "0_diff")$pl2gets,
       filter(dict_data, sameage == "1_same")$pl2gets)$conf.int

trust_sex <- t.test(filter(trust_data, samesex == "0_diff")$pl2gets,
       filter(trust_data, samesex == "1_same")$pl2gets)$conf.int

dict_sex <- t.test(filter(dict_data, samesex == "0_diff")$pl2gets,
       filter(dict_data, samesex == "1_same")$pl2gets)$conf.int

trust_class <- t.test(filter(trust_data, sameclass == "0_diff")$pl2gets,
       filter(trust_data, sameclass == "1_same")$pl2gets)$conf.int

dict_class <- t.test(filter(dict_data, sameclass == "0_diff")$pl2gets,
       filter(dict_data, sameclass == "1_same")$pl2gets)$conf.int

trust_religion <- t.test(filter(trust_data, samereligion == "0_diff")$pl2gets,
       filter(trust_data, samereligion == "1_same")$pl2gets)$conf.int

dict_religion <- t.test(filter(dict_data, samereligion == "0_diff")$pl2gets,
       filter(dict_data, samereligion == "1_same")$pl2gets)$conf.int

interval_df <- data.frame(game = c(rep("Trust Game", 6),rep("Dictator Game",6)),
                          group = rep(c("Gender", "Age", "Class", "Religion",
                                        "Nationality", "Party"), 2),
                          lower = c(trust_sex[1], trust_age[1], trust_class[1],
                                    trust_religion[1], trust_national[1],
                                    trust_partisan[1], dict_sex[1],
                                    dict_age[1], dict_class[1],
                                    dict_religion[1], dict_national[1],
                                    dict_partisan[1]),
                          upper = c(trust_sex[2], trust_age[2], trust_class[2],
                                    trust_religion[2], trust_national[2],
                                    trust_partisan[2], dict_sex[2],
                                    dict_age[2], dict_class[2],
                                    dict_religion[2], dict_national[2],
                                    dict_partisan[2]))
interval_df$group <- factor(interval_df$group, levels = rev(c("Gender", "Age",
                                                        "Class", "Religion",
                                                      "Nationality", "Party")))

# plot all confidence intervals for trust game in ggplot
ggplot(interval_df, aes(y = group, x = (lower + upper) / 2)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) +
  geom_point(col = "navy") +
  facet_wrap(~game) +
  labs(x = "(Out-Group Tokens Given) - (In-Group Tokens Given)",
       y = "Demographic of Interest",
       title = "Effects of Demographic Differences on Token Allocation") +
  coord_cartesian(xlim = c(-1.1, 1.1)) +
  # name the titles of each facet "Dictator Game" and "Trust Game"
  theme_bw()

