# Baseball linear regression

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


# Scatterplot of the relationship between stolen bases and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


# Scatterplot of the relationship between bases on balls and runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of the relationship between At bats per game and runs

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of the relationship between Wins per game and Fiedling errors (E)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Wins_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(E_per_game, Wins_per_game)) + 
  geom_point(alpha = 0.5)

# Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Tri_per_game = X3B/G, Dou_per_game = X2B/G) %>%
  ggplot(aes(Dou_per_game, Tri_per_game)) + 
  geom_point(alpha = 0.5)

