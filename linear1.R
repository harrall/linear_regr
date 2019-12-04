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

# Correlation Key points

# Galton tried to predict sons' heights based on fathers' heights.
# The mean and standard errors are insufficient for describing an important characteristic of the data: the trend that the taller the father, the taller the son.
# The correlation coefficient is an informative summary of how two variables move together that can be used to predict one variable using the other.

# Code

# create the dataset
library(tidyverse)
install.packages("HistData")
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# correlation coefficient
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# Sample Correlation is a Random Variable

# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R
            
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

library(Lahman)
teamsf <- Teams %>% filter(yearID %in% 1961:2001)
teamsf %>% mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% pull(r)
teamsf %>% mutate(Wins_per_game = W/G, E_per_game = E/G) %>%
  summarize(r = cor(Wins_per_game, E_per_game)) %>% pull(r)
teamsf %>% mutate(Tri_per_game = X3B/G, Dou_per_game = X2B/G) %>%
  summarize(r = cor(Tri_per_game, Dou_per_game)) %>% pull(r)


# Regression Code

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)


