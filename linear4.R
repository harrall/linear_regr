# assessment section 2
# q1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#q3
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# Scatterplot of the relationship between HRs and wins
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G, BB_per_game = BB / G)

fit <- lm(R_per_game ~ HR_per_game + BB_per_game, data = Teams)
fit

# q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# q5
mod <- lm(son ~ father, data = galton_heights)
summary(mod)

#q6
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#q7 
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights 
# using daughters' heights. 
fit <- lm(mother ~ daughter, data = female_heights)
fit

a <- predict(fit)

#q9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

avg <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>% 
  ungroup() %>% 
  select(playerID, mean_singles, mean_bb)

avg %>%
  filter(mean_singles > 0.2) %>% nrow()
avg %>%
  filter(mean_bb > 0.2) %>% nrow()

#q10
both <- inner_join(bat_02, avg)
both %>% summarize(cor(singles, mean_singles))
both %>% summarize(cor(bb, mean_bb))

#q11
both %>% ggplot(aes(mean_singles, singles)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

both %>% ggplot(aes(mean_bb, bb)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

#q12
# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
fit <- lm(singles ~ mean_singles, data = both)
fit

fit <- lm(bb ~ mean_bb, data = both)
fit
























