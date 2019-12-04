# assessment section 1

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>%    
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight) 

# Calculate the mean and standard deviation of mothers' heights, 
# the mean and standard deviation of daughters' heights, 
# and the correlaton coefficient between mother and daughter heights.

female_heights %>%
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))

female_heights %>% summarize(r = cor(mother, daughter)) %>% pull(r)

# Calculate the slope and intercept of the regression line predicting 
# daughters' heights given mothers' heights. Given an increase in mother's 
# height by 1 inch, how many inches is the daughter's height expected to change?

mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x  # slope
m
b <- mu_y - m*mu_x  # intercept
b
pred_change <- r * s_y / s_x
pred_change

r
explain <- r^2 * 100
explain

exp <- b + (60 * m)
exp

m
b



