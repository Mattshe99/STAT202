library(tidyverse)
library(performance)
data <- read_csv("job_satisfaction1.csv") %>% mutate(education_level = 
                                                       factor(education_level, levels = c("school", "college", "university")))
set.seed(16645573)
my_js <- data %>% sample_n(104)
my_js %>% group_by(education_level) %>% summarise(count = n(), mean_score = 
                                                    mean(score))
my_js %>% ggplot(aes(x = education_level, y = score)) +
  geom_boxplot() +
  labs(
    title = "Job satisfaction by education level",
    x = "Education level",
    y = "Job satisfaction score"
  )

mod_js <- lm(score ~ education_level, data = my_js)
summary(mod_js)

check_model(mod_js, check = c(
  "linearity",
  "homogeneity",
  "qq",
  "normality"
))

check_homogeneity(mod_js)
check_normality(mod_js)

anova(mod_js)

TukeyHSD(aov(mod_js))

set.seed(16645573)
my_plants <- PlantGrowth %>% sample_n(25)

my_plants %>%
  group_by(group) %>%
  summarise(count = n(), mean_weight = mean(weight))

my_plants %>% 
  ggplot(aes(x = group, y = weight)) +
  geom_jitter(width = 0.1) +
  labs(
    x = "Group",
    y = "Weight (g)"
  )

plants_model <- lm(weight ~ group, data = my_plants)
anova(plants_model)
TukeyHSD(aov(plants_model))