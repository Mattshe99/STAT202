library(tidyverse)
library(performance)

eucalyptus <- read_csv('euc_tricarpa.csv')
set.seed(16645573)
my_eucalyptus <- eucalyptus %>% sample_n(900)

ggplot(my_eucalyptus, aes(x= acoustic_velocity, y = MOE)) + geom_point()

model_1 <- lm(MOE ~ acoustic_velocity, data = my_eucalyptus)
summary(model_1)

my_eucalyptus <- my_eucalyptus %>% mutate(cent_velocity = acoustic_velocity -
                                            mean(acoustic_velocity))
model_2 <- lm(MOE ~ cent_velocity, data = my_eucalyptus)
summary(model_2)

check_model(model_2)

cineole <- read_csv('cineole.csv')
set.seed(16645573)
my_cineole <- cineole %>% sample_n(500)

ggplot(my_cineole, aes(x= month_number, y = cineole)) + geom_point(alpha = 0.5)

my_cineole <- my_cineole %>% mutate(new_month = month_number / 12)
ggplot(my_cineole, aes(x= new_month, y = cineole)) + geom_point(alpha = 0.5)

oil_1 <- lm(cineole ~ new_month, data = my_cineole)
check_model(oil_1)

oil_2 <- lm(cineole ~ (sin(2*pi*new_month) + cos(2*pi*new_month)), data = my_cineole)
summary(oil_2)

check_model(oil_2)