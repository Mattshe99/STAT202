library(tidyverse)
library(performance)
library(GGally)

set.seed(16645573)
mammals <- read_csv('endocranial_volume.csv')
my_mammals <- mammals %>% sample_n(490)

ggpairs(my_mammals, columns = c("height_mm", "width_mm", "bs_mm", "mass_g"))

m1 <- lm(mass_g ~ height_mm, data = my_mammals)
summary(m1)
m2 <- lm(mass_g ~ height_mm + width_mm, data = my_mammals)
summary(m2)
m3 <- lm(mass_g ~ height_mm + width_mm + bs_mm , data = my_mammals)
summary(m3)

check_model(m2)

my_mammals_log <- my_mammals %>% mutate_if(is.numeric, log)

ggpairs(my_mammals_log, columns = c("height_mm", "width_mm", "bs_mm", "mass_g"))

m4 <- lm(mass_g ~ height_mm, data = my_mammals_log)
summary(m4)
m5 <- lm(mass_g ~ height_mm + width_mm, data = my_mammals_log)
summary(m5)
m6 <- lm(mass_g ~ height_mm + width_mm + bs_mm , data = my_mammals_log)
summary(m6)

check_model(m5)

X <- model.matrix(m5)
y <- my_mammals_log$mass_g
XtX <- t(X) %*% X
Xty <- t(X) %*% y
b <- solve(XtX) %*% Xty

b