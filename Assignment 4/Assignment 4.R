library(tidyverse)
library(performance)
kungsan_data <- read_csv("http://stats.apiolaza.net/data/kungsan_full.csv")

set.seed(16645573)
my_kungsan <- sample_n(kungsan_data, 525)
my_kungsan <- my_kungsan %>% mutate(weight2 = weight^2, sex = factor(sex))

library(GGally)
ggpairs(my_kungsan, columns = c("weight", "weight2", "sex", "height"))

m1 <- lm(height ~ weight, data = my_kungsan)
m2 <- lm(height ~ weight + weight2, data = my_kungsan)
m3 <- lm(height ~ weight + weight2 + sex, data = my_kungsan)

check_collinearity(m2)
check_collinearity(m3)

check_model(m1)
check_model(m2)
check_model(m3)

my_kungsan <- my_kungsan %>% mutate(weight_c = weight - mean(weight), weight_c2 = weight_c^2)
ggpairs(my_kungsan, columns = c("weight_c", "weight_c2", "sex", "height"))

m4 <- lm(height ~ weight_c + weight_c2 + sex, data = my_kungsan)
check_collinearity(m4)

library(tibble)
new_data <- tibble(weight_c = (50-36), weight_c2 = weight_c^2, sex = factor(c("male", "female")))
predict(m4, newdata = new_data)
predict(m4, newdata = new_data, interval = "prediction")
predict(m4, newdata = new_data, interval = "confidence")

library(readxl)
wine_data <- read_xlsx("white_wines.xlsx")

set.seed(16645573)
my_wine <- sample_n(wine_data, 4800)
w1 <- lm(quality ~ ., data = my_wine)
summary(w1)
check_collinearity(w1)

library(leaps)
all_mods <- regsubsets(quality ~ ., data = my_wine)
plot(all_mods, scale = 'Cp')
w2 <- lm(quality ~ fix_acid + vol_acid + res_sugar + free_sulphur + density + pH + sulphates + alcohol, data = my_wine)
summary(w2)
check_collinearity(w2)

w3 <- lm(quality ~ fix_acid + vol_acid + res_sugar + free_sulphur + pH + sulphates + alcohol, data = my_wine)
summary(w3)
check_collinearity(w3