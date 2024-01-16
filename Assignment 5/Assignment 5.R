ibrary(tidyverse)
library(performance)

cars <- read_csv("cars_sales.zip")

cars <- cars %>% filter(Price != "Not Priced")
cars <- cars %>% filter(!is.na(DealType))
cars <- cars %>% rename(UsedNew = `Used/New`)
cars <- cars %>% mutate(Price = parse_number(Price))
cars <- cars %>% mutate(SellerType = factor(SellerType))
cars <- cars %>% mutate(DealType = factor(DealType))
cars <- cars %>% mutate(Age = 2023 - Year)
set.seed(16645573)
my_cars <- cars %>% sample_n(9100)

ggplot(my_cars, aes(x = Age, y = Price)) + geom_point() + geom_smooth(method=lm, se = 
                                                                        FALSE)
m1 <- lm(Price ~ Age, data = my_cars)
check_model(m1)

ggplot(my_cars, aes(x = Age, y = log(Price))) + geom_point() + geom_smooth(method=lm, se = 
                                                                             FALSE)
m2 <- lm(log(Price) ~ Age, data = my_cars)
check_model(m2)

ggplot(data = my_cars, aes(x = Age, y = log(Price), color = DealType)) + geom_point()
ggplot(data = my_cars, aes(x = Age, y = log(Price), color = DealType)) + geom_point() + 
  geom_smooth(method=lm)

m3 <- lm(log(Price) ~ Age * DealType, data = my_cars)
check_model(m3)

m4 <- lm(log(Price) ~ Age * DealType + Mileage, data = my_cars)
summary(m3)
summary(m4)
check_model(m4)

coef(m4)

new_car <- tibble(Age = 2023 - 2017, DealType = "Great", Mileage = 55000)
exp(predict(m4, new_car))
exp(predict(m4, new_car, interval = "confidence"))

m5 <- lm(log(Price) ~ Age * DealType + Mileage + ConsumerRating, data = my_cars)
summary(m4)
summary(m5)