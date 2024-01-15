library(tidyverse)
library(performance)
library(GGally)
library(tibble)
library(readxl)

toxic <- read_xlsx("aquatic_toxicity.xlsx")

set.seed(16645573)
my_toxic <- toxic %>% sample_n(525)

my_toxic %>% cor()

ggpairs(my_toxic, columns = c("mlogp", "rdchi", "gats1p", "lc50"))

m1 <- lm(lc50 ~ mlogp + rdchi + gats1p, data = my_toxic)
summary(m1)

library(leaps)
all_mods <- regsubsets(lc50 ~ ., data = my_toxic)
plot(all_mods, scale = 'Cp')

m2 <- lm(lc50 ~ tpsa + saacc + mlogp + rdchi + gats1p + nn, data = my_toxic)
summary(m2)

check_model(m2)

library(tibble)
new_toxic <- tibble(
  tpsa = c(9.23, 0),
  saacc = c(11, 0),
  h_050 = c(0, 0),
  mlogp = c(2.27, 3.37),
  rdchi = c(2.15, 2.08),
  gats1p = c(1.75, 1.20),
  nn = c(0, 0),
  c_040 = c(0, 0)
)
new_toxic

predict(m2, new_toxic, interval = "prediction")
predict(m2, new_toxic, interval = "confidence")

plot(all_mods, scale = 'r2')
plot(all_mods, scale = 'adjr2')

X <- model.matrix(m2)
y <- my_toxic$lc50
XtX <- t(X) %*% X
Xty <- t(X) %*% y
coefficients <- solve(XtX) %*% Xty
coefficients