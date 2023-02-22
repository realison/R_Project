library(tidyverse)
library(ggplot2)
library(modelr)
options(na.action = na.warn)


ggplot(sim1, aes(x, y)) + 
  geom_point()


# generate a tibble using random 250 numbers from -20 to 40 for a1
models <- tibble(
  a1 = runif(250,-20,40),
  a2 = runif(250,-5,5)
)

# a lot of models but only some are good
ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 


# find the best one, which means the distance, a1 is the intercept, a2 is slope, x is the variable
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

# using x in sim1, compute the distance for all points
model1(c(7, 1.5), sim1)

# We compute the difference between actual and predicted, 
# square them, average them, and the take the square root.

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)


#Now we can use purrr to compute the distance for all the models defined above

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}
models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models


# overlay the 10 best models on to the data

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )


# 10 best models, but think them as points
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

# roughly pick the points where the models are best and use grid

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

# find 10 best as well

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 


# back to the line plot, better than before

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )
# a numerical minimisation tool called Newton-Raphson search. 
# The intuition of Newton-Raphson is pretty simple: you pick a starting point and look around for the steepest slope. 

# the best one model
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

# using linear regression model, These are exactly the same values we got with optim()! 
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# Exercise

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
sim1a


models_1a <- sim1a %>% 
  mutate(dist = purrr::map2_dbl(x, y, sim1_dist))
models_1a

ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = x, slope = y, colour = -dist), 
    data = filter(models_1a, rank(dist) <= 10)
  )

ggplot(models_1a, aes(x, y)) +
  geom_point(data = filter(models_1a, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))


grid_1a <- expand.grid(
  x = seq(1, 4, length = 25),
  y = seq(5, 12, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(x, y, sim1_dist))


grid_1a %>% 
  ggplot(aes(x, y)) +
  geom_point(data = filter(grid_1a, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 


ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = x, slope = y, colour = -dist), 
    data = filter(grid_1a, rank(dist) <= 10))


best <- optim(c(0, 0), measure_distance, data = sim1a)
best$par

ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2], color = 'blue')

mod2 <- lm(y~x,data = sim1a)
mod2
coef(mod2)

# generating an evenly spaced grid of values that covers the region where our data lies.
# The easiest way to do that is to use modelr::data_grid().
grid <- sim1 %>% 
  data_grid(x) 
grid

# We’ll use modelr::add_predictions() which takes a data frame and a model. 
# It adds the predictions from the model to a new column in the data frame:

grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

# The residuals are just the distances between the observed and predicted values that we computed above

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

# This helps you calibrate the quality of the model: how far away are the predictions from the observed 
# values? Note that the average of the residual will always be 0

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# create a plot for residuals

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 
# Exercise 2a

sim_mod2 <- loess(y~x, data = sim1)
sim_mod2


ggplot(grid, aes(x,pred)) +
  geom_point() +
  #geom_smooth(color = 'red') +
  geom_ref_line(0, size = 2, colour = "red")


grid <- grid %>% 
  gather_predictions(sim_mod2) 
grid

grid <- grid %>% 
  add_predictions(sim_mod2) 
grid

grid <- grid %>% 
  spread_predictions(sim_mod2) 
grid




library(nycflights13)
library(lubridate)

# why low quality diamonds more expensive?
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()


ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)


# Focus on diamonds smaller than 2.5 carats (99.7% of the data)
# Log-transform the carat and price variables.


diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))
# The log-transformation is particularly useful here because it makes the pattern linear, 

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)


mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
mod_diamond


grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)


diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
mod_diamond2
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid


ggplot(grid, aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)


# what affects the number of daily flights

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) + 
  geom_line()

# day of week

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()


mod <- lm(n ~ wday, data = daily)


grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()



daily %>% 
  filter(resid < -100)


daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

#seasonal saturday effect
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")


term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")



library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
mod

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()





install.packages('leaflet')
library(leaflet)
leaflet() %>%
  setView(174.764, -36.877, zoom = 16) %>% 
  addTiles() %>%
  addMarkers(174.764, -36.877, popup = "Maungawhau") 





















