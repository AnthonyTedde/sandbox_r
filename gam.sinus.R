library(magrittr)
library(splines)
f <- function(n = 1000, domain = c(0,10)){
  x <- runif(n = n, min = domain[1], max = domain[2])
  data.frame(
    x = x,
    y = (sin(x)+exp(-x)) |> jitter(factor = 1000)
  )
}

data <- f()
data_train <- data[data$x <= 6,]
par(mfrow = c(2, 1))
plot(y ~ x, data = data_train)
plot(y ~ x, data = data)

par(mfrow = c(1, 1))
gam1 <- mgcv::gam(y ~ s(x, bs="tp"), data = data_train)
gam2 <- mgcv::gam(y ~ s(x, bs="cr"), data = data_train)
lm1 <- lm(y ~ bs(x, knots = 2), data = data_train)
mgcv::gam.check(gam1)

data2 <- data_train %>% 
  dplyr::mutate(
    y_fit_gam1 = fitted(gam1),
    y_fit_gam2 = fitted(gam2),
    y_fit_lm = fitted(lm1)
  )


ggplot2::ggplot(data = data2, ggplot2::aes(y = y, x = x)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam2), color = "green") +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam1), color = "blue")


# ------------------------------------------------------------------------------
# Sinus
# ------------------------------------------------------------------------------

library(magrittr)
library(splines)
random_sin <- function(n = 1000, domain = c(0,2.5*pi)){
  x <- runif(n = n, min = domain[1], max = domain[2])
  data.frame(
    x = x,
    y = sin(x) |> jitter(factor = 1000)
  )
}

data <- random_sin()
plot(y ~ x, data = data)
data_train <- data[data$x <= 2*pi,]
plot(y ~ x, data = data_train)

gam1 <- mgcv::gam(y ~ s(x, bs="tp"), data = data_train)
gam2 <- mgcv::gam(y ~ s(x, bs="cc"), data = data_train)
lm1 <- lm(y ~ bs(x, knots = 6), data = data_train)

data2 <- data_train %>% 
  dplyr::mutate(
    y_fit_gam1 = fitted(gam1),
    y_fit_gam2 = fitted(gam2),
    y_fit_lm = fitted(lm1)
  )


ggplot2::ggplot(data = data2, ggplot2::aes(y = y, x = x)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam1), color = "blue") +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam1), color = "green") +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_lm), color = "red")

termplot(lm1, partial.resid = T, se=T)
plot(gam1)

data3 <- data %>% 
  dplyr::mutate(
    y_fit_gam1 = predict(gam1, newdata = data),
    y_fit_gam2 = predict(gam2, newdata = data),
    y_fit_lm = predict(lm1, newdata = data) 
  ) 

ggplot2::ggplot(data = data3, ggplot2::aes(y = y, x = x)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam1), color = "blue") +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_gam1), color = "green") +
  ggplot2::geom_point(ggplot2::aes(y = y_fit_lm), color = "red")