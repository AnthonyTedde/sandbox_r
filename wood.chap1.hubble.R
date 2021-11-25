data(hubble, package = "gamair")
head(hubble)
str(hubble)

# Model ####
hub.mob <- lm(y ~ x - 1, data = hubble)
summary(hub.mob)

fitted(hub.mob)
residuals(hub.mob)

plot(residuals(hub.mob) ~ fitted(hub.mob),
     xlab = "Fitted values", ylab = "Residuals")

sig <- sum((hubble$y - coefficients(hub.mob) * hubble$x)^2) / (length(hubble$y) - 1)
sig <- sum((hubble$y - fitted(hub.mob))^2) / (length(hubble$y) - 1)

sqrt(sig / sum(hubble$x^2))

sum(hubble$x**2)**(-1) * sqrt(sum((hubble$y - mean(hubble$y))^2) / (length(hubble$y) - 1))

coef(hub.mob) - 

