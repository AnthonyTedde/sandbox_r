# ------------------------------------------------------------------------------
# Normal distribution
# ------------------------------------------------------------------------------

rng <- seq(-10, 10, by = .1)
qntl <- seq(0, 1, along.with = rng)

normal <- tibble::tibble(X = rng, 
                         prct = qntl,
                         pdf = dnorm(rng), # Probability Density Function (PDF)
                         cdf = pnorm(rng), # Cumulative Distribution Function (CDF)
                         Y = qnorm(qntl) # Quantile Function (Inverse CDF)
                         )

plot(cdf ~ X, data = normal)
plot(pdf ~ X, data = normal)
plot(Y ~ prct, data = normal)


# ------------------------------------------------------------------------------
# Logistic function
# ------------------------------------------------------------------------------

logistic <- tibble::tibble(
  X = rng,
  prct = qntl,
  pdf = dlogis(rng),
  cdf = plogis(rng),
  Y = qlogis(qntl)
)

plot(cdf ~ X, data = logistic)
plot(pdf ~ X, data = logistic)
plot(Y ~ prct, data = logistic)


logistic_cdf_0_1 <- tibble::tibble(y = rng, prb = plogis(rng))
logistic_cdf_2_1 <- tibble::tibble(y = rng, prb = plogis(rng, location = 2))
logistic_cdf_0_2 <- tibble::tibble(y = rng, prb = plogis(rng, scale = 2))


plot(prb ~ y, data = logistic_cdf_0_1)
plot(prb ~ y, data = logistic_cdf_2_1)
plot(prb ~ y, data = logistic_cdf_0_2)


# Logit function