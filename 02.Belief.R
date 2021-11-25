

# Example: Binomial distribution
p <- .5
t <- 2
o <- 4
dbinom(t, o, prob = p)
choose(o, t) * p^t * (1-p)^(o-t)

factorial(o) / factorial(o-t)
factorial(o) / (factorial(o-t) * factorial(t))