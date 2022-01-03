
set.seed(4242)
n <- 1000
r <- vector(mode = "numeric", length = n)
for(i in 1:n){
  y <- rnorm(30)
  x <- rnorm(30)
  
  m <- lm(y ~ x)
  s <- summary(m)
  c <- coef(s)
  r[i] <- subset(c, rownames(c) == "x", select = "Pr(>|t|)") |> drop() 
}

hist(r)
mean(r < 0.05)
mean(r < 0.1)