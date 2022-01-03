
set.seed(42)
y1 <- rnorm(100)
y2 <- rnorm(100)

hist(y1)
hist(y2)

Y <- outer(y1, y2)
dim(Y)

cbind(sort(y1), sort(y2))

contour(Y)