sigma <- 1

# 1. Design matrix ####

xa <- c(1, 1, -1, -1, 0)
xb <- c(1, 1, 1, 1, 0)
xc <- c(1., .5, 0, -.5, -1)

# For design A
Xa <- model.matrix(~xa)
Xb <- model.matrix(~xb)
Xc <- model.matrix(~xc)

mu_var <- function(design.matrix, new.data){
  inv <- solve(t(design.matrix)%*%design.matrix)
  vcv <- new.data %*% inv %*% t(new.data) * sigma
  diag(vcv)
}

x <- seq(-1, 1, length = 100)
new.design <- model.matrix(~x)

mva <- mu_var(Xa, new.data = new.design)
mvb <- mu_var(Xb, new.data = new.design)
mvc <- mu_var(Xc, new.data = new.design)

plot(
  range(c(xa, xb, xc)) ~ range(x), 
  type = "n", 
  ylim = c(0, 1.2),
  ylab = "Var. of predictions", xlab = "x Values",
  las = 1
)
lines(mva ~ x, lwd = 2, lty = 1)
lines(mvb ~ x, lwd = 2, lty = 2)
lines(mvc ~ x, lwd = 2, lty = 3)
legend("topright", lwd = 2, lty = 1:3, 
       legend = c("Design A", "Design B", "Design C"))
