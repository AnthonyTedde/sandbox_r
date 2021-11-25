library(magrittr)
data(hubble, package = "gamair")
hubble$x


X = matrix(c(3, 2, 3, 2, -1, 4,
             5, 1, -5, 4, 2, 1,
             9, -3, 2 , -1, 8, 1), ncol = 3)

qr(X)
Q <- qr.Q(qr(X))
R <- qr.R(qr(X))
qr.X(qr(X))
Q %*% R


data("mtcars")


lm.car <- lm(mpg ~ . -1, data = mtcars)

  summary()

beta_hat <- coefficients(lm.car) |>
  as.matrix()

X <- mtcars[, -1] |> as.matrix()
y <- mtcars[, 1] |> as.matrix()
class(X)
dim(X)


Q_cplte <- qr.Q(qr(X), complete = T)
R_cplte <- qr.R(qr(X), complete = T)
I <- Q_cplte %*% t(Q_cplte)
I[1:4, 1:4]

Q <- qr.Q(qr(X))
R <- qr.R(qr(X))
Q %*% R
dim(Q)
dim(R)

dim(X)
dim(beta_hat)
y_hat <- X %*% beta_hat
fitted(lm.car)


dim(y)
dim(y_hat)

dim(Q)
dim(Q_cplte)
sqrt(t(y - y_hat) %*% (y - y_hat))
norm(y - y_hat, type = "2")
norm(t(Q_cplte) %*% y - t(Q_cplte) %*% y_hat, type = "2")
norm(t(Q_cplte) %*% y - R_cplte %*% beta_hat, type = "2")

f <- (t(Q_cplte) %*% y)[1:dim(X)[2], ]
r <- (t(Q_cplte) %*% y)[(dim(X)[2]+1):dim(X)[1], ]


# ------------------------------------------------------------------------------
# Find the estimates of Beta ####
# ------------------------------------------------------------------------------

solve(R) %*% f
beta_hat

# ||r||² is the residual sum of square. 
# So to to find the residual standard error: 
r_norm <- sqrt(norm(r, type = "2")^2 / 22)
summary(lm.car)


# ------------------------------------------------------------------------------
# Distribution of the estimator Beta hat
# ------------------------------------------------------------------------------

lm.car |> summary()

sigma_hat_sqr <- norm(r, type = "2")^2 / (dim(X)[1] - dim(X)[2])
V_beta_hat <- diag(solve(R) %*% t(solve(R))) * rep(sigma_hat_sqr, 10)
# Std. Error of beta hat
sqrt(V_beta_hat)


# ------------------------------------------------------------------------------
# F-ratio result
# ------------------------------------------------------------------------------

aov(mpg ~ . -1, data = mtcars) |> summary()
lm(mpg ~ . -1, data = mtcars) |> anova()

# f_ <- f[(length(f)-1):length(f)]
f_ <- f[(length(f)-2)]
# f_ <- f[3:length(f)]
((t(f_) %*% f_)/length(f_)) / sigma_hat_sqr

norm(f_, type = "2")^2 / (sigma_hat_sqr * length(f_))
