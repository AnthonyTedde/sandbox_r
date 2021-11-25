

dbeta_own <- function(y, a, b){
  gamma(a+b) / (gamma(a) * gamma(b)) * y^(a-1) * (1-y)^(b-1)
}


dbeta(1, shape1 = 3, shape2 = 2)
dbeta_own(55, 3, 2)