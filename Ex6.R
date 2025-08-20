
# Por exemplo:
seed <- 1948
n <- 30
a <- 4
valor <- 90

set.seed(seed)
y <- vector(length = 1000)

for (i in 1:1000) {
  y[i] <- sum(rexp(n, rate = 1 / a))
}
sim <- sum(y > valor) / 1000

ext <- 1 - pgamma(valor, n, rate = 1 / a)

solution <- abs(sim - ext) * 100
