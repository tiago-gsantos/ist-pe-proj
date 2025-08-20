library(stats4)

data <- c(8.54, 4.76, 5.15, 4.96, 6.25, 7.22, 12.9, 6.04, 8.86, 4.88,
           6.54, 4.53, 4.7, 5.38, 5.96, 5.17, 5.09, 5.11)
a <- 4.5
theta <- 3.4
p <- 0.25
valor_inicial <- 3.4

# data <- c(4.98, 4.69, 6.01, 5.78, 7.21, 4.88, 4.28, 5.75, 4.41, 5.8, 4.26, 4.93)
# valor_inicial <- 4.6
# p <- 0.25
# theta <- 4.6
# a <- 4

library(stats4)

soma_logdata <- sum(log(data))
n <- length(data)

fun <- function(theta) {
  res <- -n * log(theta) + (theta + 1) * soma_logdata - n * theta * log(a)
  return(res)
}

result <- mle(minuslog = fun, start = list(theta = valor_inicial))
est_theta <- as.vector(result@coef)

quantil <- function(p, theta) {
  res <- a * (1 - p)^(-1 / theta)
}

estimativa <- quantil(p, est_theta)
valor_exato <- quantil(p, theta)

solution <- abs(estimativa - valor_exato)