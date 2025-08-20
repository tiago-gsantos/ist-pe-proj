# Definindo os parâmetros
n <- 4
r <- 150
m <- 130
v <- -0.9

# Definindo a semente
set.seed(2126)

sum_p <- 0

for (i in 1:r) {
  num_Tvalues <- 0
  for (j in 1:m) {
    Z_values <- rnorm(n+1)
    T_value <- sqrt(n) * Z_values[1] / sqrt(sum(Z_values[2:(n+1)]^2))
    if(T_value <= v) {
      num_Tvalues <- num_Tvalues + 1
    }
  }
  sum_p <- sum_p + num_Tvalues/m
}

mean_p = sum_p / r
prob <- pt(v, df = n)

print(abs(mean_p - prob) * 100)

