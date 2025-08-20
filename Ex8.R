set.seed(1592)
amostra <- c(31.8,31.7,35.2,37.1,31.7,36.1,36.3,33.2,34.3,37.5,30.4,34.6,32.4,31.7,30.2,34.3,35.6,34.9,38.9)
n <- 12
amostra_selecionada <- sample(amostra, n)

library(stats)

# Calculando a variância amostral
s2 <- var(amostra_selecionada)

# Níveis de confiança e quantis
gamma <- 0.96
alpha <- (1 - gamma) / 2
df <- n - 1

a <- qchisq(alpha, df)
b <- qchisq(1 - alpha, df)

# Intervalo de confiança para σ^2
IC_lower <- df * s2 / b
IC_upper <- df * s2 / a
IC1_amplitude <- IC_upper - IC_lower

library(pracma)

# Função para minimização
objective_function <- function(x) {
  c(pchisq(x[2], df) - pchisq(x[1], df) - gamma,
    dchisq(x[2], n+3) - dchisq(x[1], n+3))
}

# Uso de fsolve com os valores iniciais (a, b)
cd <- fsolve(objective_function, c(a, b))

# Novo intervalo de confiança para σ^2 usando c e d
IC_new_lower <- df * s2 / cd$x[2]
IC_new_upper <- df * s2 / cd$x[1]
IC2_amplitude <- IC_new_upper - IC_new_lower

# Diferença das amplitudes
amplitude_diferenca <- abs(IC1_amplitude - IC2_amplitude)
print(amplitude_diferenca)