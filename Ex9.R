# Definir os parâmetros
n <- 100
lambda_H0 <- 2.90
lambda_H1 <- 3.15
k <- 3.234
m <- 5000

# Fixar a semente para reprodutibilidade
set.seed(2822)

# Simular as amostras e realizar os testes
erro_tipo_I <- 0
erro_tipo_II <- 0

for (i in 1:m) {
  # Gerar amostras sob H0 e H1
  amostra_H0 <- rpois(n, lambda_H0)
  amostra_H1 <- rpois(n, lambda_H1)
  
  # Calcular as médias amostrais
  x_bar_H0 <- mean(amostra_H0)
  x_bar_H1 <- mean(amostra_H1)
  
  # Testar H0: Rejeitar H0 se x_bar > k
  if (x_bar_H0 > k) {
    erro_tipo_I <- erro_tipo_I + 1
  }
  if (x_bar_H1 <= k) {
    erro_tipo_II <- erro_tipo_II + 1
  }
}

# Calcular as probabilidades de erro
prob_erro_tipo_I <- erro_tipo_I / m
prob_erro_tipo_II <- erro_tipo_II / m

# Calcular o quociente das probabilidades de erro
quociente_erro <- prob_erro_tipo_II / prob_erro_tipo_I

# Imprimir os resultados
print(quociente_erro)
