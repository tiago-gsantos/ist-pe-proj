set.seed(2255)  # Fixando a semente

# Função para simular o estado de um sistema
simular_sistema <- function() {
  circuitos <- sample(1:10, 9, replace = TRUE, prob = (1:10)/55)  # Emite sinais codificados
  aviso <- 2 %in% circuitos  # Verifica se pelo menos um circuito emite o sinal 2
  desligado <- 1 %in% circuitos  # Verifica se pelo menos um circuito emite o sinal 1
  return(list(aviso = aviso, desligado = desligado))
}

# Simulação de 150 realizações do sistema
n_simulacoes <- 150
avisos <- rep(0, n_simulacoes)
nao_desligados <- rep(0, n_simulacoes)

for (i in 1:n_simulacoes) {
  resultado <- simular_sistema()
  if (resultado$aviso && !resultado$desligado) {
    avisos[i] <- 1
  }
  if (!resultado$desligado) {
    nao_desligados[i] <- 1
  }
}

# Calcular proporção de vezes em que é produzido um aviso sonoro num sistema que não é desligado
proporcao <- sum(avisos) / sum(nao_desligados)

# Resultado
print(proporcao)
