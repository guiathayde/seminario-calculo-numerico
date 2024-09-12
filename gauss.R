source("escalonar_matrix.R")

# Parâmetros:
#   A - Matriz quadrada (n x n) contendo os coeficientes do sistema linear.
#   b - Vetor coluna de tamanho n com os termos independentes do sistema.
#
# Retorno:
#   Retorna um vetor com a solução do sistema linear Ax = b.
gauss <- function(A, b) {
  start_time <- Sys.time()
  n <- nrow(A)

  resultado <- escalonar_matrix(A, b)
  A_escalonada <- resultado$A
  b_atualizado <- resultado$b

  x <- b

  # Substituição retroativa para resolver o sistema triangular superior
  for (i in seq(n, 1, by = -1)) {
    soma <- 0
    if (i < n) {
      for (j in (i + 1):n) {
        soma <- soma + A_escalonada[i, j] * x[j]
      }
    }

    x[i] <- (b_atualizado[i] - soma) / A_escalonada[i, i]
  }

  end_time <- Sys.time()
  cat("Tempo de execução do Gauss: ", end_time - start_time, "\n\n")

  return(x)
}
