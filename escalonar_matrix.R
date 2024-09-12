# Parâmetros:
#   A - Matriz quadrada (n x n).
#   b - Vetor coluna de tamanho n com os termos independentes do sistema.
#
# Retorno:
#   Retorna uma lista com a matriz escalonada e o vetor b atualizado.
escalonar_matrix <- function(A, b) {
  n <- nrow(A)

  for (j in 1:(n - 1)) {
    max_index <- j
    max_value <- abs(A[j, j])

    for (i in (j + 1):n) {
      if (abs(A[i, j]) > max_value) {
        max_value <- abs(A[i, j])
        max_index <- i
      }
    }
    if (A[max_index, j] == 0) {
      print("Matriz singular, escalonamento impossível.")
      return(NULL)
    }

    if (max_index != j) {
      A[c(j, max_index), ] <- A[c(max_index, j), ]
      b[c(j, max_index)] <- b[c(max_index, j)]
    }

    for (k in (j + 1):n) {
      fator <- A[k, j] / A[j, j]
      A[k, j:n] <- A[k, j:n] - fator * A[j, j:n]
      b[k] <- b[k] - fator * b[j]
    }
  }

  return(list(A = A, b = b))
}
