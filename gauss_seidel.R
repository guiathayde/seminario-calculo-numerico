source("escalonar_matrix.R")

# Parâmetros:
#   A     - Matriz quadrada (n x n) contendo os coeficientes do sistema linear.
#   b     - Vetor coluna de tamanho n com os termos independentes do sistema.
#   xk    - Vetor inicial de tamanho n com uma estimativa inicial para a solução.
#   k_max - (Opcional) Número máximo de iterações permitidas. O valor padrão é o maior da máquina.
#   erro  - (Opcional) Critério de parada do algoritmo, representando a diferença máxima permitida
#           entre as aproximações sucessivas. O valor padrão é o menor da máquina.
#
# Retorno:
#   Retorna um vetor com a solução aproximada do sistema linear Ax = b após a convergência.
gauss_seidel <- function(A, b, xk, k_max = 1000, erro = .Machine$double.xmin) {
  start_time <- Sys.time()

  names(xk) <- names(b)
  x_old <- xk
  n <- nrow(A)
  x_new <- b

  resultado <- escalonar_matrix(A, b)
  A_escalonada <- resultado$A
  b_atualizado <- resultado$b

  for (k in 1:k_max) {
    x_new <- x_old

    for (i in 1:n) {
      sum_new <- 0
      sum_old <- 0

      for (j in 1:n) {
        if (j < i) {
          sum_new <- sum_new + A_escalonada[i, j] * x_new[j]
        } else if (j > i) {
          sum_old <- sum_old + A_escalonada[i, j] * x_old[j]
        }
      }

      if (A_escalonada[i, i] == 0) {
        cat("O elemento da diagonal A_escalonada[", i, ",", i, "] é zero. Não é possível continuar com o método de Gauss-Seidel.\n")
        return(NULL)
      }

      x_new[i] <- (b_atualizado[i] - sum_new - sum_old) / A_escalonada[i, i]
    }

    max_diff <- max(abs(x_new - x_old))

    if (is.na(max_diff) || is.nan(max_diff)) {
      print("O cálculo gerou valores indefinidos (NA ou NaN). Verifique os dados de entrada.")
      return(NULL)
    }

    if (max_diff < erro) {
      cat("Erro relativo do Gauss-Seidel: ", max_diff, "\n")
      cat(k, "iterações para a convergência do Gauss-Seidel\n")
      break
    }

    x_old <- x_new
  }

  end_time <- Sys.time()
  cat("Tempo de execução do Gauss-Seidel: ", end_time - start_time, "\n\n")

  return(x_new)
}
