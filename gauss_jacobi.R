source("escalonar_matrix.R")

# Parâmetros:
#   A    - Matriz quadrada (n x n) contendo os coeficientes do sistema linear.
#   b    - Vetor coluna de tamanho n com os termos independentes do sistema.
#   xk   - Vetor inicial de tamanho n com uma estimativa inicial para a solução.
#   erro - (Opcional) Critério de parada do algoritmo, representando a diferença
#           máxima permitidaentre as aproximações sucessivas. O valor padrão é o menor valor da máquina.
#
# Retorno:
#   Retorna um vetor com a solução aproximada do sistema linear Ax = b após a convergência.
gauss_jacobi <- function(A, b, xk, erro = .Machine$double.xmin) {
  start_time <- Sys.time()

  k <- 0
  d <- erro + 1
  n <- nrow(A)
  xk_novo <- b

  resultado <- escalonar_matrix(A, b)
  A_escalonada <- resultado$A
  b_atualizado <- resultado$b

  max1 <- 0
  max2 <- 0

  while (d > erro) {
    k <- k + 1

    for (i in 1:n) {
      soma <- 0

      for (j in 1:n) {
        if (i != j) soma <- soma + A_escalonada[i, j] * xk[j]
      }

      if (A_escalonada[i, i] == 0) {
        cat("O elemento da diagonal A_escalonada[", i, ",", i, "] é zero. Não é possível continuar com o método de Gauss-Jacobi.\n")
        return(NULL)
      }

      xk_novo[i] <- (1 / A_escalonada[i, i]) * (b_atualizado[i] - soma)
    }

    max1 <- max(abs(xk_novo - xk))
    max2 <- max(abs(xk_novo))

    if (max2 == 0) {
      d <- max1
    } else {
      d <- max1 / max2 # Erro relativo
    }

    xk <- xk_novo
  }

  cat("Erro max: ", max2, "\n")
  cat("Erro relativo do Gauss-Jacobi: ", d, "\n")
  cat(k, "iterações para a convergência do Gauss-Jacobi\n")

  end_time <- Sys.time()
  cat("Tempo de execução do Gauss-Jacobi: ", end_time - start_time, "\n\n")

  return(xk_novo)
}
