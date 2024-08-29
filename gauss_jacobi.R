gauss_jacobi <- function(A, b, xk, erro = 10**-8) {
  k <- 0
  d <- erro + 1
  n <- nrow(A)
  xk_novo <- numeric(3)

  while (d > erro) {
    k <- k + 1
    for (i in 1:n) {
      soma <- 0
      for (j in 1:n) {
        if (i != j) soma <- soma + A[i, j] * xk[j]
      }
      xk_novo[i] <- (1 / A[i, i]) * (b[i] - soma)
    }
    max1 <- max(abs(xk_novo - xk))
    max2 <- max(abs(xk_novo))
    d <- max1 / max2
    xk <- xk_novo
    #cat(k, "-ésima interação e d=", d, "\n")
  }

  cat(k, "iterações para a convergência\n")

  return(xk_novo)
}
# Gauss-jacobi
# linha 17: multiplica oos valores das variaveis pelos coeficientes
# linha 19: termina a operação de Gauss-Jacobi operando a soma dos valores das variáveis x com a b da
# linha linha e dividindo pelo coeficiente do diagonal, ou seja, monta e opera a recursão.
# Sim. Quanto menor o erro, maior o número de iterações
# o chute não tem restrições em si. Todavia quanto mais distante de um r = (x1,x2,x3)
# verdadeiro, mais iterações necessárias.
