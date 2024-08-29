gauss <- function(A, b) {
  n <- nrow(A)

  # Eliminação de Gauss
  for (j in 1:(n - 1)) {
    for (k in seq(j + 1, n)) {
      fator <- A[k, j] / A[j, j]
      A[k, j:n] <- A[k, j:n] - fator * A[j, j:n]
      b[k] <- b[k] - fator * b[j]
    }
  }

  x <- numeric(n)

  # Triangular superior
  for (i in seq(n, 1, by = -1)) {
    sum <- 0
    if (i < n) {
      for (j in (i + 1):n) {
        sum <- sum + A[i, j] * x[j]
      }
    }

    x[i] <- (b[i] - sum) / A[i, i]
  }

  return(x)
}
