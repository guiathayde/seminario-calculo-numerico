gauss_seidel <- function(A, b, xk, k_max = 100, erro = 10**-8) {
  x_old <- xk
  n <- nrow(A)
  x_new <- numeric(n)

  for (k in 1:k_max) {
    x_new <- x_old

    for (i in 1:n) {
      new <- 0
      old <- 0
      for (j in 1:n) {
        if (j < i) {
          new <- new + A[i, j] * x_new[j]
        } else if (j > i) {
          old <- old + A[i, j] * x_old[j]
        }
      }

      x_new[i] <- (b[i] - new - old) / A[i, i]
    }

    max <- 0
    for (i in 1:n) {
      diferenca <- abs(x_new[i] - x_old[i])

      if (diferenca > max) {
        max <- diferenca
      }
    }

    if (max < erro) {
      break
    }

    x_old <- x_new
  }

  return(x_new)
}
