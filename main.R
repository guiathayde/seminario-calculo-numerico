source("gauss.R")
source("gauss_jacobi.R")
source("gauss_seidel.R")
source("trelica_simples.R")
source("trelica_fink.R")

A <- matrix(c(1, 1, 2, 2, 5, 3, 1, 1, 10), 3, 3)
b <- matrix(c(7, -8, 6), 3, 1)
xk <- matrix(c(10**-6, 10**6, 0), 3, 1)
erro <- 10**-10
k_max <- 10**8

print("##################### Resolução de teste #####################")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)

solucao_gauss_jacobi <- gauss_jacobi(A, b, xk, erro)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)

solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max, erro)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Resolução da treliça simples #####################\n\n")
cat("\n##################### Chute zerado #####################\n\n")
A <- trelica_simples_matrix
b <- trelica_simples_b
xk <- numeric(length(b))

cat("Chute inicial: ", xk, "\n")
solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Chute nosso #####################\n\n")
xk <- c(450, 500, 750, 1000, 250, 450)
cat("Chute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Chutes aleatorios #####################\n\n")
xk <- runif(6, min = 0, max = 1000)
cat("Chute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

xk <- runif(6, min = 0, max = 1000)
cat("Chute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Resolução da treliça Fink #####################\n")
cat("\n##################### Chute zerado #####################\n\n")
A <- trelica_fink_matrix
b <- trelica_fink_b
xk <- numeric(length(b))

cat("Chute inicial: ", xk, "\n")
solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Chute nosso #####################\n\n")
xk <- c(3, 4, 5, 2, 3, 4, 5, 2.5, 4, 3.5, 4, 3, 2.5, 3)
cat("Chute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

cat("\n##################### Chutes aleatorios #####################\n\n")
xk <- runif(14, min = 0, max = 1000)
cat("Chute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)

xk <- runif(14, min = 0, max = 1000)
cat("\nChute inicial: ", xk, "\n")

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)


solucao_gauss_jacobi <- gauss_jacobi(A, b, xk)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)


solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)
