source("gauss_jacobi.R")
source("gauss_seidel.R")

set.seed(Sys.time())

A <- matrix(runif(10000), nrow = 100, ncol = 100)
b <- runif(100)
xk <- runif(100)

solucao_jacobi <- gauss_jacobi(A, b, xk)
print(solucao_jacobi)

solucao_seidel <- gauss_seidel(A, b, xk)
print(solucao_seidel)
