source("gauss.R")
source("gauss_jacobi.R")
source("gauss_seidel.R")

leitura_basedados <- function(arq, tag) {
  dados_db <- read.csv(file = arq, header = TRUE, sep = tag)
  cat("\n Tudo certo com a leitura do arquivo", arq, "\n")
  return(dados_db)
}

# Leitura da base de dados separada por ";"
dados <- leitura_basedados("housing.csv", ",")

x <- dados$median_income
y <- dados$median_house_value

plot(x, y, main = "Diagrama de Dispersão", xlab = "x", ylab = "y", pch = 19)

A <- matrix(c(1, 1, 2, 2, 5, 3, 1, 1, 10), 3, 3)
b <- matrix(c(7, -8, 6), 3, 1)
xk <- matrix(c(10**-6, 10**6, 0), 3, 1)
erro <- 10**-8
k_max <- 100

solucao_gauss <- gauss(A, b)
cat("Gauss:\n")
print(solucao_gauss)

solucao_gauss_jacobi <- gauss_jacobi(A, b, xk, erro)
cat("Gauss-Jacobi:\n")
print(solucao_gauss_jacobi)

solucao_gauss_seidel <- gauss_seidel(A, b, xk, k_max, erro)
cat("Gauss-Seidel:\n")
print(solucao_gauss_seidel)
