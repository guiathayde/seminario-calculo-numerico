trelica_simples_matrix <- matrix(
  c(
    -1, 0, 0, 1, 0, 0, # Ax
    0, -1, 1, 0, 0, 0, # Ay
    0, 0, -1, 0, 0, 0.71, # Fba
    0, 0, 0, -1, 0, 0.71, # Fbc
    0, 0, 0, 0, 1, -0.71, # Cy
    0, 0, 0, 0, 0, 0.711 # Fbc
  ),
  nrow = 6, byrow = TRUE
)

trelica_simples_b <- c(0, 0, 0, 0, 0, 500)
names(trelica_simples_b) <- c("Ax", "Ay", "Fba", "Fbc", "Cy", "Fcd")
