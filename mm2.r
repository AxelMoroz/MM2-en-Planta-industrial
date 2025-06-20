#----------------------------------------------------------------
# Resolución de sistemas de colas MM2 vs. MM3 con datos reales
# Investigación Operativa - Facultad de Ingeniería - UNLZ
#----------------------------------------------------------------

# 1. Cargar el paquete necesario
library(queueing) # Instalar con: install.packages("queueing")

#----------------------------------------------------------------
# 2. Definir parámetros del problema
#----------------------------------------------------------------

# Tasa de llegada de piezas (piezas/hora)
tasa_llegada <- 20.129

# Tiempo promedio de servicio por pieza (minutos/pieza)
tiempo_servicio_min <- 4.09

# Tasa de servicio por inspector (piezas/minuto y piezas/hora)
tasa_servicio_min <- 1 / tiempo_servicio_min
# Convertimos a piezas/hora

# Tasa de servicio por inspector (piezas/hora)
tasa_servicio_hora <- tasa_servicio_min * 60
cat("Tasa de servicio por inspector (piezas/hora):", tasa_servicio_hora, "\n")

# Número de inspectores a analizar
inspectores_2 <- 2
inspectores_3 <- 3

# Capacidad máxima del sistema (trabajadores en sistema, incluyendo en servicio y en cola)
capacidad_sistema <- 10

#----------------------------------------------------------------
# 3. Modelar los sistemas MM2 y MM3
#----------------------------------------------------------------

# Modelo con 2 inspectores
input_mm2 <- NewInput.MMC(lambda = tasa_llegada, mu = tasa_servicio_hora, s = inspectores_2, n = capacidad_sistema)
CheckInput(input_mm2)
modelo_mm2 <- QueueingModel(input_mm2)

# Modelo con 3 inspectores
input_mm3 <- NewInput.MMC(lambda = tasa_llegada, mu = tasa_servicio_hora, s = inspectores_3, n = capacidad_sistema)
CheckInput(input_mm3)
modelo_mm3 <- QueueingModel(input_mm3)

#----------------------------------------------------------------
# 4. Mostrar resultados y análisis
#----------------------------------------------------------------

cat("\n--- Resultados para 2 inspectores ---\n")
print(summary(modelo_mm2), digits = 4)
cat("Factor de utilización (Rho):", modelo_mm2$RO, "\n")
cat("Promedio de piezas en la cola (Lq):", modelo_mm2$Lq, "\n")
cat("Throughput (piezas/hora):", modelo_mm2$Throughput, "\n")

cat("\n--- Resultados para 3 inspectores ---\n")
print(summary(modelo_mm3), digits = 4)
cat("Factor de utilización (Rho):", modelo_mm3$RO, "\n")
cat("Promedio de piezas en la cola (Lq):", modelo_mm3$Lq, "\n")
cat("Throughput (piezas/hora):", modelo_mm3$Throughput, "\n")

#----------------------------------------------------------------
# 5. Responder preguntas del problema
#----------------------------------------------------------------

cat("\n--- Respuestas a las preguntas ---\n")

# a) ¿Se alcanza la estabilidad del sistema?
if (modelo_mm2$RO < 1) {
  cat("a) El sistema con 2 inspectores es estable (Rho =", round(modelo_mm2$RO, 3), ")\n")
} else {
  cat("a) El sistema con 2 inspectores NO es estable (Rho =", round(modelo_mm2$RO, 3), ")\n")
}
if (modelo_mm3$RO < 1) {
  cat("   El sistema con 3 inspectores es estable (Rho =", round(modelo_mm3$RO, 3), ")\n")
} else {
  cat("   El sistema con 3 inspectores NO es estable (Rho =", round(modelo_mm3$RO, 3), ")\n")
}

# b) ¿Se alcanzan los objetivos propuestos?
objetivo_cola <- 2
if (modelo_mm2$Lq <= objetivo_cola) {
  cat("b) Con 2 inspectores, se cumple el objetivo de no más de", objetivo_cola, "en la cola (Lq =", round(modelo_mm2$Lq, 3), ")\n")
} else {
  cat("b) Con 2 inspectores, NO se cumple el objetivo de la cola (Lq =", round(modelo_mm2$Lq, 3), ")\n")
}
if (modelo_mm3$Lq <= objetivo_cola) {
  cat("   Con 3 inspectores, se cumple el objetivo de la cola (Lq =", round(modelo_mm3$Lq, 3), ")\n")
} else {
  cat("   Con 3 inspectores, NO se cumple el objetivo de la cola (Lq =", round(modelo_mm3$Lq, 3), ")\n")
}

# c) ¿Cuántos inspectores debería proponer el jefe de producción?
if (modelo_mm2$Lq <= objetivo_cola) {
  cat("c) Se recomienda proponer un solo inspector adicional (total: 2 inspectores).\n")
} else {
  cat("c) Se recomienda proponer dos inspectores adicionales (total: 3 inspectores).\n")
}
#----------------------------------------------------------------