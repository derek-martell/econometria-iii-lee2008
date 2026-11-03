# ==============================================================================
# SCRIPT MAESTRO: REPLICACIÓN COMPLETA - LEE (2008)
# Econometría III - Semestre 2026-II
# Universidad Nacional Mayor de San Marcos (UNMSM)
# ==============================================================================
# Este script ejecuta secuencialmente todos los módulos de la réplica desde cero:
# 1. Figura 1 (Motivación: Incumbentes 1948-1998)
# 2. Tabla 1 (Balance de características y ajuste polinómico grado 4)
# 3. Tabla 2 (8 especificaciones econométricas con SE clustered statedisdec)
# 4. Figuras 2 a 5 (Promedios locales en bins de 0.005 y ajustes paramétricos)
# 5. RDD Local Lineal y Sensibilidad al Ancho de Banda (Ficha C3 de la Rúbrica)
# ==============================================================================

cat("\n>>> INICIANDO REPLICACIÓN COMPLETA DE DAVID S. LEE (2008)...\n")

# Paso 1: Figura 1
cat("\n[1/5] Ejecutando scripts/01_figura1.R ...\n")
source("scripts/01_figura1.R")

# Paso 2: Tabla 1
cat("\n[2/5] Ejecutando scripts/02_tabla1.R ...\n")
source("scripts/02_tabla1.R")

# Paso 3: Tabla 2
cat("\n[3/5] Ejecutando scripts/03_tabla2.R ...\n")
source("scripts/03_tabla2.R")

# Paso 4: Figuras 2 a 5
cat("\n[4/5] Ejecutando scripts/04_figuras_2_a_5.R ...\n")
source("scripts/04_figuras_2_a_5.R")

# Paso 5: RDD Local Lineal (Rúbrica San Marcos)
cat("\n[5/5] Ejecutando scripts/05_rdd_local_lineal.R ...\n")
source("scripts/05_rdd_local_lineal.R")

cat("\n========================================================================\n")
cat("✓ ¡REPLICACIÓN COMPLETA FINALIZADA CON ÉXITO!\n")
cat("Todos los resultados han sido guardados en:\n")
cat("  - Tablas:  output/tables/\n")
cat("  - Figuras: output/figures/\n")
cat("========================================================================\n")
