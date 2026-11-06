# ==============================================================================
# SCRIPT MAESTRO: REPLICACIÓN COMPLETA - LEE (2008)
# Artículo: "Randomized experiments from non-random selection in U.S. House elections"
# Curso:    Econometría III - Semestre 2026-II (UNMSM)
# Profesor: Mg. Marco Antonio Arroyo Lazo
# ==============================================================================
# Este script ejecuta secuencialmente todos los módulos de la réplica desde cero,
# sin intervención manual:
#   0. Instalación/carga automática de bibliotecas y verificación de directorios
#   1. Figura 1 (Motivación histórica: Incumbentes 1948-1998)
#   2. Tabla 1 (Balance de covariables por submuestras y ajuste polinómico grado 4)
#   3. Tabla 2 (8 especificaciones econométricas con SE agrupados por statedisdec)
#   4. Figuras 2 a 5 (Promedios locales en bins de 0.005 y ajustes paramétricos)
#   5. RDD Local Lineal y Sensibilidad al Ancho de Banda (Ficha C3 de la Rúbrica)
# ==============================================================================

rm(list = ls())

cat("\n========================================================================\n")
cat("  INICIANDO REPLICACIÓN COMPLETA DE DAVID S. LEE (2008) - N = 6,558\n")
cat("========================================================================\n")

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN DE DIRECTORIO RAÍZ E INSTALACIÓN AUTOMÁTICA DE BIBLIOTECAS
# ------------------------------------------------------------------------------
# Si el usuario abrió este script directamente desde la subcarpeta 'scripts/',
# subimos automáticamente a la carpeta raíz del proyecto:
if (basename(getwd()) == "scripts") {
  setwd("..")
}
cat(">>> Directorio de trabajo activo:\n   ", getwd(), "\n")

# Crear carpetas de salida si no existen
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Verificar e instalar automáticamente paquetes requeridos si faltan en la PC
paquetes_requeridos <- c(
  "haven",     # Lectura de bases .dta de Stata (MHE Data Archive MIT)
  "dplyr",     # Manipulación y limpieza de datos
  "ggplot2",   # Gráficos de alta resolución
  "fixest",    # Estimación MCO/Logit con errores estándar agrupados (cluster)
  "readr",     # Exportación de tablas CSV
  "gridExtra", # Composición de paneles múltiples (Figuras 2 a 5)
  "rdrobust",  # Estimación RDD Local Lineal y ancho de banda óptimo CCT
  "RATest"     # Contraste con la versión simplificada lee2008 en CRAN
)

paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[, "Package"])]
if (length(paquetes_faltantes) > 0) {
  cat(">>> Instalando paquetes faltantes:", paste(paquetes_faltantes, collapse = ", "), "\n")
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org", quiet = TRUE)
}

# Verificación de trazabilidad de bases (Regla 4 de la Rúbrica):
# Comparación entre el paquete RATest::lee2008 y la base oficial Lee2008.zip (MIT)
data("lee2008", package = "RATest")
df_mit <- haven::read_dta("data/Lee2008/table_two_final.dta")
df_mit_sub <- subset(df_mit, use == 1 & !is.na(right))

cat("\n>>> VERIFICACIÓN DE TRAZABILIDAD DE DATOS (MHE MIT vs. Paquete RATest):\n")
cat(sprintf("  - RATest::lee2008       : N = %d obs | %d variables (omite clúster 'statedisdec')\n",
            nrow(lee2008), ncol(lee2008)))
cat(sprintf("  - MIT table_two_final   : N = %d obs | %d variables (incluye clúster 'statedisdec')\n",
            nrow(df_mit_sub), ncol(df_mit_sub)))

# ------------------------------------------------------------------------------
# EJECUCIÓN SECUENCIAL DE LOS 5 MÓDULOS
# ------------------------------------------------------------------------------

# Paso 1: Figura 1
if (basename(getwd()) == "scripts") setwd("..")
cat("\n[1/5] Ejecutando scripts/01_figura1.R ...\n")
source("scripts/01_figura1.R", encoding = "UTF-8")

# Paso 2: Tabla 1
if (basename(getwd()) == "scripts") setwd("..")
cat("\n[2/5] Ejecutando scripts/02_tabla1.R ...\n")
source("scripts/02_tabla1.R", encoding = "UTF-8")

# Paso 3: Tabla 2
if (basename(getwd()) == "scripts") setwd("..")
cat("\n[3/5] Ejecutando scripts/03_tabla2.R ...\n")
source("scripts/03_tabla2.R", encoding = "UTF-8")

# Paso 4: Figuras 2 a 5
if (basename(getwd()) == "scripts") setwd("..")
cat("\n[4/5] Ejecutando scripts/04_figuras_2_a_5.R ...\n")
source("scripts/04_figuras_2_a_5.R", encoding = "UTF-8")

# Paso 5: RDD Local Lineal y Sensibilidad al Ancho de Banda (Ficha C3 Rúbrica)
if (basename(getwd()) == "scripts") setwd("..")
cat("\n[5/5] Ejecutando scripts/05_rdd_local_lineal.R ...\n")
source("scripts/05_rdd_local_lineal.R", encoding = "UTF-8")

cat("\n========================================================================\n")
cat("✓ ¡REPLICACIÓN COMPLETA FINALIZADA CON ÉXITO!\n")
cat("Todos los productos han sido generados en:\n")
cat("  - Tablas CSV : output/tables/\n")
cat("  - Figuras PNG: output/figures/\n")
cat("========================================================================\n")
