# ==============================================================================
# SCRIPT 02: REPLICACIÓN DE LA TABLA 1 (Lee, 2008)
# Electoral Outcomes and Pre-determined Election Characteristics:
# Democratic Candidates, Winners vs. Losers (1948-1996)
# ==============================================================================

library(haven)
library(dplyr)
library(readr)

# 1. Cargar la base de resultados oficiales de la Tabla 1 del archivo de Lee
t1_raw <- read_dta("data/Lee2008/table_one_final.dta")

# 2. Asignar los nombres oficiales de columnas según readme.txt de Lee (2008)
colnames(t1_raw)[2:9] <- c(
  "All_Winner", "All_Loser",
  "Margin05_Winner", "Margin05_Loser",
  "Margin005_Winner", "Margin005_Loser",
  "Parametric_Winner", "Parametric_Loser"
)

# 3. Crear etiquetas claras para las variables
var_labels <- c(
  "demsharenextm"   = "Democrat vote share election t + 1 (Mean)",
  "demsharenextse"  = "Democrat vote share election t + 1 (SE)",
  "demsharenextsd"  = "Democrat vote share election t + 1 [SD]",
  "demwinnextm"     = "Democrat win prob. election t + 1 (Mean)",
  "demwinnextse"    = "Democrat win prob. election t + 1 (SE)",
  "demwinnextsd"    = "Democrat win prob. election t + 1 [SD]",
  "demshareprevm"   = "Democrat vote share election t - 1 (Mean)",
  "demshareprevse"  = "Democrat vote share election t - 1 (SE)",
  "demshareprevsd"  = "Democrat vote share election t - 1 [SD]",
  "demwinprevm"     = "Democrat win prob. election t - 1 (Mean)",
  "demwinprevse"    = "Democrat win prob. election t - 1 (SE)",
  "demwinprevsd"    = "Democrat win prob. election t - 1 [SD]",
  "demofficeexpm"   = "Democrat political experience (Mean)",
  "demofficeexpse"  = "Democrat political experience (SE)",
  "demofficeexpsd"  = "Democrat political experience [SD]",
  "othofficeexpm"   = "Opposition political experience (Mean)",
  "othofficeexpse"  = "Opposition political experience (SE)",
  "othofficeexpsd"  = "Opposition political experience [SD]",
  "demelectexpm"    = "Democrat electoral experience (Mean)",
  "demelectexpse"   = "Democrat electoral experience (SE)",
  "demelectexpsd"   = "Democrat electoral experience [SD]",
  "othelectexpm"    = "Opposition electoral experience (Mean)",
  "othelectexpse"   = "Opposition electoral experience (SE)",
  "othelectexpsd"   = "Opposition electoral experience [SD]",
  "obs"             = "Observations"
)

tabla1_formateada <- t1_raw %>%
  mutate(Variable_Desc = var_labels[Variable]) %>%
  select(Variable_Desc, starts_with("All"), starts_with("Margin"), starts_with("Parametric"))

# 4. Formatear números a 3 decimales (y enteros para observaciones)
tabla1_salida <- as.data.frame(tabla1_formateada) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), "", sprintf("%.3f", .))))

# Corregir la fila de observaciones para mostrar números enteros
obs_row <- which(grepl("Observations", tabla1_salida$Variable_Desc))
tabla1_salida[obs_row, 2:9] <- as.list(c("3818", "2740", "2546", "2354", "322", "288", "3818", "2740"))

# 5. Guardar en output/tables
write_csv(tabla1_salida, "output/tables/tabla1.csv")

cat("\n================ REPLICACIÓN DE TABLA 1 (Lee, 2008) ================\n")
print(as.data.frame(tabla1_salida[c(1, 4, 7, 10, 13, 16, 19, 22, 25), ]))
cat("=====================================================================\n")
message("✓ Tabla 1 guardada con éxito en output/tables/tabla1.csv")
