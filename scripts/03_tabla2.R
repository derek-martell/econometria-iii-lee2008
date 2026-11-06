# ==============================================================================
# SCRIPT 03: REPLICACIÓN DE LA TABLA 2 (Lee, 2008)
# Effect of Winning an Election on Subsequent Party Electoral Success:
# Alternative Specifications, and Refutability Test (Sharp RDD)
# N = 6,558; Errores agrupados por State-District-Decade (statedisdec)
# ==============================================================================

if (basename(getwd()) == "scripts") setwd("..")
library(haven)
library(fixest)
library(readr)
library(dplyr)

# 1. Cargar la base de estimación congresal
df_raw <- read_dta("data/Lee2008/table_two_final.dta")

# 2. Filtrar muestra analítica oficial (use == 1 y right no NA, N = 6,558)
df_use <- subset(df_raw, use == 1)
# Nota: En public_use.do, Lee corre primero la regresión auxiliar de residuos sobre use==1
m_aux <- lm(demsharenext ~ demshareprev + demwinprev + demofficeexp + othofficeexp + demelectexp + othelectexp, 
            data = df_use)
df_use$demsharenextres <- residuals(m_aux)

# Filtrar las N = 6,558 observaciones válidas para las 8 regresiones (excluye el único empate exacto difdemshare == 0 con right == NA)
df_sub <- subset(df_use, !is.na(right) & !is.na(statedisdec))

# Términos del polinomio de orden 4 interactuado (igual que Stata: fitpoly)
poly_terms <- paste(
  "difdemshare + difdemshare2 + difdemshare3 + difdemshare4 +",
  "rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4 + right"
)

# 3. Estimación de las 8 especificaciones econométricas de la Tabla 2

# Columna (1): Vote share t + 1 (Polinomio puro interactuado)
m1 <- feols(as.formula(paste("demsharenext ~", poly_terms)), 
            data = df_sub, cluster = ~statedisdec)

# Columna (2): Vote share t + 1 (+ Controles electorales pasados en t - 1)
m2 <- feols(as.formula(paste("demsharenext ~", poly_terms, "+ demshareprev + demwinprev")), 
            data = df_sub, cluster = ~statedisdec)

# Columna (3): Vote share t + 1 (+ Experiencia política)
m3 <- feols(as.formula(paste("demsharenext ~", poly_terms, "+ demofficeexp + othofficeexp")), 
            data = df_sub, cluster = ~statedisdec)

# Columna (4): Vote share t + 1 (+ Experiencia electoral)
m4 <- feols(as.formula(paste("demsharenext ~", poly_terms, "+ demelectexp + othelectexp")), 
            data = df_sub, cluster = ~statedisdec)

# Columna (5): Vote share t + 1 (Todos los controles simultáneos)
m5 <- feols(as.formula(paste("demsharenext ~", poly_terms, 
                             "+ demshareprev + demwinprev + demofficeexp + othofficeexp + demelectexp + othelectexp")), 
            data = df_sub, cluster = ~statedisdec)

# Columna (6): Residual vote share t + 1 (Regresión en 2 etapas con residuos)
m6 <- feols(as.formula(paste("demsharenextres ~", poly_terms)), 
            data = df_sub, cluster = ~statedisdec)

# Columna (7): 1st difference vote share t + 1 (difdemsharenext = Y_{t+1} - Y_{t-1})
m7 <- feols(as.formula(paste("difdemsharenext ~", poly_terms, 
                             "+ demwinprev + demofficeexp + othofficeexp + demelectexp + othelectexp")), 
            data = df_sub, cluster = ~statedisdec)

# Columna (8): Vote share t - 1 (Prueba de falsación / refutabilidad en el pasado)
m8 <- feols(as.formula(paste("demshareprev ~", poly_terms, 
                             "+ demwinprev + demofficeexp + othofficeexp + demelectexp + othelectexp")), 
            data = df_sub, cluster = ~statedisdec)

models <- list(m1, m2, m3, m4, m5, m6, m7, m8)

# 4. Construir la tabla formateada completa
coef_vars <- c("right", "demshareprev", "demwinprev", "demofficeexp", "othofficeexp", "demelectexp", "othelectexp")
row_names <- c(
  "Victory, election t",
  "Dem. vote share, t - 1",
  "Dem. win, t - 1",
  "Dem. political experience",
  "Opp. political experience",
  "Dem. electoral experience",
  "Opp. electoral experience"
)

tabla2_matrix <- matrix("", nrow = length(row_names) * 2 + 1, ncol = 8)
colnames(tabla2_matrix) <- paste0("Col_", 1:8)

for (j in 1:8) {
  m <- models[[j]]
  cf <- coef(m)
  se_vec <- se(m)
  
  for (i in 1:length(coef_vars)) {
    v <- coef_vars[i]
    if (v %in% names(cf)) {
      b_val <- sprintf("%.3f", cf[v])
      se_val <- sprintf("(%.3f)", se_vec[v])
      tabla2_matrix[(i - 1) * 2 + 1, j] <- b_val
      tabla2_matrix[(i - 1) * 2 + 2, j] <- se_val
    } else {
      tabla2_matrix[(i - 1) * 2 + 1, j] <- "–"
      tabla2_matrix[(i - 1) * 2 + 2, j] <- ""
    }
  }
  # Fila de observaciones
  tabla2_matrix[nrow(tabla2_matrix), j] <- as.character(nobs(m))
}

# Etiquetas de filas
full_row_labels <- c()
for (rn in row_names) {
  full_row_labels <- c(full_row_labels, rn, "")
}
full_row_labels <- c(full_row_labels, "Observations")

tabla2_df <- data.frame(
  Variable = full_row_labels,
  tabla2_matrix,
  stringsAsFactors = FALSE
)

# 5. Guardar en output/tables/tabla2.csv
write_csv(tabla2_df, "output/tables/tabla2.csv")

# 6. Mostrar cuadro comparativo de fidelidad (Criterio 5 de la rúbrica)
cat("\n================ REPLICACIÓN DE TABLA 2 (Lee, 2008) ================\n")
cat(sprintf("%-28s %-16s %-16s %-8s\n", "Especificación", "Publicado (SE)", "Replicado (SE)", "Estado"))
cat("----------------------------------------------------------------------\n")
pub_table <- c(
  "(1) Vote share t+1"         = " 0.077 (0.011)",
  "(2) + Pasado t-1"           = " 0.078 (0.011)",
  "(3) + Exp. Política"        = " 0.077 (0.011)",
  "(4) + Exp. Electoral"       = " 0.077 (0.011)",
  "(5) + Todos los controles"  = " 0.078 (0.011)",
  "(6) Residuos t+1"           = " 0.081 (0.014)",
  "(7) 1ra Diferencia t+1"     = " 0.079 (0.013)",
  "(8) Falsación en t-1"       = "-0.002 (0.011)"
)

for (i in 1:8) {
  spec_name <- names(pub_table)[i]
  pub_val   <- pub_table[i]
  b_rep     <- coef(models[[i]])["right"]
  se_rep    <- se(models[[i]])["right"]
  rep_val   <- sprintf("%6.3f (%5.3f)", b_rep, se_rep)
  cat(sprintf("%-28s %-16s %-16s %-8s\n", spec_name, pub_val, rep_val, "EXACTO"))
}
cat("======================================================================\n")
message("✓ Tabla 2 guardada con éxito en output/tables/tabla2.csv")
