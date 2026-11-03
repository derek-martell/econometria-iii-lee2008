# ==============================================================================
# SCRIPT 05: RDD LOCAL LINEAL Y ANÁLISIS DE SENSIBILIDAD AL ANCHO DE BANDA
# Requisito explícito de la Rúbrica de Econometría III (Ficha C3 - UNMSM)
# Paquete: rdrobust (Calonico, Cattaneo & Titiunik)
# ==============================================================================

library(haven)
library(dplyr)
library(rdrobust)
library(ggplot2)
library(readr)

# 1. Cargar datos (filtrando exactamente las N = 6,558 observaciones válidas)
df_raw <- read_dta("data/Lee2008/table_two_final.dta")
df_sub <- subset(df_raw, use == 1 & !is.na(right) & !is.na(statedisdec))

# Variables clave:
Y <- df_sub$demsharenext
X <- df_sub$difdemshare
cluster_var <- df_sub$statedisdec

# 2. Estimación con Ancho de Banda Óptimo de Cattaneo (CCT)
rdd_opt <- rdrobust(y = Y, x = X, c = 0, cluster = cluster_var, kernel = "triangular")

cat("\n================ RDD LOCAL LINEAL: ANCHO DE BANDA ÓPTIMO ================\n")
summary(rdd_opt)
cat("=========================================================================\n")

# 3. Análisis de Sensibilidad al Ancho de Banda (Bandwidth Sensitivity)
# Probamos anchos de banda fijos desde 3% hasta 20% de margen
bw_list <- c(0.03, 0.05, 0.07, 0.10, 0.15, 0.20)
results_sens <- data.frame()

for (h_val in bw_list) {
  fit_h <- rdrobust(y = Y, x = X, c = 0, h = h_val, cluster = cluster_var, kernel = "triangular")
  
  coef_val <- fit_h$coef[1]        # Convencional
  se_val   <- fit_h$se[1]          # Error estándar
  ci_low   <- fit_h$ci[1, 1]       # IC 95% inferior
  ci_high  <- fit_h$ci[1, 2]       # IC 95% superior
  n_eff_l  <- fit_h$N_h[1]         # Obs efectivas perdedores
  n_eff_r  <- fit_h$N_h[2]         # Obs efectivas ganadores
  
  results_sens <- rbind(results_sens, data.frame(
    Bandwidth = h_val,
    Margen_pct = paste0("±", h_val * 100, "%"),
    Tau_RD = coef_val,
    SE = se_val,
    CI_Lower = ci_low,
    CI_Upper = ci_high,
    N_Efectivo = n_eff_l + n_eff_r
  ))
}

# Agregar fila del óptimo automático
results_sens <- rbind(results_sens, data.frame(
  Bandwidth = rdd_opt$bws[1, 1],
  Margen_pct = sprintf("Óptimo CCT (±%.1f%%)", rdd_opt$bws[1, 1] * 100),
  Tau_RD = rdd_opt$coef[1],
  SE = rdd_opt$se[1],
  CI_Lower = rdd_opt$ci[1, 1],
  CI_Upper = rdd_opt$ci[1, 2],
  N_Efectivo = sum(rdd_opt$N_h)
))

# 4. Exportar tabla de sensibilidad
write_csv(results_sens, "output/tables/tabla_sensibilidad_bw.csv")
cat("\nTabla de sensibilidad guardada en output/tables/tabla_sensibilidad_bw.csv\n")
print(results_sens)

# 5. Gráfico de Sensibilidad al Ancho de Banda (Bandwidth Sensitivity Plot)
p_sens <- ggplot(filter(results_sens, !grepl("Óptimo", Margen_pct)), aes(x = Bandwidth, y = Tau_RD)) +
  geom_hline(yintercept = 0.077, linetype = "dashed", color = "firebrick", linewidth = 0.8) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "skyblue", alpha = 0.25) +
  geom_line(color = "navy", linewidth = 0.9) +
  geom_point(color = "navy", size = 2.5) +
  geom_point(data = filter(results_sens, grepl("Óptimo", Margen_pct)), 
             aes(x = Bandwidth, y = Tau_RD), color = "darkred", size = 3.5, shape = 17) +
  annotate("text", x = 0.16, y = 0.079, label = "Polinomio Grado 4 Lee (0.077)", color = "firebrick", size = 3.5) +
  scale_y_continuous(limits = c(0.02, 0.14), breaks = seq(0.02, 0.14, 0.02)) +
  scale_x_continuous(breaks = seq(0.02, 0.22, 0.02)) +
  labs(
    title = "Sensibilidad del Efecto Causal al Ancho de Banda (Local Linear RDD)",
    subtitle = "Línea roja punteada: Estimador paramétrico de Lee (2008). Triángulo: Ancho de banda óptimo CCT.",
    x = "Ancho de banda (Bandwidth h)",
    y = "Efecto estimado de la Incumbencia (Tau)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5)
  )

ggsave("output/figures/figura_sensibilidad_bw.png", plot = p_sens, width = 8, height = 5, dpi = 300)
message("✓ Análisis RDD Local Lineal completado con éxito en output/")
