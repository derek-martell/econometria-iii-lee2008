# ==============================================================================
# SCRIPT 04: REPLICACIÓN DE LAS FIGURAS 2 A 5 (Lee, 2008)
# Promedios locales en bins de 0.005 entre [-0.25, 0.25] y ajuste paramétrico
# ==============================================================================

if (basename(getwd()) == "scripts") setwd("..")
library(haven)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Función auxiliar para generar los gráficos RDD con estilo idéntico al paper
plot_rdd <- function(data, x_var, y_pts, y_line, title_text, y_label, y_limits, y_breaks, fit_type = "Polynomial fit") {
  # Filtrar muestra por bin (100 bins de ancho 0.005 en [-0.25, 0.25): difgrp 150 a 249)
  df_plot <- data %>%
    filter(use == 1 & difgrp >= 150 & difgrp < 250) %>%
    group_by(difgrp) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(bin_x = (difgrp - 200) * 0.005 + 0.0025)
  
  # Línea paramétrica: separamos izquierda y derecha de cero para que no cruce el umbral
  p <- ggplot(df_plot, aes(x = bin_x)) +
    # Línea vertical punteada en el corte x = 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    # Promedios locales (puntos negros)
    geom_point(aes(y = .data[[y_pts]], shape = "Local Average"), size = 1.6, color = "black") +
    # Ajuste paramétrico (línea sólida separada a izquierda y derecha de cero)
    geom_line(data = filter(df_plot, bin_x < 0), 
              aes(y = .data[[y_line]], linetype = fit_type), color = "black", linewidth = 0.7) +
    geom_line(data = filter(df_plot, bin_x >= 0), 
              aes(y = .data[[y_line]], linetype = fit_type), color = "black", linewidth = 0.7) +
    # Escalas y títulos
    scale_x_continuous(limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, 0.05)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_shape_manual(name = NULL, values = c("Local Average" = 16), guide = guide_legend(order = 1)) +
    scale_linetype_manual(name = NULL, values = setNames("solid", fit_type), guide = guide_legend(order = 2)) +
    labs(
      title = title_text,
      x = "Democratic Vote Share Margin of Victory, Election t",
      y = y_label
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.18, 0.82),
      legend.background = element_blank(),
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.4),
      legend.spacing.y = unit(-0.15, "cm"),
      legend.margin = margin(2, 6, 2, 6),
      legend.key.height = unit(0.38, "cm"),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 8.5)
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# 1. FIGURAS 2 Y 3: Nivel Candidato (individ_final.dta)
# ------------------------------------------------------------------------------
df_ind <- read_dta("data/Lee2008/individ_final.dta")

# Figura 2a: Probabilidad de ganar en t + 1
p2a <- plot_rdd(
  data = df_ind, x_var = "difshare", y_pts = "mmyoutcomenext", y_line = "mpmyoutcomenext",
  title_text = "Fig. 2a: Candidate's probability of winning election t + 1",
  y_label = "Probability of Winning, Election t+1",
  y_limits = c(0, 1), y_breaks = seq(0, 1, 0.1), fit_type = "Logit fit"
)

# Figura 2b: Victorias acumuladas en t
p2b <- plot_rdd(
  data = df_ind, x_var = "difshare", y_pts = "mofficeexp", y_line = "mpofficeexp",
  title_text = "Fig. 2b: Candidate's accumulated past victories as of election t",
  y_label = "No. of Past Victories as of Election t",
  y_limits = c(0, 5), y_breaks = seq(0, 5, 0.5), fit_type = "Polynomial fit"
)

# Figura 3a: Probabilidad de candidatura en t + 1
p3a <- plot_rdd(
  data = df_ind, x_var = "difshare", y_pts = "mrunagain", y_line = "mprunagain",
  title_text = "Fig. 3a: Candidate's probability of candidacy in election t + 1",
  y_label = "Probability of Candidacy, Election t+1",
  y_limits = c(0, 1), y_breaks = seq(0, 1, 0.1), fit_type = "Logit fit"
)

# Figura 3b: Intentos acumulados en t
p3b <- plot_rdd(
  data = df_ind, x_var = "difshare", y_pts = "melectexp", y_line = "mpelectexp",
  title_text = "Fig. 3b: Candidate's accumulated past election attempts as of election t",
  y_label = "No. of Past Attempts as of Election t",
  y_limits = c(0, 5), y_breaks = seq(0, 5, 0.5), fit_type = "Polynomial fit"
)

# ------------------------------------------------------------------------------
# 2. FIGURAS 4 Y 5: Nivel Partido / Distrito (group_final.dta)
# ------------------------------------------------------------------------------
df_grp <- read_dta("data/Lee2008/group_final.dta")

# Figura 4a: Porcentaje de votos del partido en t + 1
p4a <- plot_rdd(
  data = df_grp, x_var = "difdemshare", y_pts = "mdemsharenext", y_line = "mpdemsharenext",
  title_text = "Fig. 4a: Democrat party's vote share in election t + 1",
  y_label = "Vote Share, Election t+1",
  y_limits = c(0.30, 0.70), y_breaks = seq(0.30, 0.70, 0.05), fit_type = "Polynomial fit"
)

# Figura 4b: Porcentaje de votos del partido en t - 1
p4b <- plot_rdd(
  data = df_grp, x_var = "difdemshare", y_pts = "mdemshareprev", y_line = "mpdemshareprev",
  title_text = "Fig. 4b: Democrat party's vote share in election t - 1",
  y_label = "Vote Share, Election t-1",
  y_limits = c(0.30, 0.70), y_breaks = seq(0.30, 0.70, 0.05), fit_type = "Polynomial fit"
)

# Figura 5a: Probabilidad de victoria del partido en t + 1
p5a <- plot_rdd(
  data = df_grp, x_var = "difdemshare", y_pts = "mdemwinnext", y_line = "mpdemwinnext",
  title_text = "Fig. 5a: Democratic party probability of victory in election t + 1",
  y_label = "Probability of Victory, Election t+1",
  y_limits = c(0, 1), y_breaks = seq(0, 1, 0.1), fit_type = "Logit fit"
)

# Figura 5b: Probabilidad de victoria del partido en t - 1
p5b <- plot_rdd(
  data = df_grp, x_var = "difdemshare", y_pts = "mdemwinprev", y_line = "mpdemwinprev",
  title_text = "Fig. 5b: Democratic probability of victory in election t - 1",
  y_label = "Probability of Victory, Election t-1",
  y_limits = c(0, 1), y_breaks = seq(0, 1, 0.1), fit_type = "Logit fit"
)

# ------------------------------------------------------------------------------
# 3. Guardar figuras individuales y compuestas en output/figures/
# ------------------------------------------------------------------------------

# Individuales
ggsave("output/figures/figura2a.png", plot = p2a, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura2b.png", plot = p2b, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura3a.png", plot = p3a, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura3b.png", plot = p3b, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura4a.png", plot = p4a, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura4b.png", plot = p4b, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura5a.png", plot = p5a, width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/figura5b.png", plot = p5b, width = 7, height = 4.5, dpi = 300)

# Paneles compuestos (iguales al formato del artículo de Lee)
p2_comp <- arrangeGrob(p2a, p2b, ncol = 1)
p3_comp <- arrangeGrob(p3a, p3b, ncol = 1)
p4_comp <- arrangeGrob(p4a, p4b, ncol = 1)
p5_comp <- arrangeGrob(p5a, p5b, ncol = 1)

ggsave("output/figures/figura2_completa.png", plot = p2_comp, width = 7.5, height = 9, dpi = 300)
ggsave("output/figures/figura3_completa.png", plot = p3_comp, width = 7.5, height = 9, dpi = 300)
ggsave("output/figures/figura4_completa.png", plot = p4_comp, width = 7.5, height = 9, dpi = 300)
ggsave("output/figures/figura5_completa.png", plot = p5_comp, width = 7.5, height = 9, dpi = 300)

message("✓ Figuras 2 a 5 guardadas con éxito en output/figures/")
