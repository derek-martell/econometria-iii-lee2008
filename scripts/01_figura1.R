# ==============================================================================
# SCRIPT 01: REPLICACIÓN DE LA FIGURA 1 (Lee, 2008)
# Electoral Success of U.S. House Incumbents: 1948-1998
# ==============================================================================

if (basename(getwd()) == "scripts") setwd("..")
library(haven)
library(ggplot2)

# 1. Cargar datos
df_fig1 <- read_dta("data/Lee2008/figure1.dta")

# 2. Generar gráfico
p1 <- ggplot(df_fig1, aes(x = year)) +
  # Serie 1: Incumbent Party (filtramos NA para que la línea una directo 1950 con 1954)
  geom_line(data = subset(df_fig1, !is.na(col1)), 
            aes(y = col1, linetype = "Incumbent Party")) +
  geom_point(data = subset(df_fig1, !is.na(col1)), 
             aes(y = col1, shape = "Incumbent Party"), size = 2) +
  
  # Serie 2: Winning Candidate
  geom_line(aes(y = col2, linetype = "Winning Candidate")) +
  geom_point(aes(y = col2, shape = "Winning Candidate"), size = 2) +
  
  # Serie 3: Runner-up Candidate
  geom_line(aes(y = col3, linetype = "Runner-up Candidate")) +
  geom_point(aes(y = col3, shape = "Runner-up Candidate"), size = 2) +
  
  # Estética y leyendas idénticas a la publicación
  scale_shape_manual(
    name = NULL,
    breaks = c("Incumbent Party", "Winning Candidate", "Runner-up Candidate"),
    values = c("Incumbent Party" = 15, "Winning Candidate" = 0, "Runner-up Candidate" = 4)
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = c("Incumbent Party", "Winning Candidate", "Runner-up Candidate"),
    values = c("Incumbent Party" = "solid", "Winning Candidate" = "dashed", "Runner-up Candidate" = "dashed")
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(limits = c(1948, 1998), breaks = seq(1948, 1998, 10)) +
  labs(
    x = "Year", 
    y = "Proportion Winning Election",
    title = "Figura 1: Éxito electoral de incumbentes (Cámara de Representantes, 1948-1998)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.35, 0.22),
    legend.background = element_rect(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
  )

# 3. Guardar en carpeta output
ggsave("output/figures/figura1.png", plot = p1, width = 8, height = 5, dpi = 300)
message("✓ Figura 1 guardada con éxito en output/figures/figura1.png")
