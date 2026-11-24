# =============================================================================
# 02_figura1.R — Figura 1: éxito electoral de los incumbentes, 1948–1998
# Se calcula desde los microdatos (group_final, individ_final). figure1.dta solo
# se usa como referencia publicada para la comparación.
# =============================================================================
source("scripts/00_utilidades.R")

grp <- read_dta(file.path(RUTA_DATOS, "group_final.dta"))
ind <- read_dta(file.path(RUTA_DATOS, "individ_final.dta"))
pub <- read_dta(file.path(RUTA_DATOS, "figure1.dta"))

# Partido incumbente: proporción de distritos donde el partido ganador en t vuelve a
# ganar en t+1 (se grafica en el año t+1; excluye cambios de distrito por redistritaje).
partido <- grp |>
  filter(!is.na(partywin), !is.na(partywinnext)) |>
  group_by(year = yearel + 2) |>
  summarise(partido_incumbente = mean(partywinnext == partywin), .groups = "drop")

# Candidato ganador / segundo lugar en t: proporción que gana en t+1.
# Nota: figure1.dta ubica estas dos series en el año t (no t+1); se respeta esa convención.
ganador <- ind |> filter(myoutcome == 1, !is.na(myoutcomenext)) |>
  group_by(year = yearel) |> summarise(candidato_ganador = mean(myoutcomenext), .groups = "drop")
segundo <- ind |> filter(second == 1, !is.na(myoutcomenext)) |>
  group_by(year = yearel) |> summarise(candidato_segundo = mean(myoutcomenext), .groups = "drop")

fig1 <- data.frame(year = seq(1948, 1998, 2)) |>
  left_join(partido, "year") |> left_join(ganador, "year") |> left_join(segundo, "year")
# Lee no reporta la serie del partido en años t+1 que terminan en 2 (redistritaje)
fig1$partido_incumbente[fig1$year %% 10 == 2] <- NA
guardar_tabla(fig1, "figura1_calculada")

comp <- fig1 |> left_join(pub, "year") |>
  mutate(dif_partido = partido_incumbente - col1, dif_ganador = candidato_ganador - col2,
         dif_segundo = candidato_segundo - col3)
saveRDS(comp, "output/intermedios/comparacion_figura1.rds")
cat(sprintf("Figura 1 — máx |calculado − publicado|: partido %.1e, ganador %.1e, segundo %.1e\n",
            max(abs(comp$dif_partido), na.rm = TRUE), max(abs(comp$dif_ganador), na.rm = TRUE),
            max(abs(comp$dif_segundo), na.rm = TRUE)))

# Promedios del periodo, para contrastar con lo que dice el texto (secc. 3.1):
# partido incumbente "about 90%", candidato ganador "80 percent", segundo lugar "3%".
promedios <- data.frame(serie = c("Partido incumbente", "Candidato ganador", "Candidato en 2.º lugar"),
                        texto = c(0.90, 0.80, 0.03),
                        calculado = c(mean(fig1$partido_incumbente, na.rm = TRUE),
                                      mean(fig1$candidato_ganador, na.rm = TRUE),
                                      mean(fig1$candidato_segundo, na.rm = TRUE)))
saveRDS(promedios, "output/intermedios/figura1_promedios_texto.rds")

# Gráfico con el mismo diseño que el artículo: partido incumbente = línea sólida con
# cuadrados llenos; candidato ganador = punteada con cuadrados vacíos; segundo lugar =
# punteada con asteriscos. Sin puntos en años terminados en 2 (redistritaje).
largo <- bind_rows(
  data.frame(year = fig1$year, valor = fig1$partido_incumbente, serie = "Partido incumbente"),
  data.frame(year = fig1$year, valor = fig1$candidato_ganador, serie = "Candidato ganador"),
  data.frame(year = fig1$year, valor = fig1$candidato_segundo, serie = "Candidato en 2.º lugar")) |>
  filter(!is.na(valor)) |>
  mutate(serie = factor(serie, levels = c("Partido incumbente", "Candidato ganador", "Candidato en 2.º lugar")))
g1 <- ggplot(largo, aes(year, valor, shape = serie, linetype = serie)) +
  geom_line(linewidth = 0.4) + geom_point(size = 2, fill = "black") +
  scale_shape_manual(NULL, values = c(15, 0, 8)) +
  scale_linetype_manual(NULL, values = c("solid", "dotted", "dotdash")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0.01)) +
  scale_x_continuous(breaks = seq(1948, 1998, 10), limits = c(1947, 1999)) +
  labs(title = "Figura 1. Éxito electoral de incumbentes en la Cámara de EE. UU., 1948–1998",
       x = "Año", y = "Proporción que gana la elección",
       caption = "Calculado con group_final.dta e individ_final.dta (Lee, 2008). Sin puntos en años terminados en 2 (redistritaje).") +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.3, 0.6), legend.background = element_rect(colour = "black", linewidth = 0.3),
        legend.key.width = unit(1.2, "cm"))
ggsave("output/figures/figura1.png", g1, width = 7, height = 4.8, dpi = 300)
