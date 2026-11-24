# =============================================================================
# 01_datos.R — Importación, construcción de muestras y diccionario de datos
# Réplica de Lee (2008). Insumos: archivos públicos del autor (data/Lee2008/).
# Salidas: output/intermedios/*.rds y hoja «Diccionario» del Excel de resultados
# =============================================================================
source("scripts/00_utilidades.R")

t2  <- read_dta(file.path(RUTA_DATOS, "table_two_final.dta"))   # distrito–elección
grp <- read_dta(file.path(RUTA_DATOS, "group_final.dta"))       # distrito–elección (partidos)
ind <- read_dta(file.path(RUTA_DATOS, "individ_final.dta"))     # candidato–elección

# ---- 1. Muestra a nivel de distrito (Tablas 1–2, Figuras 4–5) ---------------
# use == 1: elecciones t de 1946–1996 cuyo año no termina en 0 o 2 (redistritaje).
# Se descarta la única observación con margen exactamente 0 (right = NA): N = 6558.
dist <- t2 |>
  filter(use == 1, !is.na(right)) |>
  mutate(x = difdemshare, D = right, bin = asignar_bin(x))
stopifnot(nrow(dist) == 6558)

# ---- 2. Resultado "el Partido Demócrata gana el escaño en t+1" --------------
# table_two_final no trae esta variable. Punto de partida: ganador observado en t+1
# desde group_final (100 = Demócrata, códigos ICPSR); 290 casos sin dato. La versión
# final (sección 2b) reconstruye la variable imputada de Lee.
grp_u <- grp |>
  filter(use == 1, difdemshare != 0) |>
  mutate(x = difdemshare, D = as.numeric(x > 0), bin = asignar_bin(x),
         demwinnext = ifelse(is.na(partywinnext), NA_real_, as.numeric(partywinnext == 100)))
stopifnot(nrow(grp_u) == 6558)

# Los dos archivos de distrito no están ordenados igual ni comparten identificador.
# Se emparejan por el margen (idéntico en ambos); los empates (743 obs., casi todos
# elecciones sin oposición, |x| = 1) se emparejan por orden de aparición. El empate
# solo afecta a regresiones de demwinnext con covariables (comentario de la sección 3.4).
emparejar <- function(a, b) {
  a <- a |> mutate(.ord = ave(seq_along(x), x, FUN = seq_along))
  b <- b |> mutate(.ord = ave(seq_along(x), x, FUN = seq_along))
  inner_join(a, b, by = c("x", ".ord")) |> select(-.ord)
}
dist <- dist |> mutate(.id = row_number())
dist_win <- emparejar(dist, grp_u |> select(x, yearel, partywinnext, demwinnext_obs = demwinnext))
stopifnot(nrow(dist_win) == 6558)

# ---- 2b. Reconstrucción de la variable de Lee (con su imputación, Apéndice A) ----
# (i) Valor de Lee: la fila de la elección siguiente del mismo distrito (misma
#     statedisdec) trae `demwinprev` ya procesada por Lee = victoria Dem. en t+1 de esta
#     fila. Se enlaza porque su `demshareprev` es igual a nuestro `demsharenext`.
#     Disponible para ~65% de la muestra (no existe cuando t+1 cruza el redistritaje).
# (ii) Si no hay enlace: ganador observado (partywinnext == 100).
# (iii) Si tampoco hay dato: promedio estado–año del ganador observado, la regla de
#     imputación de Lee; si aún falta, promedio estado–década (Apéndice A). En 8 casos
#     (estados sin ningún dato observado en esa década dentro de la muestra) se usa el
#     promedio estatal del periodo, porque Lee calculó sus promedios con datos que no son públicos.
#     Estado = statedisdec %/% 1000; década = floor(año / 10).
siguiente <- t2 |>
  filter(!is.na(demshareprev)) |>
  select(statedisdec, demsharenext = demshareprev, lee = demwinprev)
enlace <- dist |>
  select(.id, statedisdec, demsharenext) |>
  inner_join(siguiente, by = c("statedisdec", "demsharenext"), relationship = "many-to-many") |>
  group_by(.id) |>
  summarise(lee = if (n_distinct(lee) == 1) first(lee) else NA_real_, .groups = "drop")
dist_win <- dist_win |>
  left_join(enlace, by = ".id") |>
  mutate(estado = statedisdec %/% 1000) |>
  group_by(estado, yearel) |> mutate(prom_ea = mean(demwinnext_obs, na.rm = TRUE)) |>
  group_by(estado, decada = floor(yearel / 10)) |> mutate(prom_ed = mean(demwinnext_obs, na.rm = TRUE)) |>
  group_by(estado) |> mutate(prom_e = mean(demwinnext_obs, na.rm = TRUE)) |> ungroup() |>
  mutate(across(c(prom_ea, prom_ed, prom_e), \(v) ifelse(is.nan(v), NA, v)),
         fuente_demwinnext = case_when(!is.na(lee) ~ "valor de Lee (fila siguiente)",
                                       !is.na(demwinnext_obs) ~ "ganador observado",
                                       !is.na(prom_ea) ~ "imputado estado–año",
                                       !is.na(prom_ed) ~ "imputado estado–década",
                                       TRUE ~ "imputado estado (periodo)"),
         demwinnext_apendice = coalesce(lee, demwinnext_obs, prom_ea, prom_ed, prom_e),
         # Solución adoptada: demsharenext > 0.5 usa la variable ya depurada/imputada
         # por Lee en table_two_final (6558 obs., sin empates ambiguos ni 290 NAs de
         # group_final). Coincide en 98.4% de los casos observados y reproduce mejor
         # la Tabla 1 y la Figura 5a del artículo.
         demwinnext = as.numeric(demsharenext > 0.5)) |>
  select(-.id, -prom_ea, -prom_ed, -prom_e, -decada)
dist <- dist |> select(-.id)
stopifnot(!anyNA(dist_win$demwinnext))

# ---- 3. Muestra a nivel de candidato (Figuras 2–3) --------------------------
# Demócratas con elección t en cualquier año 1946–1996 (incluye años de redistritaje,
# porque a nivel candidato sí se sabe si vuelve a postular): N = 9674.
cand <- ind |>
  filter(use == 1) |>
  mutate(x = difshare, D = as.numeric(x > 0), bin = asignar_bin(x))
stopifnot(nrow(cand) == 9674)

# Para los promedios por intervalo de las Figuras 4–5 Lee usa todas las obs. use == 1,
# incluida la de margen 0 (6559 obs.); esa observación no entra en ninguna estimación.
dist_bins <- t2 |> filter(use == 1) |> mutate(x = difdemshare, bin = asignar_bin(x))
grp_bins  <- grp |> filter(use == 1) |>
  mutate(x = difdemshare, bin = asignar_bin(x),
         demwinnext = ifelse(is.na(partywinnext), NA_real_, as.numeric(partywinnext == 100)))
saveRDS(dist_bins, "output/intermedios/muestra_distrito_bins.rds")
saveRDS(grp_bins,  "output/intermedios/muestra_grupo_bins.rds")
saveRDS(dist,     "output/intermedios/muestra_distrito.rds")
saveRDS(dist_win, "output/intermedios/muestra_distrito_demwinnext.rds")
saveRDS(grp_u,    "output/intermedios/muestra_grupo.rds")
saveRDS(cand,     "output/intermedios/muestra_candidato.rds")

# ---- 4. Diccionario de datos (calculado) ------------------------------------
resumen <- function(df, var, archivo, unidad, desc) {
  v <- df[[var]]
  data.frame(variable = var, archivo = archivo, descripcion = desc, unidad = unidad,
             n_valido = sum(!is.na(v)), n_faltante = sum(is.na(v)),
             minimo = round(min(v, na.rm = TRUE), 4), media = round(mean(v, na.rm = TRUE), 4),
             maximo = round(max(v, na.rm = TRUE), 4))
}
dic <- bind_rows(
  resumen(dist, "difdemshare", "table_two_final", "proporción [-1,1]",
          "Margen de victoria demócrata en t: voto Dem. menos voto del oponente más fuerte (variable de asignación)"),
  resumen(dist, "right", "table_two_final", "0/1", "Tratamiento: el Demócrata gana en t (difdemshare > 0)"),
  resumen(dist, "demsharenext", "table_two_final", "proporción [0,1]", "Voto demócrata en t+1"),
  resumen(dist, "demshareprev", "table_two_final", "proporción [0,1]", "Voto demócrata en t-1 (predeterminada)"),
  resumen(dist, "difdemsharenext", "table_two_final", "proporción [-1,1]", "Voto demócrata t+1 menos voto demócrata t-1"),
  resumen(dist, "demwinprev", "table_two_final", "0/1 (1.5% imputado, fraccional)", "El partido Demócrata ganó en t-1 (predeterminada)"),
  resumen(dist, "demofficeexp", "table_two_final", "número de victorias", "Experiencia política del candidato Dem. (victorias acumuladas)"),
  resumen(dist, "othofficeexp", "table_two_final", "número de victorias", "Experiencia política del oponente"),
  resumen(dist, "demelectexp", "table_two_final", "número de postulaciones", "Experiencia electoral del candidato Dem. (postulaciones acumuladas)"),
  resumen(dist, "othelectexp", "table_two_final", "número de postulaciones", "Experiencia electoral del oponente"),
  resumen(dist, "statedisdec", "table_two_final", "código", "Conglomerado estado–distrito–década (errores estándar agrupados)"),
  resumen(dist_win, "demwinnext", "table_two_final (demsharenext > 0.5)", "0/1", "El partido Demócrata gana el escaño en t+1 (regla demsharenext > 0.5; evita 290 NAs y cruces ambiguos de group_final)"),
  resumen(cand, "difshare", "individ_final", "proporción [-1,1]", "Margen de victoria del candidato Dem. en t"),
  resumen(cand, "myoutcomenext", "individ_final", "0/1", "El candidato Dem. postula y gana en t+1"),
  resumen(cand, "mofficeexp", "individ_final", "victorias (promedio de intervalo)", "Promedio por intervalo de victorias pasadas del candidato (Fig. 2b; microdato no público)"),
  resumen(cand, "mrunagain", "individ_final", "proporción (promedio de intervalo)", "Promedio por intervalo de postular en t+1 (Fig. 3a; microdato no público)"),
  resumen(cand, "melectexp", "individ_final", "postulaciones (promedio de intervalo)", "Promedio por intervalo de postulaciones pasadas (Fig. 3b; microdato no público)")
)
dic$muestra <- ifelse(dic$archivo == "individ_final", "Candidatos Dem. 1946–1996 (N = 9674)",
                      "Distritos, t ∉ {años terminados en 0 o 2}, 1946–1996 (N = 6558)")
guardar_tabla(dic, "diccionario_datos")

cat(sprintf("Muestras: distrito N = %d (ganan %d / pierden %d); candidatos N = %d\n",
            nrow(dist), sum(dist$D == 1), sum(dist$D == 0), nrow(cand)))
cat("demwinnext reconstruida (fuente de cada observación):\n"); print(table(dist_win$fuente_demwinnext))
