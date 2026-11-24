# =============================================================================
# 07_cuadro_comparacion.R — Cuadro «publicado / replicado / diferencia / explicación»
# Reúne las comparaciones calculadas en los scripts 02–05. Ninguna cifra replicada se
# escribe a mano: todas vienen de output/intermedios/*.rds.
# =============================================================================
source("scripts/00_utilidades.R")

t1 <- readRDS("output/intermedios/comparacion_tabla1.rds")
t2 <- readRDS("output/intermedios/comparacion_tabla2.rds")
fg <- readRDS("output/intermedios/comparacion_figuras.rds")
f1 <- readRDS("output/intermedios/comparacion_figura1.rds")
tw <- readRDS("output/intermedios/tabla2_variante_victoria_t1.rds")
va <- readRDS("output/intermedios/validez.rds")

tolerancia <- 0.0005   # medio punto en el 3.er decimal (precisión de lo publicado)
veredicto <- function(d, tol = tolerancia) ifelse(abs(d) < tol, "Coincide", "Difiere")

expl_t1 <- function(var, est) {
  if (var == "demwinnext")
    return("Solución adoptada: indicador demsharenext > 0.5 sobre la variable depurada de Lee (evita los 743 empates ambiguos al cruzar group_final y sus 290 NAs). Acierta 98.4% frente a partywinnext observado; brecha residual por 102 elecciones con terceros partidos e imputaciones internas de Lee.")
  if (est == "Ajuste paramétrico" && var == "demwinprev")
    return("Logit con índice cuártico; convención de Stata (y ≠ 0 = éxito) sobre la variable con 1.5% de valores imputados.")
  "Mismo dato, muestra y estimador."
}

c_t1 <- t1 |>
  rowwise() |>
  mutate(explicacion = expl_t1(variable, muestra)) |>
  ungroup() |>
  transmute(resultado = "Tabla 1", detalle = paste(etiqueta, "—", muestra, "—", lado),
            publicado = media_pub, replicado = media, diferencia = dif_media,
            ee_publicado = ee_pub, ee_replicado = ee, de_publicado = de_pub, de_replicado = de,
            n_publicado = n_pub, n_replicado = n,
            estado = veredicto(dif_media), explicacion)

etq2 <- c(D = "Victoria, elección t", demshareprev = "Voto Dem. t-1", demwinprev = "Victoria Dem. t-1",
          demofficeexp = "Exp. política Dem.", othofficeexp = "Exp. política oponente",
          demelectexp = "Exp. electoral Dem.", othelectexp = "Exp. electoral oponente")
c_t2 <- t2 |>
  transmute(resultado = "Tabla 2", detalle = paste0("Col. (", sub("c", "", columna), ") — ", etq2[termino]),
            publicado = coef_pub, replicado = coef, diferencia = round(coef, 3) - coef_pub,
            ee_publicado = ee_pub, ee_replicado = ee, n_publicado = 6558, n_replicado = n,
            estado = ifelse(abs(diferencia) < 1e-9 & abs(round(ee, 3) - ee_pub) < 1e-9,
                            "Coincide (3 decimales, coef. y EE)", "Difiere"),
            explicacion = "Mismo dato y especificación que public_use.do; EE agrupados estado–distrito–década (tipo Stata).")

expl_fig <- c(
  `2a` = "Microdato disponible: logit cuártico a cada lado, evaluado como Lee.",
  `2b` = "ADAPTACIÓN: el microdato de victorias pasadas del candidato no es público; el ajuste se estima con el promedio por intervalo de Lee.",
  `3a` = "ADAPTACIÓN: el microdato de 'postular en t+1' no es público; logit sobre el promedio por intervalo. El texto da ≈0.40 (y 0.43 en la intro.).",
  `3b` = "ADAPTACIÓN: el microdato de postulaciones pasadas no es público; ajuste sobre el promedio por intervalo.",
  `4a` = "Microdato disponible: polinomio cuártico a cada lado.",
  `4b` = "Microdato disponible (placebo t-1): polinomio cuártico a cada lado.",
  `5a` = "Solución adoptada: victoria t+1 medida con demsharenext > 0.5 (evita 290 NAs y cruces ambiguos de group_final); difiere levemente en elecciones con terceros partidos.",
  `5b` = "Microdato disponible (placebo t-1): logit con convención de Stata.")
c_fg <- bind_rows(
  fg |> transmute(resultado = paste("Figura", panel), detalle = paste("Salto en el umbral (ajuste de Lee en .dta) —", ajuste),
                  publicado = salto_lee_ajuste, replicado = salto_calculado_bins,
                  diferencia = replicado - publicado, ee_publicado = NA, ee_replicado = NA,
                  n_publicado = NA, n_replicado = n, estado = veredicto(diferencia), explicacion = expl_fig[panel]),
  fg |> filter(!is.na(salto_texto)) |>
    transmute(resultado = paste("Figura", panel), detalle = "Salto reportado en el texto (aprox.)",
              publicado = salto_texto, replicado = salto_calculado, diferencia = replicado - publicado,
              ee_publicado = NA, ee_replicado = NA, n_publicado = NA, n_replicado = n,
              estado = ifelse(abs(diferencia) < 0.02, "Coincide (cifra aproximada del texto)", "Difiere"),
              explicacion = paste("El texto redondea («about»).", expl_fig[panel])),
  fg |> transmute(resultado = paste("Figura", panel), detalle = "Máx. |dif.| promedios locales / ajustes por intervalo",
                  publicado = 0, replicado = pmax(max_dif_promedio, max_dif_ajuste),
                  diferencia = replicado, ee_publicado = NA, ee_replicado = NA, n_publicado = NA,
                  n_replicado = n, estado = veredicto(diferencia, 1e-4), explicacion = expl_fig[panel]))

c_fg <- c_fg |> mutate(estado = ifelse(resultado %in% paste("Figura", c("2b", "3a", "3b")) & estado == "Difiere",
                                       "Aproximado (adaptación por datos no públicos)", estado))

c_f1 <- data.frame(resultado = "Figura 1",
  detalle = c("Partido incumbente (máx. |dif.| anual)", "Candidato ganador (máx. |dif.| anual)", "Candidato en 2.º lugar (máx. |dif.| anual)"),
  publicado = 0, replicado = c(max(abs(f1$dif_partido), na.rm = TRUE), max(abs(f1$dif_ganador), na.rm = TRUE),
                               max(abs(f1$dif_segundo), na.rm = TRUE)),
  ee_publicado = NA, ee_replicado = NA, n_publicado = NA, n_replicado = NA,
  explicacion = "Calculado desde group_final e individ_final; figure1.dta solo como referencia.") |>
  mutate(diferencia = replicado, estado = veredicto(diferencia, 1e-4))

p1 <- readRDS("output/intermedios/figura1_promedios_texto.rds")
c_f1t <- data.frame(resultado = "Figura 1", detalle = paste("Promedio 1948–1998 citado en el texto —", p1$serie),
  publicado = p1$texto, replicado = p1$calculado, ee_publicado = NA, ee_replicado = NA, n_publicado = NA, n_replicado = NA,
  explicacion = c("El texto dice «about 90%».", "El texto dice «80 percent».",
                  "El texto dice «3% chance»; la serie de la figura (idéntica a figure1.dta) promedia menos: cifra redondeada del autor.")) |>
  mutate(diferencia = replicado - publicado,
         estado = ifelse(abs(diferencia) < 0.01, "Coincide (cifra aproximada del texto)", "Difiere (cifra aproximada del texto)"))

c_tw <- data.frame(resultado = "Texto, secc. 3.4", detalle = c("Tabla 2 con victoria t+1, col. (1)", "Tabla 2 con victoria t+1, col. (8) placebo"),
  publicado = c(0.38, -0.005), replicado = tw$coef[c(1, 8)], ee_publicado = c(NA, 0.033), ee_replicado = tw$ee[c(1, 8)],
  n_publicado = 6558, n_replicado = tw$n[c(1, 8)],
  explicacion = c("Variable de victoria t+1 reconstruida con la imputación de Lee (ver Tabla 1). La conclusión no cambia: efecto grande y significativo.",
                  "El placebo pasa igual: no hay salto en t-1. La especificación de controles de esta columna no se publica; se usan voto t-1 y experiencias.")) |>
  mutate(diferencia = replicado - publicado,
         estado = ifelse(abs(diferencia) < c(0.02, 0.0005), c("Coincide (cifra aproximada del texto: «about 0.38»)", "Coincide"), "Difiere"))

ll <- va$ll
c_ll <- data.frame(resultado = "Extensión (rúbrica C3)", detalle = sprintf("Local lineal triangular, h = %.2f", ll$h),
  publicado = 0.077, replicado = ll$tau, ee_publicado = 0.011, ee_replicado = ll$ee, n_publicado = 6558, n_replicado = ll$n,
  explicacion = "ADAPTACIÓN DIDÁCTICA: estimador distinto (local lineal, Hahn et al. 2001). La diferencia frente a 0.077 refleja el estimador y la ventana, no un error.") |>
  mutate(diferencia = replicado - publicado, estado = "Comparación de estimadores")

cuadro <- bind_rows(c_f1, c_f1t, c_t1, c_t2, c_fg, c_tw, c_ll) |>
  mutate(across(c(publicado, replicado, diferencia, ee_publicado, ee_replicado, de_publicado, de_replicado), \(v) round(v, 4)))
guardar_tabla(cuadro, "comparacion_publicado_replicado")

res <- cuadro |> group_by(resultado) |>
  summarise(comparaciones = n(), coinciden = sum(grepl("^Coincide", estado)), .groups = "drop")
guardar_tabla(res, "comparacion_resumen")
print(as.data.frame(res))
