# =============================================================================
# 05_figuras_2_a_5.R — Figuras 2 a 5: promedios locales y ajustes paramétricos
# Método de Lee (2008, sección 3.4): promedios en intervalos de 0.005 en [-0.25, 0.25];
# ajuste estimado con microdatos por separado a cada lado del umbral (polinomio de
# 4.º orden o logit con índice cuártico), evaluado en la media de x de cada intervalo.
# Los valores ajustados de Lee (variables mp* de los .dta) se usan SOLO para comparar.
# =============================================================================
source("scripts/00_utilidades.R")
dist  <- readRDS("output/intermedios/muestra_distrito.rds")
grp_u <- readRDS("output/intermedios/muestra_grupo.rds")
cand  <- readRDS("output/intermedios/muestra_candidato.rds")

# Definición de los 8 paneles.
# fuente: "micro" = variable individual disponible → todo se calcula.
#         "bin"   = el microdato no es público (solo el promedio por intervalo en
#                   individ_final): el promedio local se toma del archivo y el ajuste se
#                   re-estima con ese promedio asignado a cada observación. ADAPTACIÓN.
paneles <- list(
  `2a` = list(df = "cand", y = "myoutcomenext", logit = TRUE,  fuente = "micro", lee_m = "mmyoutcomenext", lee_p = "mpmyoutcomenext",
              titulo = "Figura 2a", eje = "Prob. de postular y ganar, elección t+1", ylim = c(0, 1), texto = 0.45),
  `2b` = list(df = "cand", y = "mofficeexp",    logit = FALSE, fuente = "bin",   lee_m = "mofficeexp",     lee_p = "mpofficeexp",
              titulo = "Figura 2b", eje = "N.º de victorias pasadas al momento de t", ylim = c(0, 5), texto = NA),
  `3a` = list(df = "cand", y = "mrunagain",     logit = TRUE,  fuente = "bin",   lee_m = "mrunagain",      lee_p = "mprunagain",
              titulo = "Figura 3a", eje = "Prob. de candidatura, elección t+1", ylim = c(0, 1), texto = 0.40),
  `3b` = list(df = "cand", y = "melectexp",     logit = FALSE, fuente = "bin",   lee_m = "melectexp",      lee_p = "mpelectexp",
              titulo = "Figura 3b", eje = "N.º de postulaciones pasadas al momento de t", ylim = c(0, 5), texto = NA),
  `4a` = list(df = "dist", y = "demsharenext",  logit = FALSE, fuente = "micro", lee_m = "mdemsharenext",  lee_p = "mpdemsharenext",
              titulo = "Figura 4a", eje = "Voto Dem., elección t+1", ylim = c(0.3, 0.7), texto = 0.077),
  `4b` = list(df = "dist", y = "demshareprev",  logit = FALSE, fuente = "micro", lee_m = "mdemshareprev",  lee_p = "mpdemshareprev",
              titulo = "Figura 4b", eje = "Voto Dem., elección t-1", ylim = c(0.3, 0.7), texto = 0),
  `5a` = list(df = "win",  y = "demwinnext",    logit = TRUE,  fuente = "micro", lee_m = "mdemwinnext",    lee_p = "mpdemwinnext",
              titulo = "Figura 5a", eje = "Prob. de victoria Dem., elección t+1", ylim = c(0, 1), texto = 0.35),
  `5b` = list(df = "dist", y = "demwinprev",    logit = TRUE,  fuente = "micro", lee_m = "mdemwinprev",    lee_p = "mpdemwinprev",
              titulo = "Figura 5b", eje = "Prob. de victoria Dem., elección t-1", ylim = c(0, 1), texto = 0))

dist_win <- readRDS("output/intermedios/muestra_distrito_demwinnext.rds")
datos <- list(cand = cand, dist = dist, grp = grp_u, win = dist_win)
datos_bins <- list(cand = cand,
                   dist = readRDS("output/intermedios/muestra_distrito_bins.rds"),
                   grp  = readRDS("output/intermedios/muestra_grupo_bins.rds"),
                   win  = dist_win)
# Referencia de Lee por intervalo (promedios y ajustes de los .dta)
ref_cand <- cand  |> group_by(bin) |> summarise(across(c(mmyoutcomenext, mofficeexp, mrunagain, melectexp,
              mpmyoutcomenext, mpofficeexp, mprunagain, mpelectexp), first), .groups = "drop")
ref_grp  <- grp_u |> group_by(bin) |> summarise(across(c(mdemsharenext, mdemwinnext, mdemshareprev, mdemwinprev,
              mpdemsharenext, mpdemwinnext, mpdemshareprev, mpdemwinprev), first), .groups = "drop")

graficos <- list(); resumen <- list(); puntos <- list()
for (k in names(paneles)) {
  p  <- paneles[[k]]
  df <- datos[[p$df]]
  y  <- df[[p$y]]
  modelo <- ajuste_por_lado(y, df$x, logit = p$logit, convencion_stata = p$fuente == "micro")
  db     <- datos_bins[[p$df]]
  tab    <- tabla_figura(db[[p$y]], db$x, modelo)
  ref    <- if (p$df == "cand") ref_cand else ref_grp
  tab    <- tab |> left_join(ref |> select(bin, lee_prom = all_of(p$lee_m), lee_ajuste = all_of(p$lee_p)), by = "bin")
  puntos[[k]] <- tab |> mutate(panel = k)

  etiqueta <- if (p$logit) "Ajuste logit" else "Ajuste polinomial"
  g <- grafico_rd(tab, p$titulo, p$eje, etiqueta, p$ylim)
  if (p$fuente == "bin") g <- g + labs(caption = "Adaptación: microdato no público; promedios por intervalo del archivo de Lee")
  ggsave(sprintf("output/figures/figura%s.png", k), g, width = 5.5, height = 4, dpi = 300)
  graficos[[k]] <- g

  resumen[[k]] <- data.frame(
    panel = k, variable = p$y, ajuste = etiqueta, fuente = p$fuente,
    salto_calculado = salto_umbral(modelo),
    salto_texto = p$texto,
    salto_lee_ajuste = with(tab, lee_ajuste[bin == 200] - lee_ajuste[bin == 199]),
    salto_calculado_bins = with(tab, ajuste[bin == 200] - ajuste[bin == 199]),
    max_dif_promedio = max(abs(tab$promedio - tab$lee_prom)),
    max_dif_ajuste = max(abs(tab$ajuste - tab$lee_ajuste)),
    n = sum(!is.na(y) & df$x != 0))
}
res <- bind_rows(resumen)
guardar_tabla(res, "figuras_2_5_resumen")
guardar_tabla(bind_rows(puntos) |> relocate(panel), "figuras_2_5_puntos")
saveRDS(res, "output/intermedios/comparacion_figuras.rds")
print(res |> mutate(across(where(is.numeric), \(v) round(v, 4))))

for (f in c("2", "3", "4", "5")) {
  ggsave(sprintf("output/figures/figura%s_completa.png", f),
         arrangeGrob(graficos[[paste0(f, "a")]], graficos[[paste0(f, "b")]], ncol = 2),
         width = 11, height = 4.2, dpi = 300)
}
