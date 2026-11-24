# =============================================================================
# 03_tabla1.R — Tabla 1: ganadores vs. perdedores demócratas (balance y saltos)
# Todas las cifras se calculan con los microdatos. table_one_final.dta contiene
# los valores publicados por Lee y se usa SOLO como columna de comparación.
# =============================================================================
source("scripts/00_utilidades.R")
dist     <- readRDS("output/intermedios/muestra_distrito.rds")
dist_win <- readRDS("output/intermedios/muestra_distrito_demwinnext.rds")

variables <- c(demsharenext = "Voto Dem. en t+1",
               demwinnext   = "Prob. de victoria Dem. en t+1",
               demshareprev = "Voto Dem. en t-1",
               demwinprev   = "Prob. de victoria Dem. en t-1",
               demofficeexp = "Experiencia política Dem.",
               othofficeexp = "Experiencia política oponente",
               demelectexp  = "Experiencia electoral Dem.",
               othelectexp  = "Experiencia electoral oponente")

muestras <- list(Todas = Inf, `|Margen|<0.5` = 0.5, `|Margen|<0.05` = 0.05)

celda <- function(v) { v <- v[!is.na(v)]; c(media = mean(v), ee = sd(v) / sqrt(length(v)), de = sd(v), n = length(v)) }

filas <- list()
for (var in names(variables)) {
  df <- if (var == "demwinnext") dist_win else dist
  for (m in names(muestras)) {
    sub <- df[abs(df$x) < muestras[[m]], ]
    for (lado in c("Ganador", "Perdedor")) {
      v <- sub[[var]][sub$D == (lado == "Ganador")]
      r <- celda(v)
      filas[[length(filas) + 1]] <- data.frame(variable = var, etiqueta = variables[[var]],
        muestra = m, lado = lado, media = r[["media"]], ee = r[["ee"]], de = r[["de"]], n = r[["n"]])
    }
  }
  # Ajuste paramétrico: polinomio de 4.º orden a cada lado, evaluado en x = 0
  # (logit con índice cuártico para las dos variables de victoria, como en las Fig. 5a–5b).
  binaria <- var %in% c("demwinnext", "demwinprev")
  for (lado in c("Ganador", "Perdedor")) {
    r <- valor_en_umbral(df[[var]], df$x, df$statedisdec, tolower(lado), logit = binaria)
    filas[[length(filas) + 1]] <- data.frame(variable = var, etiqueta = variables[[var]],
      muestra = "Ajuste paramétrico", lado = lado, media = r[["estimado"]], ee = r[["ee"]],
      de = NA, n = sum(!is.na(df[[var]]) & df$D == (lado == "Ganador")))
  }
}
tabla1 <- bind_rows(filas)
guardar_tabla(tabla1, "tabla1_calculada")

# ---- Comparación con los valores publicados ---------------------------------
pub <- read_dta(file.path(RUTA_DATOS, "table_one_final.dta"))
col_pub <- c("Todas.Ganador" = "Col_1", "Todas.Perdedor" = "Col_2",
             "|Margen|<0.5.Ganador" = "Col_3", "|Margen|<0.5.Perdedor" = "Col_4",
             "|Margen|<0.05.Ganador" = "Col_5", "|Margen|<0.05.Perdedor" = "Col_6",
             "Ajuste paramétrico.Ganador" = "Col_7", "Ajuste paramétrico.Perdedor" = "Col_8")
valor_pub <- function(var, sufijo, celda) {
  fila <- pub[pub$Variable == paste0(var, sufijo), ]
  if (nrow(fila) == 0) return(NA_real_)
  as.numeric(fila[[col_pub[[celda]]]])
}
comp <- tabla1 |>
  rowwise() |>
  mutate(celda = paste(muestra, lado, sep = "."),
         media_pub = valor_pub(variable, "m", celda),
         ee_pub    = valor_pub(variable, "se", celda),
         de_pub    = valor_pub(variable, "sd", celda),
         n_pub     = as.numeric(pub[pub$Variable == "obs", col_pub[[celda]]])) |>
  ungroup() |>
  mutate(dif_media = media - media_pub, dif_ee = ee - ee_pub, dif_de = de - de_pub)
saveRDS(comp, "output/intermedios/comparacion_tabla1.rds")

ok <- comp$variable != "demwinnext"
cat(sprintf("Tabla 1 (7 variables con microdato completo): máx |dif media| = %.1e; máx |dif EE| = %.1e\n",
            max(abs(comp$dif_media[ok])), max(abs(comp$dif_ee[ok]))))
cat(sprintf("Tabla 1 fila demwinnext: máx |dif media| = %.3f (variable imputada de Lee reconstruida)\n",
            max(abs(comp$dif_media[!ok]))))

# Salida en formato del artículo (media, (EE), [DE])
ancho <- reshape(as.data.frame(tabla1 |> mutate(col = paste(muestra, lado, sep = " – "),
                 texto = ifelse(is.na(de), sprintf("%s (%s)", fmt(media), fmt(ee)),
                                sprintf("%s (%s) [%s]", fmt(media), fmt(ee), fmt(de)))) |>
                 select(etiqueta, col, texto)),
                 idvar = "etiqueta", timevar = "col", direction = "wide")
names(ancho) <- sub("^texto\\.", "", names(ancho))
obs <- tabla1 |> filter(variable == "demsharenext") |> mutate(col = paste(muestra, lado, sep = " – "))
ancho <- rbind(ancho, c("Observaciones", obs$n[match(names(ancho)[-1], obs$col)]))
guardar_tabla(ancho, "tabla1_formato_articulo")
