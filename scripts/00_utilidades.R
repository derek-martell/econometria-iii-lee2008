# =============================================================================
# 00_utilidades.R — Funciones comunes de la réplica de Lee (2008)
# Se carga desde cada script con source("scripts/00_utilidades.R").
# =============================================================================

# ---- 0. Comprobaciones de entorno -------------------------------------------
if (!file.exists("data/Lee2008/table_two_final.dta")) {
  stop("Ejecute los scripts desde la raíz del proyecto (carpeta que contiene ",
       "data/, scripts/ y output/). Abra lee2008_replication.Rproj o use setwd().")
}

paquetes_base <- c("haven", "dplyr", "ggplot2", "readr", "gridExtra", "sandwich", "openxlsx")
faltan <- paquetes_base[!vapply(paquetes_base, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) {
  message("Instalando paquetes faltantes: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2); library(readr)
  library(gridExtra); library(sandwich)
})

for (d in c("output/figures", "output/intermedios/tablas")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

RUTA_DATOS <- "data/Lee2008"
ANCHO_BIN  <- 0.005          # ancho de intervalo usado por Lee (sección 3.4)

# ---- 1. Asignación a intervalos (bins) --------------------------------------
# Lee agrupa el margen x ∈ [-1, 1] en 400 intervalos de 0.005: difgrp = floor((x+1)/0.005).
# Se verificó que esta fórmula reproduce el 100% de la variable difgrp de los archivos.
asignar_bin <- function(x) floor((x + 1) / ANCHO_BIN)

# ---- 2. Polinomio de 4.º orden a cada lado del umbral -----------------------
# Especificación de Lee (2008, Tablas 1–2): y = a + b·D + Σ_k c_k x^k + Σ_k d_k D·x^k
# con D = 1{x > 0}. Equivale a ajustar un polinomio cuártico distinto a cada lado.
formula_cuartica <- function(y, controles = NULL) {
  rhs <- "D * (x + I(x^2) + I(x^3) + I(x^4))"
  if (length(controles) > 0) rhs <- paste(rhs, "+", paste(controles, collapse = " + "))
  as.formula(paste(y, "~", rhs))
}

# Errores estándar agrupados equivalentes a Stata `regress ..., cluster()`:
# vcovCL con type = "HC1" aplica el ajuste G/(G-1)·(N-1)/(N-K).
ee_cluster <- function(modelo, cluster) {
  sqrt(diag(sandwich::vcovCL(modelo, cluster = cluster, type = "HC1")))
}

# Valor del ajuste en x = 0 por un lado (columnas 7–8 de la Tabla 1): regresión
# cuártica estimada solo con ese lado del umbral. Para variables binarias Lee usa un
# logit con índice cuártico (EE agrupados por método delta); se aplica la convención de
# Stata `logit`: todo valor distinto de 0 cuenta como éxito (variables imputadas fraccionales).
valor_en_umbral <- function(y, x, cluster, lado = c("ganador", "perdedor"), logit = FALSE) {
  lado <- match.arg(lado)
  s <- if (lado == "ganador") x > 0 else x < 0
  s <- s & !is.na(y)
  d <- data.frame(y = y[s], x = x[s])
  if (!logit) {
    m <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = d)
    return(c(estimado = unname(coef(m)[1]), ee = unname(ee_cluster(m, cluster[s])[1])))
  }
  d$y <- as.numeric(d$y != 0)
  m <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), family = binomial(), data = d)
  p <- plogis(unname(coef(m)[1]))
  V <- sandwich::vcovCL(m, cluster = cluster[s], type = "HC1")
  c(estimado = p, ee = p * (1 - p) * sqrt(V[1, 1]))
}

# ---- 3. Ajustes de las Figuras 2–5 ------------------------------------------
# Lee estima con microdatos un polinomio cuártico (o un logit con índice cuártico)
# por separado a cada lado del umbral, y evalúa el ajuste en la media de x de cada
# intervalo de 0.005. Se excluye la única observación con x = 0 (sin lado definido).
# convencion_stata = TRUE: en el logit, todo y != 0 es éxito (como `logit` de Stata).
# Se desactiva solo cuando y es un promedio por intervalo (adaptación de las Fig. 2b–3b).
ajuste_por_lado <- function(y, x, logit = FALSE, convencion_stata = TRUE) {
  ok <- !is.na(y) & !is.na(x) & x != 0
  d  <- data.frame(y = y[ok], x = x[ok], D = as.numeric(x[ok] > 0))
  if (logit && convencion_stata) d$y <- as.numeric(d$y != 0)
  f  <- y ~ D * (x + I(x^2) + I(x^3) + I(x^4))
  if (logit) suppressWarnings(glm(f, family = quasibinomial(), data = d)) else lm(f, data = d)
}

predecir <- function(modelo, x) {
  as.numeric(predict(modelo, newdata = data.frame(x = x, D = as.numeric(x > 0)),
                     type = "response"))
}

# Salto estimado en el umbral: límite por la derecha menos límite por la izquierda.
salto_umbral <- function(modelo, eps = 1e-10) {
  predecir(modelo, eps) - predecir(modelo, -eps)
}

# Promedios locales por intervalo y ajuste por intervalo = promedio de las predicciones
# individuales (así lo hace Lee: la obs. con x = 0 entra al intervalo 200 y se predice
# con el lado izquierdo, D = 0). x/y deben incluir todas las observaciones use == 1.
tabla_figura <- function(y, x, modelo, rango = 0.25) {
  ok <- !is.na(y) & !is.na(x)
  data.frame(x = x[ok], y = y[ok]) |>
    mutate(bin = asignar_bin(x), pred = predecir(modelo, x)) |>
    group_by(bin) |>
    summarise(x_media = mean(x), promedio = mean(y), ajuste = mean(pred), n = n(),
              .groups = "drop") |>
    filter(x_media > -rango - 0.001, x_media < rango + 0.001)
}

grafico_rd <- function(tab, titulo, eje_y, etiqueta_ajuste, ylim) {
  tab$lado <- tab$x_media > 0
  ggplot(tab, aes(x_media)) +
    geom_point(aes(y = promedio, shape = "Promedio local"), size = 1.6) +
    geom_line(aes(y = ajuste, group = lado, linetype = etiqueta_ajuste), linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    scale_shape_manual(NULL, values = 16) +
    scale_linetype_manual(NULL, values = "solid") +
    scale_x_continuous(breaks = seq(-0.25, 0.25, 0.05), limits = c(-0.255, 0.255)) +
    coord_cartesian(ylim = ylim) +
    labs(title = titulo, x = "Margen de victoria demócrata, elección t", y = eje_y) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
}

# ---- 4. Regresión local lineal (extensión de la rúbrica, opción C3) ---------
# Kernel triangular, pendiente distinta a cada lado, EE agrupados.
local_lineal <- function(y, x, h, cluster = NULL, kernel = c("triangular", "uniforme"),
                         dona = 0) {
  kernel <- match.arg(kernel)
  ok <- !is.na(y) & abs(x) < h & x != 0 & abs(x) >= dona
  d  <- data.frame(y = y[ok], x = x[ok], D = as.numeric(x[ok] > 0))
  w  <- if (kernel == "triangular") 1 - abs(d$x) / h else rep(1, nrow(d))
  m  <- lm(y ~ D * x, data = d, weights = w)
  ee <- if (is.null(cluster)) sqrt(diag(sandwich::vcovHC(m, type = "HC1")))
        else ee_cluster(m, cluster[ok])
  c(tau = unname(coef(m)["D"]), ee = unname(ee["D"]), n = nrow(d),
    n_izq = sum(d$D == 0), n_der = sum(d$D == 1))
}

# ---- 5. Prueba de densidad de McCrary (2008) --------------------------------
# Implementación del procedimiento original (Journal of Econometrics 142(2)):
# (i) histograma fino con ancho b = 2·σ·n^(-1/2); (ii) regresión local lineal con
# kernel triangular de las alturas del histograma a cada lado del umbral;
# (iii) θ = ln f+ − ln f−, con EE = sqrt( (1/(n·h))·(24/5)·(1/f+ + 1/f−) ).
# Ancho de banda h: regla práctica de McCrary (polinomio de 4.º grado al histograma).
prueba_mccrary <- function(x, c = 0, b = NULL, h = NULL) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (is.null(b)) b <- 2 * sd(x) * n^(-1/2)
  l <- floor((min(x) - c) / b) * b + b / 2 + c
  r <- floor((max(x) - c) / b) * b + b / 2 + c
  j <- floor((x - c) / b) * b + b / 2 + c
  centros <- seq(l, r, by = b)
  frec <- as.numeric(table(factor(round(j, 10), levels = round(centros, 10))))
  hist <- data.frame(z = centros, f = frec / (n * b))
  izq <- hist[hist$z < c, ]; der <- hist[hist$z >= c, ]
  if (is.null(h)) {
    regla <- function(dd) {
      m <- lm(f ~ z + I(z^2) + I(z^3) + I(z^4), data = dd)
      s2 <- sum(residuals(m)^2) / (nrow(dd) - 5)
      f2 <- 2 * coef(m)[3] + 6 * coef(m)[4] * dd$z + 12 * coef(m)[5] * dd$z^2
      3.348 * (s2 * (max(dd$z) - min(dd$z)) / sum(f2^2))^(1/5)
    }
    h <- mean(c(regla(izq), regla(der)))
  }
  f_lado <- function(dd) {
    dd <- dd[abs(dd$z - c) <= h, ]
    w <- pmax(0, 1 - abs(dd$z - c) / h)
    unname(coef(lm(f ~ I(z - c), data = dd, weights = w))[1])
  }
  fm <- f_lado(izq); fp <- f_lado(der)
  theta <- log(fp) - log(fm)
  ee <- sqrt((1 / (n * h)) * (24 / 5) * ((1 / fp) + (1 / fm)))
  list(theta = theta, ee = ee, z = theta / ee, p = 2 * pnorm(-abs(theta / ee)),
       b = b, h = h, f_izq = fm, f_der = fp, histograma = hist)
}

# Todas las tablas se guardan como objetos intermedios y 08_exportar_excel.R las
# reúne en un único libro: output/resultados_Lee2008.xlsx (una hoja por tabla).
guardar_tabla <- function(df, clave) {
  saveRDS(as.data.frame(df), file.path("output/intermedios/tablas", paste0(clave, ".rds")))
}

fmt <- function(v, k = 3) formatC(v, format = "f", digits = k)
