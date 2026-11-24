# =============================================================================
# 06_validez_y_extensiones.R — Pruebas de validez del RDD y extensiones
# ADAPTACIÓN DIDÁCTICA (no es parte del artículo): la rúbrica (opción C3) pide estimar
# el salto con RDD local lineal y contrastar la sensibilidad al ancho de banda.
#   A. Local lineal (kernel triangular, EE agrupados) con base R: rejilla de anchos.
#   B. Balance de covariables predeterminadas y placebo t-1 con local lineal.
#   C. Umbrales placebo (cortes falsos) y orden del polinomio global (1 a 4).
#   D. Prueba de densidad de McCrary (2008) — Lee solo la sugiere (nota 19).
#   E. rdrobust / rddensity (Calonico–Cattaneo–Titiunik): ancho óptimo, IC robusto
#      con corrección de sesgo, p = 2, kernel uniforme y "dona". Se ejecuta si los
#      paquetes están instalados (se instalan desde CRAN si falta alguno).
# =============================================================================
source("scripts/00_utilidades.R")
dist <- readRDS("output/intermedios/muestra_distrito.rds")
y <- dist$demsharenext; x <- dist$x; cl <- dist$statedisdec

# ---- A. Local lineal con rejilla de anchos de banda --------------------------
anchos <- c(0.02, 0.03, 0.05, 0.07, 0.10, 0.15, 0.20, 0.25, 0.50)
ll <- bind_rows(lapply(anchos, function(h) {
  r <- local_lineal(y, x, h, cl)
  data.frame(h = h, tau = r[["tau"]], ee = r[["ee"]], ic_inf = r[["tau"]] - 1.96 * r[["ee"]],
             ic_sup = r[["tau"]] + 1.96 * r[["ee"]], n = r[["n"]], n_izq = r[["n_izq"]], n_der = r[["n_der"]])
}))
guardar_tabla(ll, "local_lineal_anchos")

g_bw <- ggplot(ll, aes(h, tau)) +
  geom_hline(yintercept = 0.077, linetype = "dashed", colour = "firebrick") +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.008) + geom_point(size = 2) +
  annotate("text", x = 0.38, y = 0.081, label = "Lee (2008), polinomio 4.º: 0.077", colour = "firebrick", size = 3) +
  labs(title = "Sensibilidad del salto local lineal al ancho de banda",
       x = "Ancho de banda h (margen de victoria)", y = "Salto en voto Dem. t+1 (IC 95%)") +
  theme_bw(base_size = 10)
ggsave("output/figures/sensibilidad_ancho_banda.png", g_bw, width = 6.5, height = 4, dpi = 300)

# ---- B. Balance de covariables y placebo con local lineal --------------------
predet <- c(demshareprev = "Voto Dem. t-1", demwinprev = "Victoria Dem. t-1",
            demofficeexp = "Exp. política Dem.", othofficeexp = "Exp. política oponente",
            demelectexp = "Exp. electoral Dem.", othelectexp = "Exp. electoral oponente")
balance <- bind_rows(lapply(c(0.05, 0.10, 0.25), function(h) bind_rows(lapply(names(predet), function(v) {
  r <- local_lineal(dist[[v]], x, h, cl)
  data.frame(variable = v, etiqueta = predet[[v]], h = h, salto = r[["tau"]], ee = r[["ee"]],
             p_valor = 2 * pnorm(-abs(r[["tau"]] / r[["ee"]])), n = r[["n"]])
}))))
guardar_tabla(balance, "balance_local_lineal")

# ---- C1. Umbrales placebo: el salto solo debería aparecer en x = 0 -----------
cortes <- c(-0.20, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.20)
placebo <- bind_rows(lapply(cortes, function(c0) {
  lado <- if (c0 < 0) x < 0 else if (c0 > 0) x > 0 else rep(TRUE, length(x))
  r <- local_lineal(y[lado], x[lado] - c0, 0.10, cl[lado])
  data.frame(corte = c0, salto = r[["tau"]], ee = r[["ee"]],
             p_valor = 2 * pnorm(-abs(r[["tau"]] / r[["ee"]])), n = r[["n"]])
}))
guardar_tabla(placebo, "umbrales_placebo")

# ---- C2. Orden del polinomio global (Lee usa 4) -------------------------------
orden <- bind_rows(lapply(1:4, function(p) {
  f <- as.formula(paste("demsharenext ~ D * (", paste0("I(x^", 1:p, ")", collapse = " + "), ")"))
  m <- lm(f, data = dist)
  data.frame(orden = p, salto = unname(coef(m)["D"]), ee = unname(ee_cluster(m, cl)["D"]),
             aic = AIC(m), n = nobs(m))
}))
guardar_tabla(orden, "orden_polinomio_global")

# ---- D. Prueba de densidad de McCrary (2008) ----------------------------------
mc <- prueba_mccrary(x)
mccrary <- data.frame(prueba = "McCrary (2008), implementación propia",
                      theta_log_dif = mc$theta, ee = mc$ee, z = mc$z, p_valor = mc$p,
                      ancho_bin = mc$b, ancho_banda = mc$h, densidad_izq = mc$f_izq, densidad_der = mc$f_der)
hist_d <- mc$histograma |> filter(abs(z) <= 0.5)
g_dens <- ggplot(hist_d, aes(z, f)) + geom_point(size = 0.9, colour = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = sprintf("Densidad del margen de victoria (McCrary: θ = %.3f, EE = %.3f, p = %.2f)",
                       mc$theta, mc$ee, mc$p),
       x = "Margen de victoria demócrata, elección t", y = "Densidad (histograma)") +
  theme_bw(base_size = 10)
ggsave("output/figures/densidad_mccrary.png", g_dens, width = 6.5, height = 4, dpi = 300)

# ---- E. rdrobust / rddensity (CCT) --------------------------------------------
for (p in c("rdrobust", "rddensity")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Instalando ", p, " desde CRAN...")
    try(install.packages(p, repos = "https://cloud.r-project.org"), silent = TRUE)
  }
}
bloque_rdrobust <- function() {
  fila_rd <- function(fit, especificacion) {
    data.frame(especificacion = especificacion, h = fit$bws[1, 1], tau_convencional = fit$coef[1],
               ee_convencional = fit$se[1], tau_corregido = fit$coef[3], ee_robusto = fit$se[3],
               ic_robusto_inf = fit$ci[3, 1], ic_robusto_sup = fit$ci[3, 2],
               p_robusto = fit$pv[3], n_efectivo = sum(fit$N_h))
  }
  ok <- x != 0
  cct <- list(
    fila_rd(rdrobust::rdrobust(y[ok], x[ok], c = 0, p = 1, kernel = "triangular", cluster = cl[ok]),
            "Local lineal, triangular, h óptimo MSE (CCT)"),
    fila_rd(rdrobust::rdrobust(y[ok], x[ok], c = 0, p = 2, kernel = "triangular", cluster = cl[ok]),
            "Local cuadrático, triangular, h óptimo"),
    fila_rd(rdrobust::rdrobust(y[ok], x[ok], c = 0, p = 1, kernel = "uniform", cluster = cl[ok]),
            "Local lineal, kernel uniforme, h óptimo"),
    fila_rd(rdrobust::rdrobust(y[ok], x[ok], c = 0, p = 1, kernel = "triangular", cluster = cl[ok],
                               bwselect = "cerrd"), "Local lineal, h óptimo CER"))
  dona <- abs(x) >= 0.005
  cct[[5]] <- fila_rd(rdrobust::rdrobust(y[dona], x[dona], c = 0, p = 1, kernel = "triangular",
                                         cluster = cl[dona]), "Dona: excluye |x| < 0.005")
  for (h in c(0.05, 0.10, 0.25)) {
    cct[[length(cct) + 1]] <- fila_rd(rdrobust::rdrobust(y[ok], x[ok], c = 0, p = 1, h = h,
                                      kernel = "triangular", cluster = cl[ok]), sprintf("Local lineal, h fijo = %.2f", h))
  }
  bal_cct <- bind_rows(lapply(names(predet), function(v)
    fila_rd(rdrobust::rdrobust(dist[[v]][ok], x[ok], c = 0, p = 1, cluster = cl[ok]),
            paste("Balance:", predet[[v]]))))
  guardar_tabla(bind_rows(bind_rows(cct), bal_cct), "rdrobust_resultados")

  png("output/figures/rdplot_voto_t1.png", width = 1800, height = 1200, res = 250)
  rdrobust::rdplot(y[ok & abs(x) <= 0.5], x[ok & abs(x) <= 0.5], c = 0, p = 4,
                   x.label = "Margen de victoria demócrata, t", y.label = "Voto Dem. t+1",
                   title = "rdplot (CCT): voto demócrata en t+1")
  dev.off()
}
if (requireNamespace("rdrobust", quietly = TRUE)) {
  tryCatch(bloque_rdrobust(), error = function(e) message("Bloque rdrobust con error: ", conditionMessage(e)))
} else {
  message("rdrobust no disponible: se omite el bloque E (A–D no dependen de él).")
}
if (requireNamespace("rddensity", quietly = TRUE)) tryCatch({
  rdd <- rddensity::rddensity(X = x[x != 0], c = 0)
  mccrary <- bind_rows(mccrary, data.frame(
    prueba = "Cattaneo–Jansson–Ma (rddensity)", theta_log_dif = NA, ee = NA,
    z = rdd$test$t_jk, p_valor = rdd$test$p_jk, ancho_bin = NA,
    ancho_banda = mean(c(rdd$h$left, rdd$h$right)), densidad_izq = rdd$hat$left, densidad_der = rdd$hat$right))
}, error = function(e) message("rddensity con error: ", conditionMessage(e)))
guardar_tabla(mccrary, "prueba_densidad")

saveRDS(list(ll = ll, balance = balance, placebo = placebo, orden = orden, mccrary = mccrary),
        "output/intermedios/validez.rds")
cat("Local lineal:", paste(sprintf("h=%.2f: %.3f (%.3f)", ll$h, ll$tau, ll$ee), collapse = "; "), "\n")
cat(sprintf("McCrary: θ = %.3f (EE %.3f), p = %.3f\n", mc$theta, mc$ee, mc$p))
