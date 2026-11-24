# =============================================================================
# 04_tabla2.R — Tabla 2: efecto de ganar en t sobre el éxito electoral en t+1
# Réplica de la especificación de Lee (public_use.do, programa `fitpoly`):
#   regress y x x^2 x^3 x^4 D·x D·x^2 D·x^3 D·x^4 D [controles] if use==1, cluster(statedisdec)
# Se agrega la variante con "victoria demócrata en t+1" descrita en el texto (sección 3.4).
# =============================================================================
source("scripts/00_utilidades.R")
dist     <- readRDS("output/intermedios/muestra_distrito.rds")
dist_win <- readRDS("output/intermedios/muestra_distrito_demwinnext.rds")
t2_use   <- read_dta(file.path(RUTA_DATOS, "table_two_final.dta")) |> filter(use == 1)

exp4 <- c("demofficeexp", "othofficeexp", "demelectexp", "othelectexp")
todas <- c("demshareprev", "demwinprev", exp4)

# Columna 6: residuos de MCO del voto en t+1 sobre todas las covariables, calculados
# en la muestra use==1 (6559 obs., como en el .do), antes de estimar el salto.
aux <- lm(as.formula(paste("demsharenext ~", paste(todas, collapse = " + "))), data = t2_use)
t2_use$res <- NA_real_
t2_use$res[as.integer(names(residuals(aux)))] <- residuals(aux)
# `dist` es t2_use sin la observación con right = NA, en el mismo orden.
dist$demsharenextres <- t2_use$res[!is.na(t2_use$right)]
stopifnot(!anyNA(dist$demsharenextres), identical(as.numeric(dist$difdemshare), as.numeric(t2_use$difdemshare[!is.na(t2_use$right)])))

especificaciones <- list(
  c1 = list(y = "demsharenext",    ctrl = NULL),
  c2 = list(y = "demsharenext",    ctrl = c("demshareprev", "demwinprev")),
  c3 = list(y = "demsharenext",    ctrl = c("demofficeexp", "othofficeexp")),
  c4 = list(y = "demsharenext",    ctrl = c("demelectexp", "othelectexp")),
  c5 = list(y = "demsharenext",    ctrl = todas),
  c6 = list(y = "demsharenextres", ctrl = NULL),
  c7 = list(y = "difdemsharenext", ctrl = c("demwinprev", exp4)),
  c8 = list(y = "demshareprev",    ctrl = c("demwinprev", exp4)))

estimar <- function(esp, datos) {
  m  <- lm(formula_cuartica(esp$y, esp$ctrl), data = datos)
  ee <- ee_cluster(m, datos$statedisdec[as.integer(rownames(model.frame(m)))])
  nombres <- c("D", todas)
  data.frame(termino = nombres,
             coef = unname(coef(m)[nombres]), ee = unname(ee[nombres]), n = nobs(m))
}
res <- bind_rows(lapply(names(especificaciones), function(k)
  cbind(columna = k, estimar(especificaciones[[k]], dist)))) |>
  filter(!is.na(coef))
guardar_tabla(res, "tabla2_calculada")

# ---- Formato del artículo ---------------------------------------------------
etq <- c(D = "Victoria, elección t", demshareprev = "Voto Dem., t-1", demwinprev = "Victoria Dem., t-1",
         demofficeexp = "Exp. política Dem.", othofficeexp = "Exp. política oponente",
         demelectexp = "Exp. electoral Dem.", othelectexp = "Exp. electoral oponente")
formato <- do.call(rbind, lapply(names(etq), function(t) {
  fila_c <- fila_e <- rep("", 8)
  for (j in 1:8) {
    r <- res[res$columna == paste0("c", j) & res$termino == t, ]
    if (nrow(r)) { fila_c[j] <- fmt(r$coef); fila_e[j] <- paste0("(", fmt(r$ee), ")") }
  }
  rbind(c(etq[[t]], fila_c), c("", fila_e))
}))
formato <- rbind(formato, c("Observaciones", res$n[res$termino == "D"]))
colnames(formato) <- c("Variable", paste0("(", 1:8, ")"))
guardar_tabla(as.data.frame(formato), "tabla2_formato_articulo")

# ---- Valores publicados (Lee 2008, Tabla 2, p. 690; 3 decimales) -------------
pub <- read.csv(text = "columna,termino,coef,ee
c1,D,0.077,0.011
c2,D,0.078,0.011
c2,demshareprev,0.293,0.017
c2,demwinprev,-0.017,0.007
c3,D,0.077,0.011
c3,demofficeexp,-0.001,0.001
c3,othofficeexp,0.001,0.001
c4,D,0.077,0.011
c4,demelectexp,-0.001,0.001
c4,othelectexp,0.001,0.001
c5,D,0.078,0.011
c5,demshareprev,0.298,0.017
c5,demwinprev,-0.006,0.007
c5,demofficeexp,0.000,0.003
c5,othofficeexp,0.000,0.004
c5,demelectexp,-0.003,0.003
c5,othelectexp,0.003,0.004
c6,D,0.081,0.014
c7,D,0.079,0.013
c7,demwinprev,-0.175,0.009
c7,demofficeexp,-0.002,0.003
c7,othofficeexp,-0.008,0.004
c7,demelectexp,-0.003,0.003
c7,othelectexp,0.011,0.004
c8,D,-0.002,0.011
c8,demwinprev,0.240,0.009
c8,demofficeexp,0.002,0.002
c8,othofficeexp,0.011,0.003
c8,demelectexp,0.000,0.002
c8,othelectexp,-0.011,0.003")
comp <- pub |> rename(coef_pub = coef, ee_pub = ee) |>
  left_join(res, by = c("columna", "termino")) |>
  mutate(dif_coef = round(coef, 3) - coef_pub, dif_ee = round(ee, 3) - ee_pub)
saveRDS(comp, "output/intermedios/comparacion_tabla2.rds")
cat(sprintf("Tabla 2: %d de %d coeficientes y %d de %d EE coinciden al redondear a 3 decimales\n",
            sum(abs(comp$dif_coef) < 1e-9), nrow(comp), sum(abs(comp$dif_ee) < 1e-9), nrow(comp)))

# ---- Variante del texto: victoria demócrata en t+1 como variable dependiente ---
# Lee (p. 691): "all specifications in Table 2 were repeated for the indicator variable
# for a Democrat victory in t+1 ... stable at about 0.38 ... column (8) check: −0.005 (0.033)".
dw <- dist_win |> filter(!is.na(demwinnext))
aux_w <- lm(as.formula(paste("demwinnext ~", paste(todas, collapse = " + "))), data = dw)
dw$demwinnextres <- residuals(aux_w)
dw$difdemwinnext <- dw$demwinnext - dw$demwinprev
esp_w <- list(
  c1 = list(y = "demwinnext", ctrl = NULL),
  c2 = list(y = "demwinnext", ctrl = c("demshareprev", "demwinprev")),
  c3 = list(y = "demwinnext", ctrl = c("demofficeexp", "othofficeexp")),
  c4 = list(y = "demwinnext", ctrl = c("demelectexp", "othelectexp")),
  c5 = list(y = "demwinnext", ctrl = todas),
  c6 = list(y = "demwinnextres", ctrl = NULL),
  c7 = list(y = "difdemwinnext", ctrl = c("demshareprev", exp4)),
  c8 = list(y = "demwinprev", ctrl = c("demshareprev", exp4)))
res_w <- bind_rows(lapply(names(esp_w), function(k)
  cbind(columna = k, estimar(esp_w[[k]], dw)))) |> filter(termino == "D")
guardar_tabla(res_w, "tabla2_variante_victoria_t1")
saveRDS(res_w, "output/intermedios/tabla2_variante_victoria_t1.rds")
cat("Variante victoria t+1 (coef. de D):", paste(fmt(res_w$coef), collapse = ", "), "\n")
