# =============================================================================
# 08_exportar_excel.R — Reúne todas las tablas en output/resultados_Lee2008.xlsx
# Una hoja por tabla, en el orden del artículo; se regenera en cada ejecución.
# =============================================================================
source("scripts/00_utilidades.R")
library(openxlsx)

hojas <- list(
  list("comparacion_resumen",             "Resumen",            "Resumen: comparaciones que coinciden con lo publicado"),
  list("comparacion_publicado_replicado", "Comparación",        "Cuadro publicado / replicado / diferencia / explicación"),
  list("tabla1_formato_articulo",         "Tabla 1",            "Tabla 1 (formato del artículo): media (EE) [DE]"),
  list("tabla1_calculada",                "Tabla 1 detalle",    "Tabla 1 calculada: valores completos"),
  list("tabla2_formato_articulo",         "Tabla 2",            "Tabla 2 (formato del artículo): coeficiente (EE agrupado estado–distrito–década)"),
  list("tabla2_calculada",                "Tabla 2 detalle",    "Tabla 2 calculada: valores completos"),
  list("tabla2_variante_victoria_t1",     "Tabla 2 victoria t+1", "Texto, sección 3.4: Tabla 2 con victoria Dem. en t+1 como dependiente"),
  list("figura1_calculada",               "Figura 1",           "Figura 1: series calculadas"),
  list("figuras_2_5_resumen",             "Figuras 2-5",        "Figuras 2–5: saltos en el umbral y diferencias frente al ajuste de Lee"),
  list("figuras_2_5_puntos",              "Figuras 2-5 puntos", "Figuras 2–5: promedios por intervalo y ajustes (calculados y de Lee)"),
  list("local_lineal_anchos",             "Local lineal",       "EXTENSIÓN: RDD local lineal (triangular, EE agrupados) por ancho de banda"),
  list("rdrobust_resultados",             "rdrobust",           "EXTENSIÓN: rdrobust (CCT) — IC robusto con corrección de sesgo, variantes y balance"),
  list("balance_local_lineal",            "Balance",            "EXTENSIÓN: balance de covariables predeterminadas (local lineal)"),
  list("umbrales_placebo",                "Placebos",           "EXTENSIÓN: umbrales placebo (h = 0.10)"),
  list("orden_polinomio_global",          "Orden polinomio",    "EXTENSIÓN: orden del polinomio global (Lee usa 4)"),
  list("prueba_densidad",                 "Densidad",           "EXTENSIÓN: pruebas de densidad (McCrary; Cattaneo–Jansson–Ma)"),
  list("diccionario_datos",               "Diccionario",        "Diccionario de datos"))

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
st_titulo <- createStyle(fontSize = 12, textDecoration = "bold")
st_nota   <- createStyle(fontSize = 9, textDecoration = "italic", fontColour = "#666666")
st_cab    <- createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F4E3D",
                         wrapText = TRUE, valign = "center", border = "bottom")
st_num    <- createStyle(numFmt = "0.0000")
st_ajuste <- createStyle(wrapText = TRUE, valign = "top")

for (h in hojas) {
  archivo <- file.path("output/intermedios/tablas", paste0(h[[1]], ".rds"))
  if (!file.exists(archivo)) { message("Sin datos para la hoja ", h[[2]], " (se omite)"); next }
  df <- readRDS(archivo)
  addWorksheet(wb, h[[2]])
  writeData(wb, h[[2]], h[[3]], startRow = 1)
  addStyle(wb, h[[2]], st_titulo, rows = 1, cols = 1)
  writeData(wb, h[[2]], "Generado por scripts/00_ejecutar_todo.R", startRow = 2)
  addStyle(wb, h[[2]], st_nota, rows = 2, cols = 1)
  writeData(wb, h[[2]], df, startRow = 4, headerStyle = st_cab, withFilter = TRUE)
  num <- which(vapply(df, function(v) is.double(v) && any(v != round(v), na.rm = TRUE), logical(1)))
  if (length(num) && nrow(df))
    addStyle(wb, h[[2]], st_num, rows = 4 + seq_len(nrow(df)), cols = num, gridExpand = TRUE, stack = TRUE)
  anchos <- vapply(names(df), function(n) min(60, max(10, nchar(n), nchar(format(head(df[[n]], 200))) ) + 2), numeric(1))
  setColWidths(wb, h[[2]], cols = seq_along(df), widths = anchos)
  if ("explicacion" %in% names(df)) {
    k <- which(names(df) == "explicacion")
    setColWidths(wb, h[[2]], cols = k, widths = 80)
    addStyle(wb, h[[2]], st_ajuste, rows = 4 + seq_len(nrow(df)), cols = k, gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, h[[2]], firstActiveRow = 5)
}
saveWorkbook(wb, "output/resultados_Lee2008.xlsx", overwrite = TRUE)
cat("Excel generado: output/resultados_Lee2008.xlsx (", length(sheets(wb)), "hojas )\n")
