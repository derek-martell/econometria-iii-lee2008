# =============================================================================
# 00_ejecutar_todo.R — Ejecuta la réplica completa de Lee (2008) desde cero
# Uso: abrir lee2008_replication.Rproj (o fijar el directorio en la raíz del
# proyecto) y ejecutar  source("scripts/00_ejecutar_todo.R")
# =============================================================================
inicio <- Sys.time()
# Ubica la raíz del proyecto aunque el directorio de trabajo sea scripts/ u otro
# (por ejemplo, si se ejecuta línea por línea con Ctrl+Enter desde RStudio).
if (!file.exists("scripts/00_utilidades.R")) {
  candidatos <- ".."
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ruta <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(ruta)) candidatos <- c(dirname(dirname(ruta)), candidatos)
  }
  raiz <- Filter(function(d) file.exists(file.path(d, "scripts/00_utilidades.R")), candidatos)
  if (length(raiz) == 0) stop("No encuentro la raíz del proyecto. Abra lee2008_replication.Rproj ",
                              "o use setwd() hacia la carpeta 'Q regression'.")
  setwd(raiz[1])
  message("Directorio de trabajo fijado en: ", getwd())
}

pasos <- c("01_datos.R",                 # muestras y diccionario
           "02_figura1.R",               # Figura 1
           "03_tabla1.R",                # Tabla 1
           "04_tabla2.R",                # Tabla 2 (+ variante victoria t+1)
           "05_figuras_2_a_5.R",         # Figuras 2–5
           "06_validez_y_extensiones.R", # local lineal, balance, placebos, McCrary, rdrobust
           "07_cuadro_comparacion.R",    # cuadro publicado / replicado / diferencia
           "08_exportar_excel.R")        # todas las tablas en output/resultados_Lee2008.xlsx

# Ejecución limpia: se borran resultados de corridas anteriores (incluidos CSV antiguos).
unlink(c("output/tables", "output/intermedios", "output/resumen_tablas_Lee2008.xlsx"), recursive = TRUE)
for (p in pasos) {
  message("\n==== Ejecutando ", p, " ====")
  source(file.path("scripts", p), local = new.env())
}

writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
message(sprintf("\nRéplica completa en %.1f s. Tablas en output/resultados_Lee2008.xlsx; figuras en output/figures.",
                as.numeric(difftime(Sys.time(), inicio, units = "secs"))))
