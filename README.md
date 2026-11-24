# Réplica: Lee (2008) — Regresión discontinua en elecciones de la Cámara de EE. UU.

> Lee, D. S. (2008). Randomized experiments from non-random selection in U.S. House elections. *Journal of Econometrics*, 142(2), 675–697.

Trabajo final de **Econometría III** (UNMSM, 2026-II), opción **C3** del catálogo.

## Cómo ejecutar

1. Abrir `lee2008_replication.Rproj` en RStudio (o fijar el directorio de trabajo en esta carpeta).
2. Ejecutar `source("scripts/00_ejecutar_todo.R")`.

El script instala los paquetes que falten y crea las carpetas de salida. Corre de principio a fin sin intervención manual (≈10 s) y guarda `output/sessionInfo.txt`. Todas las tablas quedan en un solo Excel, `output/resultados_Lee2008.xlsx`, y las figuras en `output/figures/`. Cada corrida borra los resultados anteriores y los vuelve a generar.

**Paquetes:**
- Obligatorios: haven, dplyr, ggplot2, readr, gridExtra, sandwich, openxlsx.
- Opcionales: rdrobust y rddensity, para el bloque CCT del script 06. Si no están, el script intenta instalarlos desde CRAN y, si no lo logra, omite solo ese bloque.

## Datos

Archivos públicos del autor en `data/Lee2008/`:
- **Origen:** archivo de datos de *Mostly Harmless Econometrics* (Angrist y Pischke), carpeta *Lee 2008*.
- **Contenido:** cinco `.dta` y el programa `public_use.do`.
- **Fuente primaria:** ICPSR 7757, con correcciones del CQ y del Clerk para 1992–1998.

| Archivo | Unidad | Uso |
|---|---|---|
| `table_two_final.dta` | distrito–elección (Demócratas) | Tablas 1–2, Figuras 4–5 |
| `group_final.dta` | distrito–elección (partido ganador) | victoria Dem. en t+1, Figuras 1 y 5a |
| `individ_final.dta` | candidato–elección | Figuras 1–3 |
| `figure1.dta`, `table_one_final.dta` | valores publicados | **solo** como referencia para comparar |

**Muestras:**
- **Distrito:** N = 6558 (3818 ganadores y 2740 perdedores). Elecciones t de 1946–1996; se excluyen los años t terminados en 0 o 2 por el redistritaje, y la única observación con margen exactamente 0.
- **Candidato:** N = 9674 Demócratas.

Las variables, unidades, faltantes y rangos están en la hoja «Diccionario» de `output/resultados_Lee2008.xlsx`.

## Estructura

| Script | Qué hace | Tipo |
|---|---|---|
| `00_utilidades.R` | Funciones comunes: polinomio cuártico por lado, EE agrupados tipo Stata, ajustes de figuras, local lineal, McCrary | — |
| `01_datos.R` | Muestras y diccionario | — |
| `02_figura1.R` | Figura 1 calculada desde los microdatos | Réplica |
| `03_tabla1.R` | Tabla 1: medias, EE, DE y ajuste paramétrico en el umbral | Réplica |
| `04_tabla2.R` | Tabla 2: 8 especificaciones de `public_use.do`, más la variante con victoria en t+1 de la sección 3.4 | Réplica |
| `05_figuras_2_a_5.R` | Figuras 2a–5b: promedios en intervalos de 0.005 y ajustes polinomial/logit | Réplica (2b, 3a y 3b son adaptación, ver abajo) |
| `06_validez_y_extensiones.R` | Local lineal por ancho de banda, balance, cortes placebo, orden del polinomio, McCrary y rdrobust/rddensity | **Adaptación didáctica** |
| `07_cuadro_comparacion.R` | Cuadro «publicado / replicado / diferencia / explicación» | — |
| `08_exportar_excel.R` | Reúne todas las tablas en `output/resultados_Lee2008.xlsx` (una hoja por tabla) | — |

## Método de Lee reproducido

- **Tabla 2 y columnas 7–8 de la Tabla 1:**
  - MCO con polinomio de 4.º orden en el margen, con todos los términos interactuados con la victoria en t.
  - EE agrupados por estado–distrito–década, equivalentes a `regress, cluster()` de Stata.
- **Variables binarias:** en las Figuras 2a, 5a y 5b, y en el ajuste de la Tabla 1, se usa un logit con índice cuártico a cada lado. Se aplica la convención de `logit` de Stata: todo valor distinto de 0 cuenta como éxito.
- **Figuras:**
  - El ajuste se estima con los microdatos a cada lado del umbral.
  - El valor de cada intervalo es el promedio de las predicciones individuales.
  - Ninguna cifra publicada se usa como insumo: las variables `mp*`, `figure1.dta` y `table_one_final.dta` solo se usan para comparar.

## Resultado de la réplica (hoja «Comparación» de `output/resultados_Lee2008.xlsx`)

- **Coinciden:**
  - Figura 1 (las tres series).
  - Tabla 2: 30 de 30 coeficientes y EE a 3 decimales.
  - Tabla 1: 56 de 64 celdas; todas las de las 7 variables con microdato completo.
  - Figuras 2a, 4a, 4b y 5b: promedios locales y ajustes, con diferencia < 1e-4.
- **Diferencias explicadas:**
  - **Victoria Dem. en t+1** (fila 2 de la Tabla 1, Figura 5a y texto de la secc. 3.4). Lee construyó esta variable con un programa privado (`readme.txt`) y no la publicó en `table_two_final.dta`. Aunque `partywinnext` existe en `group_final.dta`, ambos archivos no comparten un ID único (743 empates ambiguos en elecciones sin oposición con margen ±1) y `partywinnext` tiene 290 `NA`. Por ello se adoptó la regla `demsharenext > 0.5` sobre la variable ya depurada por Lee: acierta en 6166 de 6268 casos observados (98.4 %), difiere solo en 102 elecciones con terceros partidos y reproduce con gran precisión la Tabla 1 (máx. dif. `0.017`), la variante de la Tabla 2 (`0.398` vs. `≈0.38`, placebo `-0.005` exacto) y la Figura 5a (salto `0.361` vs. `0.360` de Lee).
  - **Figuras 2b, 3a y 3b — adaptación.** Los microdatos del candidato (victorias pasadas, postular en t+1, postulaciones pasadas) no son públicos; los archivos solo traen el promedio por intervalo. Los promedios locales se toman de esos archivos y el ajuste se re-estima sobre ellos. Los saltos son similares: 3a da 0.426, frente a ≈0.40 en el texto y 0.427 en el ajuste de Lee.
- **Extensión (rúbrica C3):**
  - El salto local lineal varía entre 0.058 y 0.095 según el ancho de banda (0.077 con h = 0.25).
  - Es una adaptación didáctica: el estimador no es el del artículo.
  - Con h = 0.05, la victoria Dem. en t−1 muestra desbalance (p = 0.007). Es un punto a discutir en el informe.

## Uso de asistentes

El código fue desarrollado con apoyo de un asistente de IA (Gemini / Antigravity, Google), tal como exige la rúbrica (sección 6). El grupo revisó y ejecutó cada script y debe poder explicarlo en la defensa.
