# Replicación: Lee (2008) - Regresión Discontinua (RDD)

Este repositorio contiene la replicación computacional y empírica del artículo clásico:
> **Lee, David S. (2008).** *Randomized experiments from non-random selection in U.S. House elections.* **Journal of Econometrics**, 142(2), 675–697.

Proyecto grupal para el curso de **Econometría III** (8vo Ciclo).

---

## 📁 Estructura del Proyecto

```text
├── data/
│   └── Lee2008/          # Archivos de datos oficiales del archivo de Mostly Harmless Econometrics (MIT)
│       ├── table_two_final.dta  # Base a nivel congresal (N = 6,558, clusters state-district-decade)
│       ├── group_final.dta      # Base para análisis por grupos (Figuras 4 y 5)
│       ├── individ_final.dta    # Base a nivel de candidato (Figuras 2 y 3)
│       ├── table_one_final.dta  # Benchmarks oficiales de la Tabla 1
│       └── figure1.dta          # Datos de la Figura 1
├── scripts/              # Scripts modulares de análisis en R
│   ├── 01_setup_y_datos.R       # Carga, verificación y preparación de datos
│   ├── 02_tabla_1.R             # Réplica de Tabla 1 (Balance y continuidad)
│   ├── 03_tabla_2.R             # Réplica de Tabla 2 (8 especificaciones econométricas con SE clustered)
│   └── 04_figuras_2_a_5.R       # Réplica de Figuras 2 a 5 (promedios locales y ajustes paramétricos)
├── output/
│   ├── tables/           # Tablas exportadas
│   └── figures/          # Gráficos generados
├── lee2008_replication.Rproj # Proyecto de RStudio
└── RDrand.pdf            # Artículo original de David S. Lee (2008)
```

---

## 🎯 Objetivos de Replicación
1. **Tabla 1:** Medias por submuestras de margen de victoria ($|\text{Margen}| < 0.5$, $|\text{Margen}| < 0.05$) y ajuste polinómico de cuarto grado evaluado en el umbral.
2. **Tabla 2:** Las 8 especificaciones econométricas con polinomio de orden 4 interactuado, controles predeterminados, residuos, primera diferencia y prueba de falsación en $t-1$, con errores estándar agrupados a nivel estado-distrito-década (`statedisdec`).
3. **Figuras 2 a 5:** Promedios locales en bins de 0.005 en el rango $[-0.25, 0.25]$ y ajustes paramétricos (polinómico y logit).
