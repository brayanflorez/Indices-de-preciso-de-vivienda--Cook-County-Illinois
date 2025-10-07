🏙️ Taller 1 – Economía Urbana

Autores: David Flórez y Daniel Hernández
Curso: Economía Urbana – Universidad de los Andes


🎯 Objetivo

Este proyecto estima y compara índices de precios de vivienda aplicando tres metodologías econométricas clásicas:

Modelo Hedónico

Ventas Repetidas (Repeat Sales)

Efectos Fijos por Propiedad

El análisis busca evaluar cómo varían las estimaciones del índice de precios según el enfoque metodológico adoptado.



🧠 Estructura del repositorio

├── Code/ # Scripts en R con el desarrollo del taller
├── Data/ # Bases de datos utilizadas (.RDS)
├── Results/ # Gráficas y tablas generadas
└── README.md # Este archivo

⚙️ Requisitos

El código fue desarrollado en R (v4.3 o superior) y utiliza los siguientes paquetes:

p_load(tidyverse, fixest, broom, stargazer, skimr, modelsummary, kableExtra, psych, plm)



▶️ Reproducibilidad

Clona este repositorio:
git clone https://github.com/brayanflorez/taller1-ecoUrbana.git

Abre el archivo principal:
Code/Taller_1.R

Modifica la ruta de trabajo (workfile) según tu directorio local.

Ejecuta el script en RStudio.

Los resultados (tablas y figuras) se guardarán automáticamente en la carpeta Results/:

P1_hist_price.pdf: Histograma del precio de vivienda

P1_price_index.pdf: Comparación de índices

Tabla de desempeño de los modelos



📊 Resultados principales

El código compara el desempeño de los tres métodos utilizando métricas como R², RMSE, MAE, AIC y BIC, mostrando las diferencias en ajuste y precisión de cada modelo.



🤝 Contribuciones

Cada miembro del equipo realizó más de cinco commits sustanciales:

David Flórez: Modelos econométricos y visualización.

Daniel Hernández: Limpieza de datos y análisis comparativo.



📚 Referencias

Case, K., & Shiller, R. (1987). Prices of single-family homes since 1970: New indexes for four cities.

Rosen, S. (1974). Hedonic prices and implicit markets: Product differentiation in pure competition.
