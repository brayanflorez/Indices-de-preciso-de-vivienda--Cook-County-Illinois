######################################################################
#  Taller 1: Economía Urbana
#  Punto 1: Indices de precios
# Nombre: David Florez, Daniel Hernandez
#####################################################################

rm(list = ls()) #limpiamos el entorno
require("pacman") #pacman facilita la carga e instalaci?n simult?nea de librer?as

p_load(
  tidyverse,  # Manipulaci?n y visualizaci?n de datos
  modeldata,  # Datos de ejemplo (incluye Ames Housing)
  stargazer,  # Tablas elegantes para resultados econom?tricos
  broom,      # Resultados de modelos en dataframes limpios
  fixest,     # Estimaciones con efectos fijos y robustas
  dplyr,      # Manipulaci?n de datos (incluido en tidyverse, pero lo dejamos expl?cito)
  extrafont,  # Para cambiar las fuentes de letra
  psych,       # Para estadísticas descriptivas
  skimr, modelsummary, kableExtra
)

######################
# RUTAS DE TRABAJO
######################

# Ruta principal
workfile <- "D:/OneDrive - Oficina de Normalización Previsional/Daniel/03. Msc Economía/01. Uniandes/05. Clases/02. Segundo Semestre/02. Economía Urbana/98. Talleres/01. Taller 01"

# Subrutas
pro <- file.path(workfile, "01. Programas")
bds <- file.path(workfile, "02. Bases")
res <- file.path(workfile, "03. Resultados")

################################################################################
# A. Carga de datos
################################################################################

# Cargamos los datos:
df <- readRDS(file.path(bds, "dataTaller01_PriceIndeces.RDS"))
#names(df)

# Variables clave
df <- df %>%
  rename(
    id = pin,
    year = year,
    price = sale_price
  ) %>%
  filter(!is.na(price), price > 0, !is.na(year)) %>%
  mutate(log_price = log(price)) %>%
  mutate(age = year - year_built)

# Año base
base_year <- min(df$year, na.rm = TRUE)
message("Año base: ", base_year)

#--------------------------------------------------------------------------#
# Estadísticas descriptivas de la base de datos ####
#--------------------------------------------------------------------------#

df <- df %>%
  # eliminar ventas sin precio o superficie, y precios <= 0
  filter(!is.na(price), price > 0) %>%
  
  # crear logaritmo del precio
  mutate(log_price = log(price),
         
         # antigüedad al momento de la venta
         age = year - year_built) %>%
  
  # eliminar edades negativas
  filter(age >= 0)


# Tabla  1 de estadisticas descriptivas

vars_desc <- c("price", "year", "age",
              "building_sqft", "land_sqft",
              "num_rooms", "num_bedrooms",
              "num_full_baths", 
              "renovation", "recent_renovation", 
              "num_fireplaces")

datasummary_skim(df[ , vars_desc], 
                 output = "latex_tabular", 
                 title = "Estadísticas descriptivas de la muestra",
                 histogram = FALSE)

# Histograma académico
ggplot(df, aes(x = price)) +
  geom_histogram(bins = 60, fill = "#0072B2", color = "black", alpha = 0.8) +
  scale_y_continuous(
    breaks = seq(0, 35000, 5000),
    labels = scales::comma
  ) +
  scale_x_continuous(
    breaks = seq(0, 1400000, 200000),
    labels = scales::comma
  ) +
  labs(
    x = "Precio (USD)",
    y = "Frecuencia"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )

ggsave(file.path(res,"P1_hist_price.pdf"), width = 7, height = 3, device = cairo_pdf)

################################################################################
# B. METODOLOGIAS PARA ESTIMACION DE INDICE DE PRECIOS
################################################################################

#--------------------------------------------------------------------------#
# 1a. Modelo Hedónico ####
#--------------------------------------------------------------------------#

# quitamos land_sqft porque tiene más de 152mil missings 

reg_hedonic <- feols(
  log_price ~ factor(year) + age + building_sqft +
    num_bedrooms + num_rooms + num_full_baths + num_half_baths +
    num_fireplaces + garage_size + construction_quality + type_of_residence
  + renovation | township_code,  
  data = df,
  vcov = ~id
)

summary(reg_hedonic)
etable(reg_hedonic, se.below = TRUE)

# Guardamos los coeficientes 
coefs_h <- broom::tidy(reg_hedonic, conf.int = TRUE) %>%
  filter(str_detect(term, "^factor\\(year\\)")) %>%
  mutate(year = as.integer(str_remove(term, "factor\\(year\\)"))) %>%
  transmute(year, method = "Hedonic",
            log_index = estimate, se = std.error,
            lwr = conf.low, upr = conf.high)

# Añadir base
coefs_h <- bind_rows(
  tibble(year = base_year, method = "Hedonic",
         log_index = 0, se = NA, lwr = NA, upr = NA),
  coefs_h
)

#--------------------------------------------------------------------------#
# 1b. Repeat Sales (Ventas Repetidas) ####
#--------------------------------------------------------------------------#

#----------------------------Primera Etapa ----------------------------#
# Objetivo: preparar datos para análisis de ventas repetidas (repeat sales)

# Calculamos el número de veces que cada propiedad fue vendida
house_sales <- df %>% 
  group_by(id) %>% 
  mutate(times_sold = row_number()) %>%  # cada venta se numera en orden
  ungroup()

# Filtramos solo propiedades vendidas más de una vez
house_sales <- house_sales %>% 
  group_by(id) %>% 
  mutate(sold_multiple = ifelse(n() > 1, 1, 0)) %>% 
  ungroup() %>% 
  filter(sold_multiple == 1)

# Preparamos los datos de precios (pares de ventas consecutivas)
prices_house_sales <- house_sales %>% 
  group_by(id) %>%
  mutate(price0 = lag(price)) %>%
  ungroup() %>%
  filter(!is.na(price0)) %>% # quedarse con pares de ventas consecutivas
  rename(price1 = price) %>%
  select(price1, price0, id)

# Preparamos los datos de años (pares consecutivos)
times_house_sales <- house_sales %>% 
  group_by(id) %>%
  mutate(time0 = lag(year)) %>%
  ungroup() %>%
  filter(!is.na(time0)) %>%        # mismos pares consecutivos
  rename(time1 = year) %>%
  select(time1, time0)

# Combinamos precios y tiempos (primeras diferencias)
rep_sales <- cbind(prices_house_sales, times_house_sales)

# Transformaciones adicionales
price1 <- log(rep_sales$price1)
price0 <- log(rep_sales$price0)
time1  <- rep_sales$time1
time0  <- rep_sales$time0

# Vista preliminar
head(rep_sales)

#-----------------Diferencial de precios en cada observación ------------------#
dv <- price1 - price0   # cambio en log(precio)
dv

# Creamos vector de años únicos ordenados
timevar <- sort(unique(c(time0, time1)))
timevar  

nt <- length(timevar)   # número de periodos
nt
n  <- length(dv)        # número de observaciones
n

# Construcción de la matriz X (dummies de tiempo)
# Regla: +1 si corresponde a la segunda venta, -1 si es la primera, 0 en otro caso
xmat <- matrix(0, nrow = n, ncol = nt - 1)
for (j in seq(2, nt)) {
  xmat[, j - 1] <- ifelse(time1 == timevar[j],  1, xmat[, j - 1])
  xmat[, j - 1] <- ifelse(time0 == timevar[j], -1, xmat[, j - 1])
}
xmat

# Etiquetas de columnas
colnames(xmat) <- paste0("Time", timevar[-1])  
xmat

#--------------------Estimamos el modelo de ventas repetidas ------------------#
# Nota: sin intercepto, los coeficientes son índices de tiempo relativos al año base
# dv es el diferencial de precios en cada observación
fit_rs1 <- lm(dv ~ xmat + 0)  # "+0" = sin intercepto
summary(fit_rs1)

#----------------------------Segunda Etapa ----------------------------#
# Objetivo: modelar la varianza condicional de los errores (heterocedasticidad)

# Guardamos residuos del modelo
e <- residuals(fit_rs1)

# Diferencia en tiempo entre ventas (años entre ventas)
xvar <- time1 - time0
xvar

# Ajuste de varianza condicional de los errores
fit_var <- lm(e^2 ~ xvar)

# Predicciones = estimación de la varianza
wgt <- fitted(fit_var)

# Construcción de pesos: inverso de la varianza (solo si > 0)
samp <- wgt > 0 
wgt <- ifelse(samp == TRUE, 1 / wgt, 0)

#--------------------------------Tercera Etapa---------------------------------#
# Objetivo: estimar el modelo de ventas repetidas ponderado
fit_rs3 <- lm(dv ~ xmat - 1, weights = wgt) # "-1" = sin intercepto
summary(fit_rs3)              
broom::tidy(fit_rs3)           # tabla ordenada
stargazer(fit_rs3, type="text")  # tabla tipo econométrica


#----------------------Guardamos los coeficientes--------------------------#

# Guardamos coeficientes para índice de precios
coefs_rs <- broom::tidy(fit_rs3, conf.int = TRUE) %>%
  mutate(year = as.integer(str_remove(term, ".*Time"))) %>%
  transmute(year, method = "Repeat Sales",
            log_index = estimate, se = std.error,
            lwr = conf.low, upr = conf.high)

# Añadimos año base con log_index = 0
coefs_rs <- bind_rows(
  tibble(year = min(df$year), method = "Repeat Sales",
         log_index = 0, se = NA, lwr = NA, upr = NA),
  coefs_rs
)

#--------------------------------------------------------------------------#
# 1c. Modelo con Efectos Fijos por Propiedad #####
#--------------------------------------------------------------------------#

# Evaluando estructura de datos de panel
library(plm)

# Convertir a objeto panel explícito
df_panel <- pdata.frame(df, index = c("id", "year"))

# Verificar si es balanceado
is.pbalanced(df_panel)  # Probablemente FALSE

# Estructura del panel
print(pdim(df_panel))

#----------------------------Primera Etapa ----------------------------#
# Objetivo: especificar un modelo de precios donde cada propiedad tiene
#           su propio efecto fijo (intercepto individual).
#           De esta forma controlamos por la calidad inobservable invariante
#           de cada propiedad.

# Fórmula: log(price) = δ_t + X_it β + α_i + ε_it
# δ_t: efectos de año (el índice de precios)
# X_it: controles observables (age, sqft, cuartos, etc.)
# α_i: efecto fijo de cada propiedad

# Definimos la fórmula con FE
f_fe <- log_price ~ i(year, ref = base_year) + age + renovation  | id + township_code

#----------------------------Segunda Etapa ----------------------------#
# Estimamos con feols y agrupamos errores a nivel de propiedad (id)
reg_fe <- feols(
  f_fe, 
  data = df,
  vcov = ~id  # errores agrupados en la propiedad
)

# No se considera clusterizar por township_code debido al poco número de cluster (19)
# Si se incluyese los intervalos de confianza (errores) de las estimaciones se inflarían sustancialmente 

# Resultados resumidos
etable(reg_fe, se.below = TRUE)

#----------------------------Tercera Etapa ----------------------------#
# Objetivo: extraer los coeficientes de los efectos de año (δ_t)
#           para construir el índice de precios

coefs_fe <- broom::tidy(reg_fe, conf.int = TRUE) %>%
  filter(str_detect(term, "^year::")) %>%           # solo efectos de año
  mutate(year = as.integer(str_remove(term, "year::"))) %>%
  transmute(year, method = "FE Property",
            log_index = estimate, se = std.error,
            lwr = conf.low, upr = conf.high)

# Añadimos el año base con log_index = 0 (índice = 1)
coefs_fe <- bind_rows(
  tibble(year = base_year, method = "FE Property",
         log_index = 0, se = NA, lwr = NA, upr = NA),
  coefs_fe
)

#--------------------------------------------------------------------------#
# 2. Gráficamos los 3 modelos ####
#--------------------------------------------------------------------------#

# Unimos los tres resultados
index_all <- bind_rows(coefs_h, coefs_rs, coefs_fe) %>%
  arrange(method, year) %>%
  mutate(
    index = ifelse(year == 2000, 100, exp(log_index) * 100),
    lwr = ifelse(year == 2000, 100, exp(lwr) * 100),
    upr = ifelse(year == 2000, 100, exp(upr) * 100),
    log_index = ifelse(year == 2000, 0, log_index),
    se = ifelse(year == 2000, NA, se)
  ) %>%
  mutate(method = recode(method,
                         "Repeat Sales" = "Ventas repetidas",
                         "Hedonic" = "Hedónico",
                         "FE Property" = "Efectos fijos"
  ))

#  Gráfica comparativa

 #font_import(pattern = "times", prompt = FALSE)
 loadfonts(device = "win")

 ggplot(index_all, aes(x = year, y = index, color = method)) +
   # Bandas de confianza
   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = method),
               alpha = 0.3, colour = NA, show.legend = FALSE) +
   # Línea + puntos
   geom_line(linewidth = 0.8) +
   geom_point(size = 1.2, shape = 16) +
   
   # Línea base punteada en el año base (índice = 1)
   geom_hline(yintercept = 100 , linetype = "dashed", color = "black", size = 0.7, alpha = 0.7) +
   
   # Escalas de ejes
   scale_x_continuous(
     breaks = seq(min(index_all$year), max(index_all$year), by = 2),
     expand = expansion(mult = 0.02)
   ) +
   scale_y_continuous(
     limits = c(80, 220),
     breaks = seq(80, 220, by = 20),
     expand = expansion(mult = 0.02)
   ) +
   
   # Etiquetas
   labs(
     x = "Año",
     y = "Índice de precios",
     caption = "Nota: las áreas sombreadas representan intervalos de confianza al 95%."
   ) +
   
   # Colores sobrios (tipo segunda gráfica)
   scale_color_manual(values = c(
     "Hedónico" = "#1F77B4",       # azul sobrio
     "Ventas repetidas" = "#FF7F0E", # naranja
     "Efectos fijos" = "#2CA02C"    # verde
   )) +
   scale_fill_manual(values = c(
     "Hedónico" = "#1F77B4",
     "Ventas repetidas" = "#FF7F0E",
     "Efectos fijos" = "#2CA02C"
   )) +
   
   # Tema sobrio con grillas horizontales
   theme_minimal(base_size = 14, base_family = "Times New Roman") +
   theme(
     plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
     plot.subtitle = element_text(size = 12, hjust = 0.5),
     axis.title = element_text(face = "bold"),
     legend.position = "bottom",
     legend.title = element_blank(),
     panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.minor.y = element_blank(),
     plot.caption = element_text(size = 9, hjust = 0)  # nota pequeña alineada a la izquierda
   )
 
ggsave(file.path(res,"P1_price_index.pdf"), width = 8, height = 5, device = cairo_pdf)

################################################################################
# C. Evaluación de desempeño de los 3 metodos
################################################################################

## Metricas de desempeño según distintos modelos

# 1. Métricas para el modelo HEDÓNICO
r2_hedonic <- r2(reg_hedonic)
metrics_hedonic <- data.frame(
  method = "Hedónico",
  n_observations = nobs(reg_hedonic),
  r_squared = as.numeric(r2_hedonic[1]),  # Primer elemento es R²
  adj_r_squared = as.numeric(r2_hedonic[2]),  # Segundo elemento es R² ajustado
  rmse = sqrt(mean(residuals(reg_hedonic)^2, na.rm = TRUE)),
  mae = mean(abs(residuals(reg_hedonic)), na.rm = TRUE),
  aic = AIC(reg_hedonic),
  bic = BIC(reg_hedonic),
  stringsAsFactors = FALSE
)

# 2. Métricas para el modelo de VENTAS REPETIDAS
metrics_rs <- data.frame(
  method = "Ventas repetidas",
  n_observations = nobs(fit_rs3),
  r_squared = summary(fit_rs3)$r.squared,
  adj_r_squared = summary(fit_rs3)$adj.r.squared,
  rmse = sqrt(mean(residuals(fit_rs3)^2, na.rm = TRUE)),
  mae = mean(abs(residuals(fit_rs3)), na.rm = TRUE),
  aic = AIC(fit_rs3),
  bic = BIC(fit_rs3),
  stringsAsFactors = FALSE
)

# 3. Métricas para el modelo de EFECTOS FIJOS
r2_fe <- r2(reg_fe)
metrics_fe <- data.frame(
  method = "Efectos fijos",
  n_observations = nobs(reg_fe),
  r_squared = as.numeric(r2_fe[1]),  # Primer elemento es R²
  adj_r_squared = as.numeric(r2_fe[2]),  # Segundo elemento es R² ajustado
  rmse = sqrt(mean(residuals(reg_fe)^2, na.rm = TRUE)),
  mae = mean(abs(residuals(reg_fe)), na.rm = TRUE),
  aic = AIC(reg_fe),
  bic = BIC(reg_fe),
  stringsAsFactors = FALSE
)

# Combinar métricas básicas
model_performance <- bind_rows(metrics_hedonic, metrics_rs, metrics_fe)

# Preparar los datos con formato de números
model_performance_formatted <- model_performance %>%
  mutate(
    n_observations = format(n_observations, big.mark = ",", scientific = FALSE),
    r_squared = round(r_squared, 4),
    adj_r_squared = round(adj_r_squared, 4),
    rmse = round(rmse, 4),
    mae = round(mae, 4),
    aic = format(round(aic, 1), big.mark = ",", scientific = FALSE),
    bic = format(round(bic, 1), big.mark = ",", scientific = FALSE)
  ) %>%
  rename(
    "Método" = method,
    "N" = n_observations,
    "R²" = r_squared,
    "R² Ajustado" = adj_r_squared,
    "RMSE" = rmse,
    "MAE" = mae,
    "AIC" = aic,
    "BIC" = bic
  )

kable_latex <- model_performance_formatted %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Métricas de desempeño de modelos de Índice de Precios de Vivienda",
    label = "tab:model_performance",
    align = c("l", "c", "c", "c", "c", "c", "c", "c")  # Todas centradas excepto método
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position", "scale_down"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%           # Encabezados en negrita
  column_spec(1, bold = TRUE) %>%        # Columna Método en negrita
  column_spec(2:8, bold = FALSE) %>%     # Demás columnas sin negrita
  add_header_above(
    c(" " = 1, "Estadísticas de Ajuste" = 5, "Criterios de Información" = 2),
    bold = TRUE,
    align = "c"
  ) %>%
  footnote(
    general = "Nota: RMSE = Raíz del Error Cuadrático Medio, MAE = Error Absoluto Medio, AIC = Criterio de Información de Akaike, BIC = Criterio de Información Bayesiano.",
    threeparttable = TRUE,
    escape = FALSE
  )

cat(kable_latex)
