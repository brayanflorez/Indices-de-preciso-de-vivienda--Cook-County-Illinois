######################################################################
#  Taller 1: Economía Urbana
# Nombre: David Florez, Daniel  y Juan Sebastián
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
  extrafont
)

setwd("C:/Users/braya/OneDrive - Universidad de los andes/Escritorio/U/8vo semestre/Economía Urbana/Taller 1")

# Cargamos los datos:
df <- readRDS("Data/dataTaller01_PriceIndeces.Rds")
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
# 1a. Modelo Hedónico ####
#--------------------------------------------------------------------------#

reg_hedonic <- feols(
  log_price ~ factor(year) + age + building_sqft + land_sqft +
    num_bedrooms + num_rooms + num_full_baths + num_half_baths +
    num_fireplaces + garage_size + construction_quality
  | township_code,  
  data = df
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
  select(price1, price0)

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
f_fe <- log_price ~ i(year, ref = base_year) + age + building_sqft + land_sqft +
  num_bedrooms + num_rooms + num_full_baths + num_half_baths +
  num_fireplaces + garage_size + construction_quality | id

#----------------------------Segunda Etapa ----------------------------#
# Estimamos con feols y agrupamos errores a nivel de propiedad (id)
reg_fe <- feols(
  f_fe,
  data = df,
  vcov = ~id   # errores agrupados en la propiedad
)

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
    index = exp(log_index),
    lwr = exp(lwr),
    upr = exp(upr)
  )

#  Gráfica comparativa

 font_import(pattern = "times", prompt = FALSE)
 loadfonts(device = "win")

ggplot(index_all, aes(x = year, y = index,
                      color = method, linetype = method)) +
  # Bandas de confianza
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = method),
              alpha = 0.2, colour = NA, show.legend = FALSE) +
  # Línea + puntos
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  
  # Etiquetas
  labs(
    title = "Índice anual de precios de vivienda - Cook County, Illinois",
    subtitle = paste("Base:", base_year),
    x = "Año",
    y = "Índice de precios"
  ) +
  
  # Colores sobrios (tipo académico)
  scale_color_manual(values = c(
    "Hedonic" = "#1b9e77",       # verde
    "Repeat Sales" = "#7570b3",  # azul
    "FE Property" = "#d95f02"    # naranja
  )) +
  scale_fill_manual(values = c(
    "Hedonic" = "#1b9e77",
    "Repeat Sales" = "#7570b3",
    "FE Property" = "#d95f02"
  )) +
  
  # Tema sobrio sin grillas
  theme_classic(base_size = 14, base_family ="Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7), # marco negro
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank()
  )

ggsave("Results/P1_price_index.pdf", width = 8, height = 5, device = cairo_pdf)



