####### Modelos
library(test2021)
library(tidyverse)
### Ejemplo con detalle para londres
# Cambiar segun el path correspondiente
PATH <- getwd()
# Conjunto de datos principales
main_db <- lon_hurto_bicicletas
main_db <- subset(main_db, select = c(FECHA, CODIGO_DIV1, CODIGO_DIV2, SEXO,
                                      RANG_DIA))
## Proporcionalidad por sexo - Tomado de robo de bicis Bogota
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(1579, 6332, 112))
)
## Proporcionalidad por rango del dia - Tomado de robo de bicis Bogota
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(1868, 2387, 1915, 1791))
)
# Conjunto de datos extendido
lon_part1 <- read.csv(paste0(PATH,"/extdata/londres_part1.csv"))
lon_part2 <- read.csv(paste0(PATH,"/extdata/londres_part2.csv"))
lon_part3 <- read.csv(paste0(PATH,"/extdata/londres_part3.csv"))
lon_part4 <- read.csv(paste0(PATH,"/extdata/londres_part4.csv"))
lon_full <- dplyr::bind_rows(lon_part1, lon_part2, lon_part3, lon_part4)
rm(lon_part1,  lon_part2, lon_part3, lon_part4)
lon_filter <- lon_full %>%
  dplyr::filter(Crime.type == "Bicycle theft") %>%
  dplyr::select(Month, Longitude, Latitude)
rm(lon_full)
names(lon_filter) <- c("FECHA", "longitude", "latitude")
lon_filter <- lon_filter[complete.cases(lon_filter),]
# Procesamiento con las funciones del paquete
lon_filter2 <- add_spatial_f(lon_filter, londres, "CODIGO_DIV1", "CODIGO_DIV2")
lon_filter2 <- simular_variables_adicionales(lon_filter2, df_prop_sexo = df_prop_sexo,
                                             df_prop_rang_dia = df_prop_rang_dia)
# Sintetizar todo en una sola base de datos
df_work <- dplyr::bind_rows(lon_filter2, main_db)
##################
###### 1. EDA
##################
#' 1. Tenemos el mapa de datos en la siguiente App
#' run_shiny("crime_colombia_city_2")
#' Nota: Es necesario seleccionar Londres, luego elegir el delito "Hurto de
#' bicicletas"
#' 2. Algunos de los graficos requeridos se mostraran a continuacion
#####
### 1.1. Evaluacion de serie agregada general
#####
# Agregar por Fecha
df_work$FECHA <- as.Date(paste0(df_work$FECHA,"-01"))
main_DT <- setDT(df_work)
main_DT[,.N,FECHA] %>%
  ggplot(mapping = aes(x = FECHA, y = N)) +
  geom_line(color = '#00AFBB', size = 1) +
  theme_minimal(base_size = 12) +
  stat_smooth(color = "#e9ecef", fill = "#e9ecef", method = "loess") +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = 'black', lineend = 'round',
                                   size = 1),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
# Notas: Observamos un patron estacional pronunciado
main_DT[,.N,FECHA] %>%
  select(N) %>%
  stats::ts(start = c(year(min(df_work$FECHA)),month(min(df_work$FECHA))),
     end = c(year(max(df_work$FECHA)),month(max(df_work$FECHA))),
     frequency = 12) %>%
  stats::decompose() %>%
  forecast::autoplot()
#' La descomposicion nos muestra que tanto el trend como componente estacional
#' son de cuidar.
main_DT[,.N,FECHA] %>%
  select(N) %>%
  stats::ts(start = c(year(min(df_work$FECHA)),month(min(df_work$FECHA))),
     end = c(year(max(df_work$FECHA)),month(max(df_work$FECHA))),
     frequency = 12) %>%
  as.vector() %>%
  stats::ts(start = c(year(min(df_work$FECHA)),month(min(df_work$FECHA))),
     end = c(year(max(df_work$FECHA)),month(max(df_work$FECHA))),
     frequency = 12) %>%
  stats::stl(t.window = 13, s.window = "periodic",robust = TRUE) %>%
  forecast::autoplot()
#' La descomposicion por LOESS confirma los hallazgos. Aunque ahora se detecta
#' una serie de outliers en el ultimo anno de observacion
# ACF
ciline <- qnorm((1 - 0.95)/2)/sqrt(36)
main_DT[,.N,FECHA] %>%
  select(N) %>%
  acf(plot = FALSE) %>%
  with(data.frame(lag, acf)) %>%
  ggplot(mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
  ylab("") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
#
main_DT[,.N,FECHA] %>%
  select(N) %>%
  pacf(plot = FALSE) %>%
  with(data.frame(lag, acf)) %>%
  ggplot(mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
  ylab("") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
# Una leve evidencia de un patron ciclico
######
### 1.2 Evaluacion de series por division administrativa uno
######
# Mostrar algunas series por CODIGO_DIV1
main_DT[,.N,.(FECHA, CODIGO_DIV1)] %>%
  ggplot(mapping = aes(x = FECHA, y = N, color = CODIGO_DIV1)) +
  geom_line(size = 1) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = 'black', lineend = 'round',
                                   size = 1),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
# Los conteos dependen de sectores, no obstante podrian crearse algunos grupos
# Usar el algoritmo de Calinski y Harabatz para clasificar datos longitudinales
library(kml)
cluster_db <- main_DT[,.N,.(FECHA, CODIGO_DIV1)] %>%
  as.data.frame() %>%
  stats::reshape(timevar = "FECHA", idvar = "CODIGO_DIV1", direction = "wide")
names(cluster_db) <- c("id", paste("t",1:(ncol(cluster_db)-1)))
cluster_cld <- kml::cld(cluster_db)
kml::kml(cluster_cld, nbRedrawing = 10)
# Usaremos 3
clusters_ids <- kml::getClusters(cluster_cld, 3)
cluster_ids_col <- rep(clusters_ids, each = length(unique(main_DT$FECHA)))
cluster_DT <- main_DT[,.N,.(FECHA, CODIGO_DIV1)] %>% setorder(CODIGO_DIV1)
cluster_DT$clusters <- cluster_ids_col
# Todos
cluster_DT %>%
  ggplot(mapping = aes(x = FECHA, y = N, group = CODIGO_DIV1,
                       color = clusters)) +
  geom_line(size = 1) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = 'black', lineend = 'round',
                                   size = 1),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
######
### 1.3 Otros graficos
######
# Agregados por division uno solamente
cluster_DT %>%
  ggplot(mapping = aes(x = CODIGO_DIV1, y = N, fill = clusters)) +
  geom_boxplot() +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = 'black', lineend = 'round',
                                   size = 1),
        axis.ticks.x = element_line(color = 'black', lineend = 'round',
                                    size = 1),
        axis.ticks.length.x = unit(7, 'pt'),
        axis.title.x = element_text(face = 'bold', size = 13))
######
### 1.4 Patrones en faltantes (fechas)
######
c_dist_neig <- main_DT[,.N,.(FECHA,CODIGO_DIV1,CODIGO_DIV2)]
c_neig_fill <- fill_long(c_dist_neig$N, c_dist_neig[,.(FECHA,CODIGO_DIV2)],
                         date_col = "FECHA", time_delta = "1 month", fill = 0)
c_neig_fill$y[c_neig_fill$y == 0] <- NA
# Por barrios - Completo
c_neig_fill %>%
  tidyr::pivot_longer(-c(y,FECHA), names_to = "key", values_to = "value") %>%
  dplyr::mutate(is_missing = is.na(y)) %>%
  ggplot2::ggplot(ggplot2::aes(as.numeric(value), FECHA, fill = is_missing)) +
  ggplot2::geom_tile(alpha = 0.6) +
  ggplot2::scale_fill_manual(name = "",
                             values = c("blue", "red"),
                             labels = c("Presente", "Ausente")) +
  ggplot2::xlab("Vecindario") +
  ggplot2::ylab("Fecha") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::theme_bw() +
  ggplot2::coord_flip()
# Por barrios - Agregado de faltantes en meses
c_neig_fill %>%
  tidyr::pivot_longer(-c(y,FECHA), names_to = "key", values_to = "value") %>%
  dplyr::mutate(is_missing = is.na(y)) %>%
  dplyr::group_by(FECHA) %>%
  dplyr::summarise(n_missing = sum(is_missing)) %>%
  ggplot2::ggplot(ggplot2::aes(FECHA, n_missing)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Fecha") +
  ggplot2::ylab("N. perdidos") +
  ggplot2::theme_bw()
# Por barrios - Filtrar entre 2018 y 2020
c_neig_fill %>%
  dplyr::filter(FECHA>=as.Date("2018-01-01") & FECHA<=as.Date("2019-12-31")) %>%
  tidyr::pivot_longer(-c(y,FECHA), names_to = "key", values_to = "value") %>%
  dplyr::mutate(is_missing = is.na(y)) %>%
  ggplot2::ggplot(ggplot2::aes(as.numeric(value), FECHA, fill = is_missing)) +
  ggplot2::geom_tile(alpha = 0.6) +
  ggplot2::scale_fill_manual(name = "",
                             values = c("blue", "red"),
                             labels = c("Presente", "Ausente")) +
  ggplot2::xlab("Vecindario") +
  ggplot2::ylab("Fecha") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::theme_bw() +
  ggplot2::coord_flip()
# Por distritos
c_dist_fill <- fill_long(c_dist_neig$N, c_dist_neig[,.(FECHA,CODIGO_DIV1)],
                         date_col = "FECHA", time_delta = "1 month", fill = 0)
c_dist_fill$y[c_dist_fill$y == 0] <- NA
c_dist_fill %>%
  tidyr::pivot_longer(-c(y,FECHA), names_to = "key", values_to = "value") %>%
  dplyr::mutate(is_missing = is.na(y)) %>%
  ggplot2::ggplot(ggplot2::aes(value, FECHA, fill = is_missing)) +
  ggplot2::geom_tile(alpha = 0.6) +
  ggplot2::scale_fill_manual(name = "",
                             values = c("blue", "red"),
                             labels = c("Presente", "Ausente")) +
  ggplot2::xlab("Distrito") +
  ggplot2::ylab("Fecha") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::theme_bw() +
  ggplot2::coord_flip()
##################
###### 2. Modelos
##################
### 2.1 Modelos de regresion lineal
#' Exploramos ahora los modelos lineales en si
#' Tenemos primero que crear los conjuntos de datos tratados
db_1 <- as.data.frame(main_DT[,.N,.(FECHA, CODIGO_DIV1)])
db_1 <- fill_long(db_1$N, subset(db_1, select = -N),
                  date_col = "FECHA", time_delta = "1 month")
db_dates  <- db_1$FECHA
db_1 <- add_date_split(db_1, "FECHA", week = FALSE, day_range = FALSE,
                       week_day = FALSE)
db_1$ANNO <- as.numeric(db_1$ANNO)
fct_split_db_1 <- split_by_factors(db_1, "CODIGO_DIV1")
# El _1 hace mencion a los grupos sin componentes AR
Y_1 <- db_1$y
X_1 <- subset(db_1, select = -y)
# El _2 hace menciona los grupos aumentados por AR
Y_2 <- unlist(lapply(fct_split_db_1, function(z) tail(z$y,-6)))
X_2 <- lapply(fct_split_db_1, function(z){
  Y <- z$y
  X <- subset(z, select = -y)
  data_aug <- add_laggs(X = X, Y = Y, p = 6)
  data_aug}) %>% dplyr::bind_rows()
# Los splits basados en cada sub-conjunto
split_1 <- create_split_long(Y = Y_1, X = X_1, split_prop = c(0.7,0.15,0.15),
                             factors = "CODIGO_DIV1")
split_2 <- create_split_long(Y = Y_2, X = X_2, split_prop = c(0.7,0.15,0.15),
                             factors = "CODIGO_DIV1")
# Los modelos lineales propuestos
mod_lin_1 <- lin_mod_selector(split_1)
mod_lin_2 <- lin_mod_selector(split_2)
# Diagnostico residual
plot(mod_lin_1$selected_model)
plot(mod_lin_2$selected_model)
# Rendimiento en validacion
c(mod_lin_2$rmse_val, mod_lin_1$rmse_val)
# Medidas de rendimiento
linmod_metrics(mod_lin_2$selected_model, split_2, "train", "all")
linmod_metrics(mod_lin_2$selected_model, split_2, "validation", "all")
# Hubo una mejora
### 2.2 Modelos AR
# Adicionar fechas
X_3 <- X_1
X_3$FECHA <- db_dates
# Creamos el db agregado por fechas
split_3 <- create_split_long(Y = Y_1, X = X_3, factors = "CODIGO_DIV1",
                             split_prop = c(0.7,0.15,0.15))
#' Notas: 1. Fable es molesto cuando se deja automatico, si detecta singularidades
#' para ciertos ordenes, simplemente muere en lugar de seguir con otros donde
#' sea posible el calculo.
#' 2. Fable no reconoce por defecto series temporales mensuales, debe ajustarse
#' en ese caso. Trabajo futuro.
mod_ar <- ar_mod_selector(split_3, dates_col = NULL, factor_split = "CODIGO_DIV1")
# El rmse completo seria
sqrt(sum(sapply(mod_ar$models, function(x){x$rmse_val})^2))
### 2.2 Modelos ARIMA
mod_arima <- arima_mod_selector(split_3, dates_col = NULL, factor_split = "CODIGO_DIV1")
# El rmse completo seria
sqrt(sum(sapply(mod_arima$models, function(x){x$rmse_val})^2))
# Los resumenes
lapply(mod_arima$models,function(x)fabletools::report(x$model))
# Las medidas de rendimiento
metricas <- arima_metrics(mod_arima, split_3, factor_split = "CODIGO_DIV1")
View(metricas)

# Pendiente
a<-mod_arima$models[[30]]$model %>% fabletools::forecast(h = "1 month") %>%
  fabletools::hilo(level = 90) %>% fabletools::unpack_hilo('90%')
aaa<-mod_arima$models[[1]]$model %>%
  fabletools::refit(as_tsibble(tibble(value = split_3$Y$Y_val[1:10],
                                     date = seq(as.Date("2020-01-01"),
                                                as.Date("2020-01-10"),
                                                        by = "1 day"))),
                              reestimate = FALSE)
# Pendiente
fabletools::accuracy(aaa)
?fabletools::accuracy
mod_arima$models[[1]]$model %>%
  fabletools::report()
algo <- data.frame(Y = split_3$Y$Y_train, split_3$X$X_train)
algo <- algo[algo$CODIGO_DIV1 == "15",]
algo2 <-mod_arima$models[[30]]$model %>% fabletools::forecast(h = "1 week") %>%
  fabletools::hilo(level = 90) %>% fabletools::unpack_hilo('90%')
names(algo2)[c(4,5,6)] <- c("M", "SUP", "INF")
algo2$date <- seq(algo$FECHA[nrow(algo)], length = nrow(algo2), by = "1 month")
ggplot(data = algo, mapping = aes(x = FECHA, y = Y)) +
  geom_line() +
  geom_line(data = algo2, mapping = aes(x = date, y = M)) +
  geom_smooth(data = algo2, aes(x = date, y = M, ymax = SUP,
                                   ymin = INF), color = 'darkblue', stat = 'identity')
Y_tibble <- dplyr::tibble(
  date = dates_mix,
  value = c(Y_train, Y_val)
)
