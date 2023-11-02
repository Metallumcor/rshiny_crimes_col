###### Rutina de simulacion - hurto de bicicletas

#### Librerias
library(osmdata)
library(sf)
library(test2021)
library(dplyr)
#### Modificar el PATH de ser necesario
PATH <- getwd()
#### Medellin
### Informacion para generar rutinas
##  Proporcionalidad por comunas
df_prop_comuna <- read.csv(paste0(PATH,"/extdata/reporte_medellin_bici.txt"),
                           colClasses = c("character","character","integer"))
df_prop_comuna$NOMBRE_DIV1 <- toupper(df_prop_comuna$NOMBRE_DIV1)
df_prop_comuna$PROP <- prop.table(df_prop_comuna$N)
## Proporcionalidad por sexo (inventado)
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(20, 77, 3))
)
## Proporcionalidad por rango del dia (copiado de Medellin)
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(1868, 2387, 1915, 1791))
)
### Generador de cantidad de hurtos diaria - asumir entre 13 a 25 robos dia
robo_dia <- function(){
  floor(runif(1,13,25))
}
### Vias de Medellin
# Creacion de cajas por ciudad
med_box <- osmdata::getbb('Medellin, Colombia')
# Creacion de los poligonos para hacer trimming
med_pol <- osmdata::getbb('Medellin, Colombia', format_out = "polygon")
# Consultar cuales son los tipos de vias para OSM - Descomentar para correr
# available_tags('highway')
## Query para vias principales, secundarias y terciarias
med_road_ppl <- osmdata::opq(med_box) %>%
  osmdata::add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata::osmdata_sf()
med_road_scnd <- osmdata::opq(med_box) %>%
  osmdata::add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata::osmdata_sf()
med_road_thrd <- osmdata::opq(med_box) %>%
  osmdata::add_osm_feature(key = 'highway', value = 'tertiary') %>%
  osmdata::osmdata_sf()
# El siguiente operado viene de osmdata, no confundir con c de base
# med_road <- c (med_road_ppl, med_road_scnd, med_road_thrd)
# Este es mas ligero
med_road <- c (med_road_ppl, med_road_scnd)
## Triming en el poligono de Medellin
med_road <- osmdata::trim_osmdata(med_road, med_pol)
## Subconjunto de lineas de carreteras
extract_lines <- function(object){
  o_lines <- object$osm_lines
  o_lines <- o_lines[complete.cases(o_lines$name),]
  Encoding(o_lines$name) <- 'UTF-8'
  Encoding(o_lines$addr.city) <- 'UTF-8'
  o_lines
}
### Vias por comunas
# Lista de polys
div1_nom <- unique(medellin$NOMBRE_DIV1)
poly_div1 <- lapply(
  div1_nom,
  function(y){
    osmdata::getbb(paste0('Comuna  ', y,
                          ', Medellín, Colombia'), format_out = "polygon")
  }
)
names(poly_div1) <- div1_nom
# Rutina para simular los puntos diarios
simu_comuna <- function(x, n, object, poly_list){
  ply <- get(x, poly_list)
  o_sub <- osmdata::trim_osmdata(object, ply, exclude = FALSE)
  o_lines <- extract_lines(o_sub)
  # Muestreo dentro de unidad
  result <- sf::st_sample(o_lines, n, type = 'random')
  # Retorno
  do.call(rbind, result)
}
# Rutina para simular toda Medellin en un dia
simu_medellin <- function(day){
  if(!inherits(day,"POSIXct")){
    day <- as.POSIXct(day)
  }
  ncounts <- robo_dia()
  u_comuna <- div1_nom
  probs <- df_prop_comuna$`prop.table(incidencia)`
  components <- sample(1:length(u_comuna), prob = probs, size = ncounts,
                       replace = TRUE)
  # Ajuste de retorno a formato
  df_result <- data.frame(u_comuna[components], rep(day, ncounts))
  names(df_result) <- c("NOMBRE_DIV1", "FECHA")
  # Retorno
  df_result
}
# Rutina para simular toda Medellin en un rango de dias
simu_medellin_range <- function(start_day, end_day){
  date_range <- as.POSIXct(c(start_day, end_day))
  dates <- seq(date_range[1], date_range[2], by = "day")
  result <- lapply(dates,
                   function(y){
                     simu_medellin(day = y)
                   })
  dplyr::bind_rows(result)
}
# Rutina para simular de forma eficiente los puntos en las vias
simu_medellin_vias <- function(simu_df, object, poly_list){
  # Tabla de frecuencias por comuna
  freq_loc <- table(factor(simu_df$NOMBRE_DIV1, levels = div1_nom))
  # Ordenar por comuna y fecha - Esto ayuda los calculos
  order_simu_df <- dplyr::left_join(data.frame(NOMBRE_DIV1 = div1_nom),
                                    simu_df, by = "NOMBRE_DIV1")
  # Simulacion puntual en vias por comuna
  point_result <- lapply(1:length(div1_nom),
                         function(x){
                           simu_comuna(x = div1_nom[x],
                                          n = freq_loc[x],
                                          object = object,
                                          poly_list = poly_list)
                         })
  point_result <- data.frame(do.call(rbind, point_result))
  names(point_result) <- c("LNG", "LAT")
  # Resultado
  result <- cbind(point_result, order_simu_df)
  result$NOMBRE_DIV1 <- NULL
  result
}
### Simulacion actual
# Dia inicial
inicial <- "2010-01-01"
final <- "2021-04-30"
# Simulacion de puntos
set.seed(2021)
simu_df <- simu_medellin_range(inicial, final)
med_hurto_motos <- simu_medellin_vias(simu_df, med_road, poly_div1)
# Simulacion de variables categoricas
med_hurto_motos$SEXO <- sample(df_prop_sexo$SEXO,
                               prob = df_prop_sexo$PROP,
                               size = nrow(med_hurto_motos),
                               replace = TRUE)
med_hurto_motos$RANG_DIA <-  sample(df_prop_rang_dia$RANG_DIA,
                                    prob = df_prop_rang_dia$PROP,
                                    size = nrow(med_hurto_motos),
                                    replace = TRUE)
### Elementos adicionales - comuna y upz
sp_elements <- test2021:::get_location(med_hurto_motos, medellin)
col_sel <- c("CODIGO_DIV1", "CODIGO_DIV2")
med_hurto_motos <- cbind(med_hurto_motos, sp_elements[,col_sel])
med_hurto_motos <- med_hurto_motos[complete.cases(med_hurto_motos),]
### Guardar base de datos
usethis::use_data(med_hurto_motos, overwrite = TRUE)
