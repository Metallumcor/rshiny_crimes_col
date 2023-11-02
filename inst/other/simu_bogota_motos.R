###### Rutina de simulacion - hurto de motos

#### Librerias
library(osmdata)
library(sf)
library(test2021)
library(dplyr)
#### Modificar el PATH de ser necesario
PATH <- getwd()
#### Bogota
### Informacion para generar rutinas
##  Proporcionalidad por localidades - Excluir SIN LOCALIZACION y SUMAPAZ
incidencia <- c(59, 41, 315, 9, 17, 242, 220, 103, 450,
                48, 90, 109, 158, 14, 245, 26, 72, 82, 97)
lista_localidades <- unique(bogota@data[,c("CODIGO_DIV1","NOMBRE_DIV1")])
df_prop_localidad <- cbind(lista_localidades[order(lista_localidades$NOMBRE_DIV1),],
                           prop.table(incidencia))
## Proporcionalidad por sexo
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(307, 2089, 1))
)
## Proporcionalidad por rango del dia
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(208, 662, 619, 1518))
)
### Generador de cantidad de hurtos diaria - asumir entre 10 a 20 robos dia
robo_dia <- function(){
  round(runif(1,10,20),0)
}
### Vias de Bogota
# Creacion de cajas por ciudad
bog_box <- osmdata::getbb('Bogota, Colombia')
# Creacion de los poligonos para hacer trimming
bog_pol <- osmdata::getbb('Bogota, Colombia', format_out = "polygon")
# Consultar cuales son los tipos de vias para OSM - Descomentar para correr
# available_tags('highway')
## Query para vias principales, secundarias y terciarias
bog_road_ppl <- osmdata::opq(bog_box) %>%
  osmdata::add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata::osmdata_sf()
bog_road_scnd <- osmdata::opq(bog_box) %>%
  osmdata::add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata::osmdata_sf()
#bog_road_thrd <- osmdata::opq(bog_box) %>%
#  osmdata::add_osm_feature(key = 'highway', value = 'tertiary') %>%
#  osmdata::osmdata_sf()
# El siguiente operado viene de osmdata, no confundir con c de base
# Este peso algo como 20 megas
# bog_road <- c (bog_road_ppl, bog_road_scnd, bog_road_thrd)
# El de aca 10
bog_road <- c (bog_road_ppl, bog_road_scnd)
## Triming en el poligono de Bogota
bog_road <- osmdata::trim_osmdata(bog_road, bog_pol)
## Subconjunto de lineas de carreteras
extract_lines <- function(object){
  o_lines <- object$osm_lines
  o_lines <- o_lines[complete.cases(o_lines$name),]
  Encoding(o_lines$name) <- 'UTF-8'
  Encoding(o_lines$addr.city) <- 'UTF-8'
  o_lines
}
### Vias por localidades
# Lista de polys
div1_nom <- unique(bogota$NOMBRE_DIV1)
div1_nom <- sub('RAFAEL URIBE', 'RAFAEL URIBE URIBE', div1_nom)
poly_div1 <- lapply(
  div1_nom,
  function(y){
    osmdata::getbb(paste0('localidad ', y,
                          ' Bogota, Colombia'), format_out = "polygon")
  }
)
names(poly_div1) <- div1_nom
# Rutina para simular los puntos diarios
simu_localidad <- function(x, n, object, poly_list){
  ply <- get(x, poly_list)
  o_sub <- osmdata::trim_osmdata(object, ply, exclude = FALSE)
  o_lines <- extract_lines(o_sub)
  # Muestreo dentro de unidad
  result <- sf::st_sample(o_lines, n, type = 'random')
  # Retorno
  do.call(rbind, result)
}
# Rutina para simular toda Bogota en un dia
simu_bogota <- function(day){
  if(!inherits(day,"POSIXct")){
    day <- as.POSIXct(day)
  }
  ncounts <- robo_dia()
  u_localidad <- div1_nom
  probs <- df_prop_localidad$`prop.table(incidencia)`
  components <- sample(1:length(u_localidad), prob = probs, size = ncounts,
                       replace = TRUE)
  # Ajuste de retorno a formato
  df_result <- data.frame(u_localidad[components], rep(day, ncounts))
  names(df_result) <- c("NOMBRE_DIV1", "FECHA")
  # Retorno
  df_result
}
# Rutina para simular toda Bogota en un rango de dias
simu_bogota_range <- function(start_day, end_day){
  date_range <- as.POSIXct(c(start_day, end_day))
  dates <- seq(date_range[1], date_range[2], by = "day")
  result <- lapply(dates,
                   function(y){
                     simu_bogota(day = y)
                   })
  dplyr::bind_rows(result)
}
# Rutina para simular de forma eficiente los puntos en las vias
simu_bogota_vias <- function(simu_df, object, poly_list){
  # Tabla de frecuencias por localidad
  freq_loc <- table(factor(simu_df$NOMBRE_DIV1, levels = div1_nom))
  # Ordenar por localidad y fecha - Esto ayuda los calculos
  order_simu_df <- dplyr::left_join(data.frame(NOMBRE_DIV1 = div1_nom),
                                    simu_df, by = "NOMBRE_DIV1")
  # Simulacion puntual en vias por localidad
  point_result <- lapply(1:length(div1_nom),
                         function(x){
                           simu_localidad(x = div1_nom[x],
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
simu_df <- simu_bogota_range(inicial, final)
bog_hurto_motos <- simu_bogota_vias(simu_df, bog_road, poly_div1)
# Simulacion de variables categoricas
bog_hurto_motos$SEXO <- sample(df_prop_sexo$SEXO,
                                    prob = df_prop_sexo$PROP,
                                    size = nrow(bog_hurto_motos),
                                    replace = TRUE)
bog_hurto_motos$RANG_DIA <-  sample(df_prop_rang_dia$RANG_DIA,
                                         prob = df_prop_rang_dia$PROP,
                                         size = nrow(bog_hurto_motos),
                                         replace = TRUE)
### Elementos adicionales - localidad y upz
sp_elements <- test2021:::get_location(bog_hurto_motos, bogota)
col_sel <- c("CODIGO_DIV1", "CODIGO_DIV2")
bog_hurto_motos <- cbind(bog_hurto_motos, sp_elements[,col_sel])
bog_hurto_motos <- bog_hurto_motos[complete.cases(bog_hurto_motos),]
### Guardar base de datos
usethis::use_data(bog_hurto_motos, overwrite = TRUE)
