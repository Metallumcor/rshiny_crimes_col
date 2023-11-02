# Funcion auxiliar para filtrar los datos de Colombia
col_selector_filter <- function(input_crime, input_date){
  # Mirar en la look-up table para identificar la db y otra info
  db_col <- look_colombia_crime[
    which(look_colombia_crime$selector == input_crime), 2]
  sub_type <- look_colombia_crime[
    which(look_colombia_crime$selector == input_crime), 1]
  r_db <- get(db_col)
  # Creacion de los filtros para data.table
  seq_dates <- data.table::as.IDate(
    seq(as.POSIXct(input_date[1]), as.POSIXct(input_date[2]),
        by = 'day'))
  if (is.na(sub_type)){
    list_filter <-
      list('fecha_hecho' = seq_dates)
  } else {
    sub_type_col = ifelse('tipo_de_hurto' %in% names(r_db),
                          'tipo_de_hurto',
                          'descripci_n_conducta')
    list_filter <-
      list(sub_type, seq_dates)
    names(list_filter) <- c(sub_type_col, 'fecha_hecho')
  }
  # Filtrado y creacion de codigos nuevos
  db_crime <- dt_filter_crime(r_db, list_filter)
  db_crime <- cod_dane(db_crime, 'codigo_dane')
  # Nuevo dataset
  db_crime[,c('DPTO_CCDGO', 'MPIO_CCDGO', 'fecha_hecho'),]
}
# Funcion auxiliar para seleccionar el shape-file y unirlo si es el caso
city_selector <- function(x, geo_div, var_sum = NULL){
  if(x == 'Bogot\u00E1'){
    shp_file <- bogota
  }
  if(x == 'Medell\u00EDn'){
    shp_file <- medellin
  }
  if(x == 'Cartagena'){
    shp_file <- cartagena
  }
  if(x == 'Londres'){
    shp_file <- londres
  }
  if (geo_div[1] == 'RAW'){
    return(shp_file)
  }
  sp_crime_pre(shp_file, geo_div, var_sum)
}
# Funcion auxiliar para unir base de datos con shape-file
city_crime <- function(df , shp_file, geo_div){
  db_sp_merge(df, shp_file, geo_div)
}
# Funcion auxiliar para asignar valores reactivos en crime_colombia_city
# Introducir otros geo_div y valores reactivos cuando sea necesario
prepro_colombia <- function(db, city_sel, date_sel){
  # Nombres de las geo_div
  geo_div <- "CODIGO_DIV1"
  # Procesamiento de la base de datos
  db <- data.table::as.data.table(db)
  db_crime <- db[FECHA_MES == date_sel]
  db_crime2 <- db_crime[,.("N" = sum(NUM_HECHOS)), by = geo_div]
  # Seleccion del objeto espacial
  sp_obj1 <- city_selector(city_sel, geo_div = "RAW")
  sp_obj2 <- city_selector(city_sel,
                           c("CODIGO_DIV1", "NOMBRE_DIV1"),
                           "AREA_HECTA")[[1]]
  # Procesamiento del objeto espacial
  centroids <- as.data.frame(
    rgeos::gCentroid(sp_obj2, byid = TRUE)@coords
  )
  names(centroids) <- c('lng', 'lat')
  sp_obj2@data <- cbind(sp_obj2@data, centroids)
  sp_obj3 <- city_crime(db_crime, sp_obj2, geo_div)
  sp_obj4 <- city_crime(db_crime2, sp_obj2, geo_div)
  fillna0(sp_obj2@data)
  fillna0(sp_obj3@data)
  # Objeto de retorno - Lista
  return(list(db_crime = db_crime,
              db_crime2 = db_crime2,
              sp_obj1 = sp_obj1,
              sp_obj2 = sp_obj2,
              sp_obj3 = sp_obj3,
              sp_obj4 = sp_obj4))
}
# Funcion auxiliar para asignar valores reactivos en crime_colombia_city_2
# Esta version de prepro_colombia funciona a un nivel mas granular
prepro_points <- function(db, city_sel, date_sel){
  # Nombres de las geo_div
  geo_div <- c("CODIGO_DIV1", "CODIGO_DIV2")
  # Procesamiento de la base de datos
  if(!inherits(db$FECHA,"POSIXct")){
    db$FECHA <- as.POSIXct(paste0(db$FECHA,"-01"))
  }
  db <- data.table::as.data.table(db)
  db_crime <- db[FECHA >= date_sel[1] & FECHA <= date_sel[2]]
  db_crime2 <- db_crime[,.N, by = geo_div]
  db_crime3 <- db_crime2[,.("N" = sum(N)), by = eval(geo_div[1])]
  # Seleccion del objeto espacial
  sp_obj1 <- city_selector(city_sel, geo_div = "RAW")
  sp_obj2 <- city_selector(city_sel,c("CODIGO_DIV1","NOMBRE_DIV1"),
                           "AREA_HECTA")[[1]]
  # Procesamiento del objeto espacial
  centroids <- as.data.frame(
    rgeos::gCentroid(sp_obj2, byid = TRUE)@coords
  )
  names(centroids) <- c('lng', 'lat')
  sp_obj2@data <- cbind(sp_obj2@data, centroids)
  sp_obj3 <- city_crime(db_crime3, sp_obj2, geo_div[1])
  fillna0(sp_obj3@data)
  # Objeto de retorno - Lista
  return(list(db_crime = db_crime,
              db_crime2 = db_crime2,
              db_crime3 = db_crime3,
              sp_obj1 = sp_obj1,
              sp_obj2 = sp_obj2,
              sp_obj3 = sp_obj3))
}
# Esta funcion ayuda a renderizar la lista de opciones para las aplicaciones
droplist_crimes <- function(city){
  if (city == 'Londres'){
    return(look_crimes_cat$ESP[-1])
  }
  c("Hurto de bicicletas", "Hurto de motos")
}

# Funcion de coordenadas para registrar el centroide del mapa
coord_city <- function(x){
  if (x == 'Bogot\u00E1'){
    lng = -74.08175
    lat = 4.60971
    zoom = 11
  } else if (x == 'Medell\u00EDn'){
    lng = -75.56359
    lat = 6.25184
    zoom = 12
  } else if (x == 'Cartagena'){
    lng = -75.51444
    lat = 10.39972
    zoom = 13
  } else if (x == 'Londres'){
    lng = -0.135278
    lat = 51.494720
    zoom = 11
  }
  return(list(lng = lng, lat = lat, zoom = zoom))
}
# Funcion de mapa base
base_map <- function(x){
  leaflet() %>%
    clearShapes() %>%
    clearControls() %>%
    clearMarkers() %>%
    leaflet.extras::clearHeatmap() %>%
    removeLayersControl() %>%
    addTiles() %>%
    setView(lng = x$lng, lat = x$lat, zoom = x$zoom)
}
# Funcion de asignacion de base de datos
db_call <- function(city){
  if(city == "Bogot\u00E1") quote(siedco_wc)
  else NULL
}
# Funciones de leyendas - Mapas
pretty_legend <- function(x){
  if (is.numeric(x)){
    y <- c(0,x[1:(length(x)-1)])
    paste(round(y,0),"a",round(x,0))
  } else {
    y <- c("0",as.character(x[1:(length(x)-1)]))
    paste(y,"a",x)
  }
}
pretty_legend_2 <- function(x){
  if (is.numeric(x)){
    y1 <- c(x[2:(length(x)-1)])
    y2 <- c(x[3:length(x)])
    c("Ninguno", paste("Entre",round(y1,0),"a",round(y2,0)))
  } else {
    y1 <- as.character(x[2:(length(x)-1)])
    y2 <- as.character(x[3:length(x)])
    c("Ninguno", paste("Entre",y1,"a",y2))
  }
}
# Funcion de paleta de colores - Hexmap
pal_fun_hex <- function(x, FUN){
  sapply(x,
         function(y){
           if(y == 0){
             "transparent"
           } else {
             FUN(y)
           }
         })
}
# Obtener fines de semana
weekend <- function(fechas){
  w_look <- c(1,7)
  data.table::wday(fechas) %in% w_look
}
# Mapeo de colores para correlacion
color_assign <- function(dat,
                         selection,
                         color,
                         special = "#ADFF2F")
{
  cor_mat <- cor(dat, method = "pearson")
  DAT <- cor_mat[selection,]
  DAT <- data.frame(SECTOR = names(DAT), VAL = unname(DAT))
  newcorr = (DAT$VAL + 1)/2
  newcorr[newcorr <= 0] = 0
  newcorr[newcorr >= 1] = 1 - 1e-16
  DAT$COLOR <- color[floor(newcorr * length(color)) + 1]
  DAT$COLOR[which(DAT$VAL==1)] <- special
  return(DAT[,c(1,3)])
}
