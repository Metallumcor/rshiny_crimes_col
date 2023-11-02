#' @encoding UTF-8
#' @title Obtener datos espaciales por tipo de crimen y
#'     fecha - Londres.
#'
#' @description Esta función esta dirigida para el uso interno
#'     de una Shiny App, no obstante, se exporta para otras
#'     aplicaciones. Mezcla una base de datos de crimen en formato
#'     \code{list} o \code{df}, con un objeto \code{SpatialPolygonsDataFrame}
#'     correspondiente al archivo de la ciudad seleccionada, a partir
#'     de una selección de categoría de crimen y fecha. Se requiere
#'     una llamada inicial a la función \code{\link{sp_crime_pre}} para
#'     la lista del argumento \code{sp_pre}.
#'
#' @param crime_s Nombre de la categoría de crimen. En el caso de
#'     Londres se puede llamar a \code{fast_lon_a} (uso interno, ver
#'     manual) para consultar los posibles valores.
#' @param date_s Fecha de filtrado. En los casos estándar de
#'     Londres se espera un formato 'AAAA-MM' en numero.
#' @param sp_pre Una lista devolvida por \code{\link{sp_crime_pre}}.
#' @param df Un \code{data.frame} con información geo-referenciada
#'     de crimen por categorías.
#' @param crim_col Un string que contiene el nombre del campo que
#'     contiene las categorías en \code{df}.
#' @param date_col Un string que contiene el nombre del campo que
#'     contiene las fechas en \code{df}.
#'
#' @return Un \code{SpatialPolygonsDataFrame}
#' @export
#'
#' @examples
#' \dontrun{
#' df_pre <- sp_crime_pre()
#' sp_crime_cat('Drogas', '2020-01', df_pre)
#' }
sp_crime_cat <- function(crime_s, date_s, sp_pre, df = db_crime_london_a,
                         crim_col = 'category', date_col = 'month'){

  # Deshacer lista
  spd_file <- sp_pre[[1]]
  geo_div <- sp_pre[[2]]
  # Obtener la lista transformada con col de geo_div y conteo por crimen
  if(inherits(df,'list')){
    df <- list_to_df(df, geo_div)
    df <- data.table::as.data.table(df)
  }
  if(!inherits(df, 'data.table')){
    df <- data.table::as.data.table(df)
  }
  if(!crime_s %in% get(crim_col, df)){
    stop('El delito (crime_s) no se encuentra en la base de datos')
  }
  if(!date_s %in% get(date_col, df)){
    stop('La fecha (date_s) no se encuentra en la base de datos')
  }

  filter_list <- list(crime_s, date_s)
  names(filter_list) <- c(crim_col, date_col)
  df <- dt_filter_crime(x = df,
                        filter_list)

  df <- df[, .N, by = geo_div]

  # Join y transformacion de datos
  spd_file <- db_sp_merge(df, spd_file, geo_div)
  spd_file$STAT <- spd_file$N/spd_file$AGG

  spd_file
}

#' @encoding UTF-8
#' @title Pre-procesamiento de para \code{\link{sp_crime_cat}}.
#'
#' @description Esta función debe ser llamada antes de la ejecución
#'     del algoritmo principal \code{\link{sp_crime_cat}}. Se deja
#'     como función independiente para evitar procesos redundantes.
#'
#' @param city Un \code{SpatialPolygonsDataFrame} de la ciudad seleccionada.
#' @param geo_div Un string que contiene el nombre del campo que determina
#'     la división geo-política de ínteres, el cual debe estar contenido
#'     en el campo \code{data} del \code{city}. Se acepta también vectores
#'     de strings para múltiples llaves a conservar en el resultado.
#' @param var_sum Un string que contiene el nombre del campo auxiliar a ser
#'     agregado por la agrupación obtenida por \code{geo_div}. Debe ser
#'     un campo númerico de \code{data} en \code{city}.
#'
#' @return Una lista con un \code{SpatialPolygonsDataFrame} y el nombre
#'     del campo usado en \code{geo_div}.
#' @export
#' @importFrom maptools unionSpatialPolygons
#' @importFrom sp SpatialPolygonsDataFrame
#'
#' @examples
#' \dontrun{
#' sp_crime_pre()
#' }
sp_crime_pre <- function(city = lon_poly_ward_b, geo_div = 'DISTRICT',
                         var_sum = 'HECTARES'){
  if(is.null(city) || is.null(geo_div) || is.null(var_sum)){
    stop('Ningun parametro puede ser nulo')
  }

  # Obtener los poligonos agregados
  if(sum(!geo_div %in% colnames(city@data)) != 0){
    stop('geo_div: Categoria no encontrada en los datos',
         call.=FALSE)
  } else {
    merge_id <- get(geo_div[1], city@data)
    city_poly <- maptools::unionSpatialPolygons(city, merge_id)
  }

  # Obtener el df del archivo de ciudad
  df_city <- as.data.table(city@data)
  if(!is.null(var_sum)){
    df_city <- df_city[,.('AGG' = sum(get(var_sum))), by = geo_div]
  } else{
    df_city <- df_city[,.N, by = geo_div]
  }
  df_city <-  as.data.frame(df_city)
  row.names(df_city) <- get(geo_div[1], df_city)

  spd_city <- sp::SpatialPolygonsDataFrame(city_poly, df_city)

  return(list(spd_city = spd_city, geo_div = geo_div))
}
