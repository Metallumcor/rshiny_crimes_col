#' @encoding UTF-8
#' @title Obtener datos de crimen por fecha y localización (Londres)
#' @description Método del paquete que llama la función \code{ukpolice::ukc_crime_poly}
#'     sobre cada una de las divisiones establecidas sobre el mapa, ya sean distritos o
#'     *wards*. Los datos son aproximados por anonimización de las fuentes, según la
#'     documentación de la [API base](https://data.police.uk/docs/) proporcionada por
#'     el departamento de policía de Reino Unido.
#'
#'     Para que esta función pueda correr sin problema alguno, se requiere usar alguno
#'     de las base de datos pre-cargadas y llamar una de las divisiones especificadas.
#'     No es posible obtener toda la información de Londres dentro de un solo REQUEST
#'     por parte de la API (Error 503). Es posible que en algunos casos no se reporte
#'     información para algún año-mes dado
#'
#' @param data Un objeto \code{SpatialPolygonDataFrame} correspondiente
#'     a Londres. Conjuntos disponibles:
#'     \code{\link{lon_poly_ward_a}} o \code{\link{lon_poly_ward_b}}.
#' @param geo_div Nombre de la columna usada para crear las divisiones.
#'     Ver posibles valores en \code{\link{lon_poly_ward_a}} o
#'     \code{\link{lon_poly_ward_b}}.
#' @param date Año y mes donde se registran los hechos, usar el formato
#'     de fechas numéricas 'AAAA-MM' (usar ceros a izquierda de ser
#'     necesario). Consultar fechas disponibles en
#'     \code{available_dates}.
#' @param lang Lenguaje en que se reportan los crímenes: inglés 'ENG'
#'     o español 'ESP'.
#' @param static Bolean. Usar o no el conjunto pre-cargado. Solo
#'     disponible para \code{\link{lon_poly_ward_b}} y nivel distritos.
#' @return Un \code{tibble} que integra la información de crímenes.
#'         [Ver documentación](https://data.police.uk/docs/)
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' get_crime_london()
#' }
get_crime_london <- function(data = lon_poly_ward_a, geo_div = "BOROUGH",
                             date = NULL, lang = 'ESP', static = FALSE){
  data_name <- deparse(substitute(data))

  if (toupper(substr(data_name,1,3)) != 'LON'){
    stop('La funcion solo acepta mapas de Londres, identificados por el cod LON',
         call. = FALSE)
  }

  if (!is.character(geo_div) || !geo_div %in% colnames(data@data)){
    stop('Division invalida o no caracter ingresado',
         call. = FALSE)
  }

  look_up_list <- unique(get(geo_div, data@data))
  crim_data <- lapply(look_up_list,
    function(x){
      Sys.sleep(1)
      crim_data_u <- get_crime_london_unit(unit = x, data = data, geo_div = geo_div,
                                           date = date, lang = lang,
                                           static = static)
      crim_data_u
    }
  )

  dplyr::bind_rows(Filter(Negate(is.null), crim_data))
}

#' @encoding UTF-8
#' @title Obtener datos de crimen por fecha y localización (Londres) - Unidad
#' @description Método del paquete que llama la función \code{ukpolice::ukc_crime_poly}
#'     sobre **UNA** de las divisiones establecidas sobre el mapa, ya sean distritos o
#'     *wards*. Los datos son aproximados por anonimización de las fuentes, según la
#'     documentación de la [API base](https://data.police.uk/docs/) proporcionada por
#'     el departamento de policía de Reino Unido.
#'
#' @param unit Nombre de la localización. Ver posibles valores en
#'     \code{\link{get_london_boroughs}} o
#'     \code{\link{get_london_districts}}.
#' @param data Un objeto \code{SpatialPolygonDataFrame} correspondiente
#'     a Londres. Conjuntos disponibles:
#'     \code{\link{lon_poly_ward_a}} o \code{\link{lon_poly_ward_b}}.
#' @param geo_div Nombre de la columna usada para crear las divisiones.
#'     Ver posibles valores en \code{\link{lon_poly_ward_a}} o
#'     \code{\link{lon_poly_ward_b}}.
#' @param date Año y mes donde se registran los hechos, usar el formato
#'     de fechas numéricas 'AAAA-MM' (usar ceros a izquierda de ser
#'     necesario). Consultar fechas disponibles en
#'     \code{available_dates}.
#' @param lang Lenguaje en que se reportan los crímenes: inglés 'ENG'
#'     o español 'ESP'.
#' @param static Bolean. Usar o no el conjunto pre-cargado. Solo
#'     disponible para \code{\link{lon_poly_ward_b}} y nivel distritos.
#' @return Un \code{tibble} que integra la información de crímenes.
#'         [Ver documentación](https://data.police.uk/docs/)
#' @export
#' @importFrom maptools unionSpatialPolygons
#' @importFrom rgeos gSimplify
#' @importFrom ukpolice ukc_crime_poly
#'
#' @examples
#' \donttest{
#' get_crime_london_unit('City of London')
#' }
get_crime_london_unit <- function(unit, data = lon_poly_ward_a, geo_div = "BOROUGH",
                                  date = NULL, lang = 'ESP', static = FALSE){
  s_unit <- subset(data, get(geo_div, data@data) == unit)
  s_unit_union <- maptools::unionSpatialPolygons(s_unit, get(geo_div, s_unit@data))
  s_unit_union <- rgeos::gSimplify(s_unit_union, tol = 0.001, topologyPreserve = TRUE)
  poly_df <- data.frame(s_unit_union@polygons[[1]]@Polygons[[1]]@coords)
  names(poly_df) <- c('lng','lat')
  if (static){
    if (geo_div != 'DISTRICT'){
      stop('La version estatica solo funciona con distritos',
           call. = FALSE)
    }

    if (is.null(date)){
      date <- available_dates(static = TRUE)
      date <- date[length(date)]
    }

    crim_data_u <- data.table::as.data.table(db_crime_london_a[[unit]])
    crim_data_u <- crim_data_u[month == date]
    crim_data_u <- as.data.frame(crim_data_u)
    #crim_data_u <- dplyr::filter(crim_data_u, month == date)

  } else{
    crim_data_u <- ukpolice::ukc_crime_poly(poly_df = poly_df, date = date)
    if(!is.null(crim_data_u)){
      crim_data_u <- subset(crim_data_u, select = -c(context, type, subtype, persistent_id,
                                                     id, street_id, street_name, outcome_status_date,
                                                     outcome_status_category))
      crim_data_u <- get_crim_cat(crim_data_u, lang)
    }
  }

  crim_data_u
}
