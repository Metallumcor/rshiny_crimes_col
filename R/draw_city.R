#' @encoding UTF-8
#' @title Dibujar el mapa de la ciudad seleccionada
#'
#' @param city_file Un objeto de tipo \code{SpatialPolygonDataFrame}.
#' @param merge_on En que unidades el mapa debe integrar sus divisiones.
#'     Consultar las columnas del archivo \code{city_file} suministrado
#'     sobre las cuales se agregan los polígonos.
#'
#' @importFrom maptools unionSpatialPolygons
#' @return Un mapa de vectores. Por defecto un mapa de Londres y sus
#'     *wards*.
#' @export
#'
#' @examples
#' \donttest{
#' draw_city()
#' }
draw_city <- function(city_file = lon_poly_ward_a, merge_on = NULL){
  #valid_entries <- list.files(system.file("data", package = "test2021"), all.files = TRUE)
  #city_codes <- toupper(unique(substr(valid_entries, 1, 3)))
  #valid_msg <- paste0('Los codigos disponibles son: "',
  #    paste(valid_entries, collapse = '", ",'),'"')

  #if (!is.character(location) || !is.character(type)){
  #  stop('Los parametros de la funcion deben ser caracteres!',
  #       call. = FALSE)
  #} else if (!toupper(location) %in% city_codes){
  #  stop('El codigo de ciudad no es valido.\n',
  #       valid_msg,
  #       call. = FALSE)
  #} else if (!toupper(type) %in% c('A', 'B')){
  #  stop('Tipo no valido de mapa',
  #       call. = FALSE)
  #}

  #patt <- paste0('^',tolower(location),'.*?',tolower(type),'.rda$')
  #city <- valid_entries[grepl(patt, valid_entries)]
  #city_file <- get(gsub('.rda', '', city))

  if(!is.null(merge_on)){
    if(!merge_on %in% colnames(city_file@data)){
      print('Categoria no encontrada en los datos. Datos originales devueltos.\n')
    } else {
      merge_id <- get(merge_on, city_file@data)
      city_file <- maptools::unionSpatialPolygons(city_file, merge_id)
    }
  }
  plot_map <- leaflet(city_file) %>%
    addPolygons(color = '#36FFFF', weight = 3, smoothFactor = 0.5,
                opacity = 0.8, fill = FALSE) %>%
    addTiles()


  return(plot_map)
}
