#' @encoding UTF-8
#' @title Dibuja un mapa con la localización seleccionada
#'
#' @description Función creada con el fin de ser ejecutada sin el uso de Shiny.
#'     Representa una funcionalidad aproximada de la app crime_visor_london donde
#'     se solicita un mapa ya cargado, una localización y una fecha (puede ser nula).
#'
#'     Argumentos adicionales pueden ser entregados.
#'
#' @param map Un mapa de Londres del paquete \code{leaflet}, creado con
#'     la función \code{\link{draw_city}}. Dejar Nulo para crear un mapa
#'     automatíco usando el conjunto \code{\link{lon_poly_ward_a}}
#'     agregado por *BOROUGH*.
#' @param location El nombre del distrito o *ward* a usar. Dejar nulo para
#'     gráficar todo Londres.
#' @param date Año y mes donde se registran los hechos, usar el formato
#'     de fechas numéricas 'AAAA-MM' (usar ceros a izquierda de ser
#'     necesario). Consultar fechas disponibles en
#'     \code{available_dates}.
#' @param top_n La cantidad de crímenes a reportar. Selecciona las *n*
#'     categorías más frecuentes.
#' @param col_palette Una paleta de colores de longitud igual a *n*. Los
#'     colores deben darse en formato hexadecimal o RGB.
#'
#' @return Un mapa de vectores con adición de marcadores según crímenes.
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' \donttest{
#' crime_london_map(NULL, location = 'City of London', date = '2020-01')
#' }
crime_london_map <- function(map, location, date = NULL, top_n =5,
                             col_palette = NULL){

  if (is.null(map)){
    map <- draw_city(merge_on = 'BOROUGH')
  } else if (!inherits(map, 'leaflet')){
    stop("El argumento 'map' debe ser un mapa leaflet",
         call. = FALSE)
  }

  if (is.null(location)){
    crime_data <- get_crime_london(date = date)
  } else {
    crime_data <- get_crime_london_unit(unit = location, date = date)
  }

  if (is.null(col_palette)){
    col_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")
  }

  cl <-  dplyr::mutate(crime_data, top_n = forcats::fct_lump(factor(category),
                      n = 5,
                      other_level = 'Otros'),
                      latitude = as.numeric(latitude),
                      longitude = as.numeric(longitude))
  crim_pal <- colorFactor(col_palette, cl$top_n)

  map <- map %>% clearMarkers() %>%
         addCircleMarkers(data = cl, clusterOptions = markerClusterOptions(),
                   stroke = FALSE, color = ~crim_pal(top_n),
                   label = ~category, radius = 15) %>%
         addLegend(data = cl, "bottomright", pal = crim_pal, values = ~top_n,
              title = "Categoria")

  return(map)
}
