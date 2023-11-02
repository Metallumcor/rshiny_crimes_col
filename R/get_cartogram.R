#' @encoding UTF-8
#' @title Crear un cartograma a partir de objecto \code{sp}/\code{sf}.
#'
#' @description Se suministra un \code{SpatialPolygonsDataFrame} o un
#'     \code{sf} que contengan una variable sobre la cual se pueda
#'     asignar pesos a las propias geometrías o a círculos dentro de
#'     estas, cuyos tamaños reflejen los valores de dicha variable, y
#'     luego la función retorna un gráfico  en \code{ggplot2}
#'     que muestra el respectivo cartograma.
#'
#'     Los dos tipos de cartogramas posibles son \code{cont} o continuo
#'     el cual deforma las geometrías en función del peso, haciendo más
#'     pequeños los polígonos donde este sea menor y viceversa. Por otro
#'     lado se tiene el cartograma \code{dorling} que crea los círculos
#'     ya mencionados y los posiciona cerca a su lugar de origen, pero
#'     sin que alguno se solape.
#'
#' @param x Un objeto \code{SpatialPolygonsDataFrame} o \code{sf}.
#' @param var_weight El nombre de la columna de la variable numérica sobre
#'     la cual se asignan pesos.
#' @param crs El número de la proyección a usar.
#' @param type El tipo de cartograma, puede ser: cont o dorling.
#' @param itermax La cantidad máxima de veces que el algoritmo
#'     itera.
#'
#' @return Una sf que contiene el cartograma calculado.
#' @importFrom cartogram cartogram_cont
#' @importFrom cartogram cartogram_dorling
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @export
#'
#' @examples
#' \dontrun{
#' # Dibujar los departamentos de Colombia segun su poblacion
#' # Usar el CRS de Colombia - 3118
#' get_cartogram(col_dept, 'TOTPOP', 3118)
#' }
get_cartogram <- function(x, var_weight, crs,
                           type = 'cont',itermax = 10){
  ### Integridad de las input
  if (!inherits(x, c('SpatialPolygonsDataFrame', 'sf'))){
    stop('x debe ser una clase de sp o sf admisible')
  }
  if(!inherits(x, 'sf')){
    x <- sf::st_as_sf(x)
  }
  ### Proyeccion con el CRS proporcionado
  x_sf_proj <- sf::st_transform(x, crs = crs)
  # Usando el tipo de cartograma elegido
  if (type == 'cont'){
    x_carto <- cartogram::cartogram_cont(x_sf_proj,
                                         var_weight, itermax = itermax)
  } else if (type == 'dorling'){
    x_carto <- cartogram::cartogram_dorling(x_sf_proj,
                                         var_weight, itermax = itermax)
  } else {
    stop('Tipo de cartograma no reconocido')
  }
}
