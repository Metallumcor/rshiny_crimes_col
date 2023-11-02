#' @encoding UTF-8
#' @title Creación de polígonos hexagonales para mapas
#'
#' @description Transforma una base de datos de puntos geo-localizados
#'     (latitud-longitud) en una grilla de hexágonos con información
#'     adjunta, ideal para la creación de gráficas y mapas que soporten
#'     los objetos tipo \code{SpatialPolygonsDataFrame}.
#'
#' @param db_pnts Un \code{data.frame} o \code{data.table} que contiene
#'     dos columnas de latitud o longitud, cuyos nombres deben reflejar
#'     esto. Además, de usarse \code{agg_col}, esta debe de estar incluida
#'     acá.
#' @param crs El número del sistema de proyección utilizada. Por defecto
#'     se utiliza la EPSG:3118.
#' @param n_hex La cantidad de hexágonos que tiene la grilla.
#' @param buffer Un buffer a la distancia de al casco convexo usado para
#'     crear la grilla hexagonal. Por defecto es 5\%.
#' @param agg_col El nombre de la columna sobre la cual se agrega. Dejar
#'     nulo si se usa \code{agg_fun} como \code{count}.
#' @param agg_fun El nombre de la función que agrega o una expresión que
#'     se pueda pasar a \code{R}.
#'
#' @return Una grilla hexagonal contenida en una clase
#'     \code{SpatialPolygonsDataFrame}.
#' @importFrom sf st_as_sf
#' @importFrom sf as_Spatial
#' @importFrom sf st_convex_hull
#' @importFrom sf st_union
#' @importFrom sf st_coordinates
#' @importFrom sf st_buffer
#' @importFrom sp spsample
#' @importFrom sp HexPoints2SpatialPolygons
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom stats aggregate
#' @importFrom stats dist
#' @export
#'
#' @examples
#' \dontrun{
#' # Base de datos
#' db_1 <- test2021:::list_to_df(db_crime_london_a)
#' db_1 <- data.table::as.data.table(db_1)
#' # Filtrada
#' db_2 <- db_1[ category == 'Drogas' & month == '2019-01']
#' hex_drogras <- hex_bins(db_2, crs = 27700)
#' }
hex_bins <- function(db_pnts, crs, n_hex = 500, buffer = 0.05,
                     agg_col = NULL,  agg_fun = "count"){
  ### Integridad de inputs
  if (!inherits(db_pnts, c("data.table", "data.frame"))){
    stop("La base de datos (db_pnts) de puntos debe ser data.table o data.frame",
         call. = FALSE)
  }
  cols <- names(db_pnts)
  lat_slice <- grepl('^?(lat).*$', cols, ignore.case = TRUE)
  lng_slice <- grepl('^?(l).?(n).?(g).*$', cols, ignore.case = TRUE)
  if(sum(lat_slice)==0){
    stop('No se encuetra la columna de latitud',
         call. = FALSE)
  }
  if(sum(lng_slice)==0){
    stop('No se encuetra la columna de longitud',
         call. = FALSE)
  }
  #
  if (missing(crs)){
    warning("Usando la proyeccion EPSG:3118 - Colombia (IGAC)")
    crs = 3118
  }
  #
  if (!is.numeric(n_hex)){
    stop("Ingresar una cantidad entera positiva de hexagonos")
  }
  # Faltan clausulas para agg_col y agg_fun
  ### Evaluacion
  # Puntos
  # sf - El parametro agr tiene un hardcode en funcion de las aplicaciones
  points_sf <- sf::st_as_sf(db_pnts,
                            coords = c(cols[lng_slice],cols[lat_slice]),
                            crs = crs, agr = 'identity')
  # sp
  points_sp <- sf::as_Spatial(points_sf)
  # Convex hull - Para poder encerrar los puntos bien
  c_hull <- sf::st_convex_hull(sf::st_union(points_sf))
  # Buffer
  max_dist <- max(stats::dist(sf::st_coordinates(points_sf)))
  c_hull <- sf::st_buffer(c_hull, max_dist * buffer)
  c_hull_sp <- sf::as_Spatial(c_hull)
  # Creacion del hexbin
  h_points <- sp::spsample(c_hull_sp, n = n_hex, type = "hexagonal")
  h_pol <- sp::HexPoints2SpatialPolygons(h_points)
  # Identificar el hex al que pertenece cada punto
  points_sp$hexbin <- over(points_sp, h_pol)
  # Agregar los puntos
  if(is.null(agg_col) || agg_fun == 'count'){
    agg_col = "dummy"
    points_sp$dummy <- 1
    agg_fun = "sum"
  }
  summ_stat <- stats::aggregate(points_sp@data[,agg_col],
                         by = list(points_sp$hexbin),
                         FUN = agg_fun)
  names(summ_stat) <- c("ID", agg_fun)
  # Crear salida
  h_pol <- sp::SpatialPolygonsDataFrame(h_pol,
                                    data.frame(ID = 1:length(h_pol),
                                               row.names = row.names(h_pol)))
  h_pol@data <- merge(h_pol@data, summ_stat, by = "ID", all.x = TRUE)
  fillna0(h_pol@data)
  # Retorno
  h_pol
}
