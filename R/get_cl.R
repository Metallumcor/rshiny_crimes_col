#' @encoding UTF-8
#' @title Obtener líneas de contorno para gráficas
#'
#' @description Creación de líneas de contorno a partir de kernels
#'     normales bivariados estimados sobre un conjunto de datos
#'     de coordenadas espaciales y evaluado sobre una grilla.
#'
#' @param df_coords Un \code{data.frame} o \code{tibble} con
#'     columnas de latitud o longitud. Los nombres de ellas deben
#'     poseer un nombre que asemeje la descripción dada.
#' @param grid_size Un vector que contiene el número de puntos
#'     equidistantes en cada dirección sobre los que se estima
#'     la densidad.
#' @param band_width Un vector con dos números positivos que determina
#'     la longitud de banda para el estimador del kernel.
#'
#' @return Una lista que contiene los siguiente elementos:
#' \describe{
#'     \item{sp_obj}{Un objeto \code{SpatialPolygons}.}
#'     \item{cl}{Las líneas de contorno calculadas.}
#'     \item{cl_lev}{Los niveles de las líneas de contorno,
#'     usados para asignar colores en los mapas.}
#'     \item{cl_nlev}{El número de niveles de las líneas
#'     de contorno, usado para asignar colores en los mapas.}
#' }
#' @importFrom KernSmooth bkde2D
#' @importFrom KernSmooth dpik
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp SpatialPolygons
#' @importFrom grDevices contourLines
#' @export
#'
#' @examples
#' \dontrun{
#' crime_1000 <- test2021:::list_to_df(db_crime_london_a)[1:1000,]
#' get_cl(crime_1000)
#' }
get_cl <- function(df_coords, grid_size = c(50,50), band_width = NULL){
  if(!(inherits(df_coords,'data.frame') || inherits(df_coords,'tibble'))){
    stop('Las coordenadas deben estar en un data.frame o tibble',
         call. = FALSE)
  }
  cols <- names(df_coords)
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
  df <- data.frame(df_coords[,lng_slice], df_coords[,lat_slice])
  df <- as.data.frame(sapply(df, as.numeric))
  if(is.null(band_width)){
    bw_d <- c(KernSmooth::dpik(df[,1]), KernSmooth::dpik(df[,2]))
  } else {
    if(!inherits(band_width, 'numeric') || length(band_width) != 2){
      stop('El parametro band_width debe ser numerico de longitud 2',
           call. = FALSE)
    } else {
      bw_d <- band_width
    }
  }
  kd <- KernSmooth::bkde2D(df, bandwidth =  bw_d, gridsize = grid_size)
  cl <- grDevices::contourLines(kd$x1, kd$x2, kd$fhat)
  polys <- lapply(1:length(cl),
                  function(ind){
                    sp::Polygons(
                      list(
                        sp::Polygon(
                        cbind(cl[[ind]]$x, cl[[ind]]$y)
                        )
                      ),
                      ID = ind
                    )
                  })
  sp_polys <- sp::SpatialPolygons(polys)
  cl_lev <- as.factor(sapply(cl, '[[', 'level'))
  cl_nlev <- length(levels(cl_lev))
  return(list(sp_obj = sp_polys, cl = cl,
              cl_lev = cl_lev, cl_nlev = cl_nlev))
}
