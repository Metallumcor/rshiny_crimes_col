#' @encoding UTF-8
#' @title Transformación de categorías en código a valores significativos
#'
#' @param x Base de datos de crímenes
#' @param lang Lenguaje usada en tabla
#'
#' @return Categorías traducidas
get_crim_cat <- function(x, lang = 'ESP'){
  if(!is.character(lang) || !lang %in% c('ESP', 'ENG')){
    stop('Lenguaje no encontrado en tabla',
         call. = FALSE)
  }
  # Se mejoro rendimiento dplyr -> data.table
  x <- data.table::as.data.table(x)
  y <- data.table::merge.data.table(x, look_crimes_cat,
                                    by.x = 'category', by.y = 'URL',
                                    all.x = TRUE)
  x[,('category') := get(lang, y)]
  as.data.frame(x)
}

#' @encoding UTF-8
#' @title Obtención de centroide para visualización
#'
#' @param unit Unidad de división geo-política.
#' @param data Conjunto de datos espaciales:
#'     \code{\link{lon_poly_ward_a}} o \code{\link{lon_poly_ward_b}}.
#' @param geo_div Nombre de la columna usada para crear las divisiones.
#'     Ver posibles valores en \code{\link{lon_poly_ward_a}} o
#'     \code{\link{lon_poly_ward_b}}.
#' @importFrom maptools unionSpatialPolygons
#' @importFrom rgeos gSimplify
#' @importFrom rgeos gCentroid
#'
#' @return Centroide
get_centroid <- function(unit, data = lon_poly_ward_a, geo_div = 'BOROUGH'){
  s_unit <- subset(data, get(geo_div, data@data) == unit)
  s_unit_union <- maptools::unionSpatialPolygons(s_unit, get(geo_div, s_unit@data))
  s_unit_union <- rgeos::gSimplify(s_unit_union, tol = 0.001, topologyPreserve = TRUE)
  centroid  <- rgeos::gCentroid(s_unit_union)

  centroid@coords
}

#' @encoding UTF-8
#' @title Devuelve un listado de los *boroughs* (Londres)
#'     disponibles
#'
#' @return Listado de *boroughs* de Londres.
#' @export
get_london_boroughs <- function(){
  data = lon_poly_ward_a
  geo_div = 'BOROUGH'
  unique(get(geo_div, data@data))
}

#' @encoding UTF-8
#' @title Devuelve un listado de los  distritos (Londres)
#'     disponibles
#'
#' @return Listado de distritos de Londres.
#' @export
get_london_districts <- function(){
  data = lon_poly_ward_b
  geo_div = 'DISTRICT'
  unique(get(geo_div, data@data))
}

#' @encoding UTF-8
#' @title Fechas disponibles para Londres.
#'
#' @param static Bolean, falso por defecto. En caso de ser
#'     verdadero se devuelven las fechas disponibles del
#'     conjunto de datos estático TODO.
#' @return Listado de fechas formateadas año y mes.
#' @export
#' @importFrom  dplyr bind_rows
available_dates <- function(static = FALSE){
  if(static){
    data_aval <- dplyr::bind_rows(db_crime_london_a)$month
    data_aval_inter <- unique(data_aval)

  } else {
    data_aval <- as.data.table(ukpolice::ukc_available())
    data_aval2 <- as.POSIXct(paste0(data_aval[c(.N,1),date,],'-01'))
    data_aval_inter <- seq(from = data_aval2[1], to = data_aval2[2],
                           by = "month")
    data_aval_inter <- format(data_aval_inter, '%Y-%m')
  }

  data_aval_inter
}

#' @encoding UTF-8
#' @title Convertir lista a \code{data.frame}
#'
#' @description El nombre de los elementos se convierte en una columna.
#' @param z Una lista de \code{data.frames}
#' @param col_name Nombre que se asigna a la columna creada.
#' @importFrom dplyr bind_rows
list_to_df <- function(z, col_name = 'DISTRICT'){
  z_wcol <- mapply(function(x,y){
    x[,col_name] <- y
    x
  }, z, names(z), SIMPLIFY = F)

  dplyr::bind_rows(z_wcol)
}

#' @encoding UTF-8
#' @title Obtener el objeto donde pertenece un punto
#'
#' @description Examina una lista de polígonos y devuelve aquel donde
#'     pertenece (de existir)
#' @param v Coordenadas del punto (long, lat)
#' @param sp_ob Objeto \code{SpatialPolygonsDataFrame}.
#'
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp over
get_location <- function(v, sp_ob){
  if(inherits(v, c("data.frame", "data.table"))){
    cols <- names(v)
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
    v <- as.data.frame(v)
    sp_point <- data.frame(v[,lng_slice], v[,lat_slice])
    names(sp_point) <- c("x", "y")
  } else {
    sp_point <- data.frame(x = v[1], y = v[2])
  }
  sp::coordinates(sp_point) <- c('x', 'y')
  sp::proj4string(sp_point) <- sp::proj4string(sp_ob)
  sp::over(sp_point, sp_ob)
}

#' @encoding UTF-8
#' @title Filtro crimen/fecha - POR REMOVER
#'
#' @param x Datos a filtrar.
#' @param crime_s Crimen (string).
#' @param date_s Fecha (string).
#' @param crim_col Nombre columna con crimenes.
#' @param date_col Nombre columna con fechas.
#' @importFrom dplyr filter
filter_crime <- function(x, crime_s, date_s, crim_col, date_col){
  .Deprecated("dt_filter_crime")
   x <- dplyr::filter(.data = x,
                      !!as.name(crim_col) == crime_s,
                      !!as.name(date_col) == date_s)
   x
}
# Filtrado de data.table para Shiny
dt_filter_crime <- function(x, list_filter){
  if(!inherits(x, 'data.table')){
    stop('Se requiere de un data.table para proceder')
  }
  cj_filter <- do.call(data.table::CJ, list_filter)
  data.table::setkeyv(x, names(cj_filter))
  x[cj_filter,,]
}
# Llenar entradas numericas de NA con ceros
fillna0 <- function(x){
  for(j in seq_along(x)){
    if (is.numeric(x[[j]]) )
      set(x, i = which(is.na(x[[j]])), j = j, value = 0)
  }
}
# Chequear paquetes
is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

# Dividir matrices en bloques
# Credito MrFlick (https://stackoverflow.com/questions/24299171/function-to-split-a-matrix-into-sub-matrices-in-r)
matsplitter<-function(M, r, c) {
  chk_val <- sum(mapply(function(x,y) x%%y, x = dim(M), y = c(r,c)))
  if(chk_val != 0) stop("Split cannot be done since dimentions don't match")
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
}

pcor <- function (x, method = c("pearson", "kendall", "spearman"))
{
  method <- match.arg(method)
  if (is.data.frame(x))
    x <- as.matrix(x)
  if (!is.matrix(x))
    stop("supply a matrix-like 'x'")
  if (!(is.numeric(x) || is.logical(x)))
    stop("'x' must be numeric")
  stopifnot(is.atomic(x))
  n <- dim(x)[1]
  gp <- dim(x)[2] - 2
  cvx <- cov(x, method = method)
  if (det(cvx) < .Machine$double.eps) {
    warning("The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.")
    icvx <- MASS::ginv(cvx)
  }
  else icvx <- solve(cvx)
  pcor <- -stats::cov2cor(icvx)
  diag(pcor) <- 1
  if (method == "kendall") {
    statistic <- pcor/sqrt(2 * (2 * (n - gp) + 5)/(9 * (n -
                                                          gp) * (n - 1 - gp)))
    p.value <- 2 * stats::pnorm(-abs(statistic))
  }
  else {
    statistic <- pcor * sqrt((n - 2 - gp)/(1 - pcor^2))
    p.value <- 2 * pt(-abs(statistic), (n - 2 - gp))
  }
  diag(statistic) <- 0
  diag(p.value) <- 0
  list(estimate = pcor, p.value = p.value, statistic = statistic,
       n = n, gp = gp, method = method)
}
