#' @encoding UTF-8
#' @title Simulador en un sistema de vias un delito.
#' @description En esta rutina se simula un delito arbitrario en una ciudad
#'     que cuenta con un sistema vial bien definido, durante una ventana de
#'     tiempo especificada (diario).
#'
#'     El algoritmo asigna una cantidad aleatoria uniforme de cada delito en
#'     un día determinado, luego, se reparte la cantidad obtenida entre
#'     unos rangos horarios establecidos en función de los pesos asignados
#'     (deben sumar uno). Dado un día y un rango horario, los delitos son
#'     asignados a una selección aleatoria de vías la cual viene dada por
#'     unas categorías de velocidades (e.g. lentas y rápidas) con unas
#'     probabilidades personalizadas por el usuario (o dadas por defecto).
#'     Finalmente, dentro de cada vía seleccionada se realiza un muestreo
#'     uniforme en todo el segmento (no necesariamente una recta, tan solo
#'     debe unir dos puntos y debe ser continuo).
#'
#'     La mayor parte de la rutina es automática, pero se puede personalizar
#'     con el mayor detalle posible.
#'
#'     *Nota:* La cantidad de delitos por día siguen la misma distribución
#'     uniforme para todos los días de la semana. Las probabilidades de
#'     observar un delito en un rango de horas/velocidades son asignadas
#'     con un criterio subjetivo.
#'
#' @param range_day El inicio y final de la secuencia de días a simular. Deben
#'     tener un formato admisible como "AAAA-MM-DD" o "DD/MM/AAAA".
#' @param sp_obj El objeto espacial que contiene la información de vías
#'     (geometrías) con columnas auxiliares de velocidad, rango horario,
#'     día de la semana y nombre de la vía.
#' @param ... Una serie de parámetros opcionales destinados a configurar
#'     las opciones internas de funciones a un nivel especifico.
#'
#' @return Un \code{data.frame} que contiene las columnas
#'     \describe{
#'         \item{LAT}{Coordenada de latitud.}
#'         \item{LNG}{Coordenada de longitud.}
#'         \item{HORA}{Rango horario en donde ocurrió el delito. Su nombre
#'         puede ser alterado.}
#'         \item{DIA_SEMANA}{El día de la semana donde ocurrió el delito. Su
#'         nombre puede ser alterado.}
#'         \item{FECHA}{La fecha exacta donde ocurrió el delito. Sin hora.}
#'     }
#' @export
#' @importFrom sf st_as_sf
#' @importFrom stats runif
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#'
#' @examples
#' \donttest{
#' # Simular un delito generico en las vias principales de Bogota, 2020 a 2021
#' delito_bog <- simular_delito_vias(c("2020-01-01","2021-01-01"),
#'     movilidad_marzo_bog, col_factor_rango = "HORA")
#' }
simular_delito_vias <- function(range_day,
                                sp_obj,
                                ...){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  if(!class(range_day) == "Date") range_day <- as.Date(range_day)
  # Secuencia de dias
  seq_day <- seq(range_day[1], range_day[2], by = "1 day")
  # Parametros pasados a funciones internas
  n_pnts_range <- c(20,40)
  col_factor_day <- "DIA_SEMANA"
  order_levels_day <- c("Lunes", "Martes", "Miercoles", "Jueves",
                         "Viernes", "Sabado", "Domingo")
  # Creacion de la tabla para agregar
  pnts <- round(stats::runif(length(seq_day),
                             n_pnts_range[1], n_pnts_range[2]),0)
  d_wk <- weekdays(seq_day) %>%
    iconv(to="ASCII//TRANSLIT")
  d_wk <- sub("(.)", ("\\U\\1"), d_wk, pe = TRUE)
  look_df <- data.frame(day = seq_day, day_week = d_wk, counts = pnts)
  # Agregacion por dia de la semana
  look_df$day_week <- factor(look_df$day_week,
                             levels = order_levels_day)
  look_df <- look_df[order(look_df$day_week),]
  n_day_week <- look_df %>%
    dplyr::group_by(day_week) %>%
    dplyr::summarise(total = sum(counts))
  # Creacion del vector con fechas repetidas segun cantidad
  r_seq_day <- rep(look_df$day, look_df$count)
  # Ciclo sobre todas las posibilidades
  result <- pnts_dia_semana(n_day_week$total,
                            sp_obj,
                            col_factor = col_factor_day,
                            order_levels = order_levels_day,
                            ...)
  # Adicionar fechas
  result[["FECHA"]] <- r_seq_day
  # Salida
  return(result)
}
### Las siguientes funciones seran presentadas de particular a general
# Primero la simulacion de puntos dentro de un grupo seleccionado de vias
#' @importFrom sf st_as_sf
#' @importFrom sf st_cast
#' @importFrom sf st_sample
pnts_via <- function(n_points, sp_obj, ...){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  # Casteo a LINESTRING
  sp_obj <- sf::st_cast(sf::st_cast(sp_obj,"MULTILINESTRING"),
                        "LINESTRING")
  # Muestreo dentro de unidades
  result <- sf::st_sample(sp_obj, n_points, type = 'random')
  # Retorno
  do.call(rbind, result)
}
# Ahora se crea la funcion para muestrear un rango de velocidades
#' @importFrom sf st_as_sf
#' @importFrom stats rmultinom
pnts_rango_vl <- function(n_points,
                          sp_obj,
                          col_factor,
                          order_levels,
                          probs,
                          ...){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  n_points_vct <- stats::rmultinom(1, n_points, probs)
  # Ciclo sobre todas las posibilidades
  result <- lapply(
    1:length(probs),
    function(x){
      sp_filter <- sp_obj[sp_obj[[col_factor]] == order_levels[x],]
      pnts_via(n_points_vct[x], sp_filter)
    })
  # Uniones
  result <- do.call(rbind, result)
  return(result)
}
# Creacion de rangos de velocidades
#' @importFrom sf st_as_sf
rangos_vl <- function(sp_obj,
                      col_vel,
                      quantiles,
                      n_levels){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  q_vl <- quantile(sp_obj[[col_vel]], quantiles)
  sp_obj$R_VL <- cut(sp_obj[[col_vel]],breaks = q_vl,
          include.lowest = TRUE)
  levels(sp_obj$R_VL) <- n_levels
  return(sp_obj)
}
# Se pasa a la funcion para muestrear un rango de dias
#' @importFrom sf st_as_sf
#' @importFrom stats rmultinom
pnts_rangos_dia <- function(n_points,
                            sp_obj,
                            col_factor,
                            order_levels,
                            probs = rep(1/5,5),
                            ...){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  if(length(probs) != 5)
    stop("El vector de probabilidades debe tener 5 entradas")
  # Cantidad de puntos por categoria
  n_points_vct <- stats::rmultinom(1, n_points, probs)
  # Parametros pasados a funciones internas
  col_vel <- "VEL_MEDIA"
  quantiles_vel <- c(0,0.25,0.5,0.75,1)
  col_factor_vel <- "R_VL"
  order_levels_vel <- c("LENTO", "MOD_LENTO", "MOD_RAPIDO", "RAPIDO")
  probs_vel <- c(0.4,0.3,0.2,0.1)
  # Pasar elipsis a envir
  dots <- list(...)
  list2env(dots, envir = environment())
  # Ciclo sobre todas las posibilidades
  result <- lapply(
    1:length(probs),
    function(x){
      sp_filter <- sp_obj[sp_obj[[col_factor]] == order_levels[x],]
      sp_filter <- rangos_vl(sp_filter,
                             col_vel = col_vel,
                             quantiles = quantiles_vel,
                             n_levels = order_levels_vel)
      sp_filter
      pnts_df <- pnts_rango_vl(n_points_vct[x],
                    sp_filter,
                    col_factor = col_factor_vel,
                    order_levels = order_levels_vel,
                    probs = probs_vel)
      pnts_df <- data.frame(pnts_df,rep(order_levels[x], nrow(pnts_df)))
      names(pnts_df) <- c("LNG", "LAT", col_factor)
      pnts_df
    })
  # Uniones
  result <- dplyr::bind_rows(result)
  # Salida
  return(result)
}
# Se prosigue con la funcion a nivel de dia de la semana
#' @importFrom sf st_as_sf
#' @importFrom dplyr bind_rows
pnts_dia_semana <- function(n_points_vct,
                            sp_obj,
                            col_factor,
                            order_levels,
                            ...){
  if(!class(sp_obj)[1] == "sf") sp_obj <- sf::st_as_sf(sp_obj)
  if(length(n_points_vct) != 7)
    stop("Debe propornciarse una cantidad por cada factor")
  # Parametros pasados a funciones internas
  col_factor_rango <- "HORA"
  order_levels_rango <- c("VALLE_MADRU", "PICO_MANANA", "VALLE_MAN_TAR",
                        "PICO_TAR_NOC", "VALLE_NOCHE")
  probs_rango <- c(0.1,0.25,0.15,0.3,0.2)
  # Pasar elipsis a envir
  dots <- list(...)
  list2env(dots, envir = environment())
  # Ciclo sobre todas las posibilidades
  result <- lapply(
    1:length(order_levels),
    function(x){
      sp_filter <- sp_obj[sp_obj[[col_factor]] == order_levels[x],]
      pnts_df <- pnts_rangos_dia(n_points_vct[x],
                    sp_filter,
                    col_factor = col_factor_rango,
                    order_levels = order_levels_rango,
                    probs = probs_rango,
                    ...)
      pnts_df <- data.frame(pnts_df,rep(order_levels[x], nrow(pnts_df)))
      names(pnts_df)[ncol(pnts_df)] <- col_factor
      pnts_df[sample(1:nrow(pnts_df),size = nrow(pnts_df)),]
    })
  # Uniones
  result <- dplyr::bind_rows(result)
  # Salida
  return(result)
}
