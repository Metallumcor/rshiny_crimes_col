#' @encoding UTF-8
#' @title Simular por kernels normales un delito.
#'
#' @description En esta rutina se simula un delito arbitrario en una ciudad
#'     que cuenta con divisiones administrativas bien definidas, durante un
#'     rango de fechas determinado.
#'
#'     El algoritmo asigna una cantidad aleatoria uniforme de cada delito en
#'     un día determinado, luego, se reparte la cantidad obtenida entre las
#'     divisiones definidas para la rutina en función de los pesos asignados
#'     (deben sumar uno). Dependiendo de lo ingresado por el usuario en
#'     las medias de los kernels normales para cada división (no puede
#'     ser el caso que una división con peso diferente de cero no posea por
#'     lo menos un kernel), se volverá a realizar una repartición entre
#'     cada una de las posibles distribuciones correspondientes (nuevamente,
#'     los pesos de los kernels deberán sumar a uno)
#'
#'     Argumentos opcionales son aceptados para modificar las varianzas y
#'     el rango de delitos simulados por día.
#'     TODO: Ejemplos.
#'
#' @param range_day Día inicial y final, en ese orden. Se aceptan formatos que
#'     puedan ser convertidos a POSIXct, e.g: "2020-01-01" o "01/03/2018".
#' @param df_prop_unidad Un \code{data.frame} que contiene la información de
#'     las divisiones administrativas (la columna debe llevar por nombre
#'     NOMBRE_DIV1) y la proporción de delitos cometidos en cada división
#'     (la columna debe llevar por nombre PROP)
#' @param df_kernels Un \code{data.frame} que contiene la información de
#'     las divisiones administrativas (la columna debe llevar por nombre
#'     NOMBRE_DIV1), las coordenadas de las medias de los kernels
#'     que deben ser dadas en longitud y latitud. Además, las columnas
#'     que contienen dicha información deben se la segunda y tercera, de
#'     forma respectiva.
#' @param ... Otros parámetros adicionales a pasar. De usarse se aceptan
#'     matrices de varianzas simuladas \code{var_list} y rango del conteo
#'     de delitos por día \code{ncount_range}. De igual manera se aceptan
#'     \code{data.frames} de dos columnas donde la primera contenga
#'     las categorías/niveles y la segunda las proporciones (PROP), cada
#'     parámetro de información adicional debe iniciar por el nombre
#'     \code{df_prop}.
#' @return Un \code{data.frame} con los campos
#' \describe{
#'     \item{LAT}{Latitud del evento.}
#'     \item{LNG}{Longitud del evento.}
#'     \item{FECHA}{La fecha "AAAA-MM-DD" del evento.}
#' }
#' @export
#' @importFrom dplyr bind_rows
simular_delito_divisiones <- function(range_day,
                                      df_prop_unidad,
                                      df_kernels,
                                      ...){
  date_range <- as.POSIXct(range_day)
  dates <- seq(date_range[1], date_range[2], by = "day")
  other_args <- list(...)
  if(length(other_args)==0){
    var_list = var_unidad_divisiones(df_kernels)
    ncount_range = c(35,41)
  } else {
    var_list = other_args$var_list
    ncount_range = other_args$ncount_range
  }
  result <- lapply(dates,
                   function(y){
                     simular_dia_division(day = y,
                                       var_list = var_list,
                                       df_prop_unidad,
                                       df_kernels,
                                       ncount_range = ncount_range)
                   })
  dplyr::bind_rows(result)
}

#' @encoding UTF-8
#' @title Simulación de varianza 2x2.
#'
#' @description Función de utilidad usada en \code{simular_delito_divisiones}
#'     para ser pasada como argumento adicional.
#'
#'     TODO: Ejemplos.
#'
#' @param df_kernels Un \code{data.frame} que contiene la información
#'     correspondiente a los kernels, clasificada por division administrativa.
#' @param sd_range El rango entre el cual se obtendran los valores de las
#'     desviaciones estándar.
#'
#' @return Una lista cuyos elementos mínimos son matrices de varianzas 2x2.
#'     La cantidad de elementos viene determinada por la cantidad de unidades
#'     y kernels reportadas en \code{df_kernels}.
#' @export
#' @importFrom stats runif
var_unidad_divisiones <- function(df_kernels, sd_range = c(0.005, 0.01)){
  u_unit <- unique(df_kernels$NOMBRE_DIV1)
  result <- lapply(u_unit,
                   function(x){
                     selection <- df_kernels[df_kernels$NOMBRE_DIV1 == x,]
                     lapply(1:nrow(selection),
                            function(y){
                              sd_components <- stats::runif(2, sd_range[1], sd_range[2])
                              cor_component <- stats::runif(1, -0.99, 0.99)*sd_components[1]*sd_components[2]
                              var_components <- sd_components*sd_components
                              c(var_components[1], cor_component, cor_component,
                                var_components[2])
                            })
                   })
  names(result) <- u_unit
  result
}

# Simulador de unidad
simular_unidad_divisiones <- function(df_kernels, unidad, n, var_list){
  selection <- df_kernels[df_kernels$NOMBRE_DIV1 == unidad,]
  probs <- selection$PROP
  if (is.null(probs)) stop("La columna PROP no existe en df_kernels",
                           call. = FALSE)
  if (sum(probs) != 1){
    probs[1] <- 1-sum(probs)
  }
  components <- sample(1:nrow(selection), prob = probs, size = n,
                       replace = TRUE)
  u_components <- unique(components)
  var_sub_list <- get(unidad, var_list)
  freq_components <- table(factor(components, levels = 1:nrow(selection)))
  # Ciclo sobre localizaciones
  result <- lapply(u_components,
                   function(y){
                     MASS::mvrnorm(n = freq_components[y],
                                   mu = as.numeric(selection[y, 2:3]),
                                   Sigma = matrix(var_sub_list[[y]], 2, 2))
                   })
  # Retornos
  if (inherits(result, "list")){
    do.call(rbind, result)
  } else {
    result
  }
}

# Rutina para simular toda Bogota en un dia
#' @importFrom stats runif
simular_dia_division <- function(day, var_list,
                                 df_prop_unidad,
                                 df_kernels,
                                 ncount_range = c(35,41)){
  if(!inherits(date,"POSIXct")){
    day <- as.POSIXct(day)
  }
  ncounts <- round(stats::runif(1, ncount_range[1], ncount_range[2]),0)
  u_unidad <- df_prop_unidad$NOMBRE_DIV1
  probs <- df_prop_unidad$PROP
  components <- sample(1:length(u_unidad), prob = probs, size = ncounts,
                       replace = TRUE)
  u_components <- unique(components)
  freq_components <- table(factor(components, levels = 1:length(u_unidad)))
  # Ciclo sobre unidades
  result <- lapply(u_components,
                   function(y){
                     simular_unidad_divisiones(
                       df_kernels = df_kernels,
                       unidad = u_unidad[y],
                       n = freq_components[y],
                       var_list = var_list)
                   })
  # Ajuste de retorno a formato
  df_result <- data.frame(do.call(rbind, result))
  df_result <- cbind(df_result, rep(day, nrow(df_result)))
  names(df_result) <- c("LAT", "LNG", "FECHA")
  # Retorno
  df_result
}
