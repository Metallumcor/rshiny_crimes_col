#' @encoding UTF-8
#' @title Creación de particiones entrenamiento-validación-prueba para
#'     series de tiempo.
#'
#' @description Dado que los paquete \code{caret} y \code{tidymodels} no tiene
#'     una función nativa para crear particiones completas del conjunto de
#'     datos en las tres partes mencionadas, se proporciona en este paquete
#'     una rutina que se ajusta a las necesidades del paquete. Otras
#'     particiones del tipo rolling window pueden consultarse en los paquetes
#'     mencionados.
#'
#'     *Importante:* Se da por hecho que los datos son organizados por una
#'     columna temporal.
#'
#'     Asume la existencia de una matriz diseño con factores, es posible que
#'     algunos niveles no queden representados en los conjuntos de validación
#'     y prueba, por lo cual se ofrece un **remedio** parcial
#'     mediante la opción \code{missing_policy}. Lo anterior solo aplica
#'     al conjunto de validación. Para un manejo adecuado de datos
#'     de tipo longitudinal se debe usar la función \code{create_split_long}
#'
#'     TODO: Corregir el problema de faltantes en versiones futuras sin generar
#'     eliminación de variables. Aumentar el alcance al conjunto de pruebas.
#'
#' @param Y Un vector que contiene las respuestas.
#' @param X Un \code{data.frame}, \code{data.table} o \code{matrix} que guarda
#'     la información de características o predictoras.
#' @param split_prop La proporción (tamaños) de cada conjunto, se ingresan en
#'     el orden entrenamiento/validación/prueba. Debe cuidarse que la suma de
#'     exactamente uno.
#' @param missing_policy La política de valores faltantes en el conjunto
#'     de validación. Puede ser "drop" para eliminar las observaciones completas
#'     o "stop" para detener el proceso si las detecta.
#'
#' @return Una lista que contiene las particiones de Y y X, si la última es
#'     proporcionada.
#' @export
#' @examples
#' # Se carga una fraccion de bog_hurto_motos
#' df_puntos <- bog_hurto_motos %>%
#'     dplyr::filter(CODIGO_DIV1 == "01") %>%
#'     data.table::as.data.table()
#' # Se agrega por fechas
#' df_agregado <- df_puntos[,.N,FECHA]
#' # Definimos Y y X
#' Y <- df_agregado$N
#' X <- df_agregado[,-"N"]
#' # Realizamos la particion
#' split_ts <- create_split_ts(Y = Y, X = X, split_prop = c(0.6,0.2,0.2))
#' # Una vista al conjunto de entrenamiento
#' df_split <- data.frame(Y=split_ts$Y$Y_train,X=split_ts$X$X_train)
#' utils::head(df_split)
create_split_ts <- function(Y, X = NULL,
                            split_prop = c(0.9,0.05,0.05),
                            missing_policy = "drop"){
  if (sum(split_prop) != 1){
    stop("La suma de las proporciones debe ser 1")
  }
  if (!class(Y) %in% c("character", "complex", "factor",
                       "integer", "logical", "numeric")){
    stop("Y debe ser un vector")
  }
  index_split <- cumsum(ceiling(length(Y)*split_prop))
  Y_out <- list(Y_train = Y[1:index_split[1]],
                Y_val = Y[(index_split[1]+1):index_split[2]],
                Y_test = Y[(index_split[2]+1):length(Y)])
  if (!is.null(X)){
    if (!inherits(X,c("data.frame","data.table","matrix"))){
      stop("X debe ser data.frame, data.table o matrix")
    }
    if (nrow(X)!=length(Y)){
      stop(paste("X debe tener la misma cantidad de observaciones que Y",
                 "\n X:",nrow(X),", Y:",length(Y)), call. = FALSE)
    }
    if(inherits(X, "data.table")) X <- as.data.frame(X)
    X_out <- list(X_train = X[1:index_split[1],],
                  X_val = X[(index_split[1]+1):index_split[2],],
                  X_test = X[(index_split[2]+1):length(Y),])
    factor_levels_check <- function(x_1, x_2){
      if(!inherits(x_1, c("factor","character"))) return(NULL)
      check_list <- unique(x_1)
      if(sum(!unique(x_2) %in% check_list) != 0){
        return(which(!x_2 %in% check_list))
      }
    }
    index_missing <- sapply(names(X_out$X_train), function(x){
      factor_levels_check(get(x, X_out$X_train),get(x, X_out$X_val))})
    index_missing <- unlist(Filter(Negate(is.null), index_missing))
    if (length(index_missing) != 0){
      if (missing_policy == "drop"){
        X_out$X_val <- X_out$X_val[-index_missing,]
        Y_out$Y_val <- Y_out$Y_val[-index_missing]
      } else if (missing_policy == "stop"){
        stop("Existen nuevos nivel de los factores en el conjunto de validacion",
             call. = FALSE)
      } else {
        stop("La politica de niveles faltantes elegida no valida, usar: drop o stop",
             call. = FALSE)
      }
    }
    return(list(Y = Y_out, X = X_out))
  }
  return(Y_out)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para modelos de regresión
#'
#' @description Usada para obtener diferentes medidas para evaluación de
#'     rendimiento sobre modelos definidos por el usuario. La medición
#'     se realiza sobre el conjunto de entrenamiento o validación.
#'     Se asume que X existe y es proporcionada.
#'
#'     Por defecto entrega el RMSE.
#'
#'     TODO: Ejemplos.
#'
#' @param model El modelo de regresión ajustado a medir su rendimiento
#' @param split_output La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#' @param ... Los nombres de las medidas adicionales, entre las que se
#'     encuentran:
#'     \describe{
#'         \item{\code{mean}}{media de los residuos.}
#'         \item{\code{mean_observed}}{media de la variable respuesta.}
#'         \item{\code{mae}}{error absoluto medio de los residuos.}
#'         \item{\code{cve_rmse}}{cociente entre \code{rmse} y \code{mean}.}
#'         \item{\code{sd_observed}}{desviación estándar de la variable respuesta.}
#'         \item{\code{rmse_scaled}}{RMSE obtenido con residuales estandarizados por la suma absoluta de las diferencias de las observaciones.}
#'     }
#'
#' @return Una lista de medidas de rendimiento dentro de muestra o fuera,
#'     según la elección del usuario
#' @export
#' @importFrom stats predict
linmod_metrics <- function(model, split_output,  objective = "validation", ...){
  if(objective == "train"){
    Y <- get("Y_train", split_output$Y)
    res <- stats::residuals(model)
  }
  if(objective == "validation"){
    X <- get("X_val", split_output$X)
    Y <- get("Y_val", split_output$Y)
    res <- stats::predict(model, X)-Y
  }
  metrics <- list()
  metrics$rmse <- sqrt(mean(res^2))
  dots <- unlist(list(...))
  if(length(dots) != 0){
    if("mean" %in% dots | "all" %in% dots) metrics$mean <- mean(res)
    if("mean_observed" %in% dots | "all" %in% dots)
      metrics$mean_observed <- mean(Y)
    if("mae" %in% dots | "all" %in% dots) metrics$mae <- mean(abs(res))
    if("cve_rmse" %in% dots | "all" %in% dots)
      metrics$cve_rmse <- metrics$rmse/abs(mean(res))
    if("sd_observerd" %in% dots | "all" %in% dots) metrics$sd_observed <- sd(Y)
    if("rmse_scaled" %in% dots | "all" %in% dots){
      res_scaled <- res/mean(abs(diff(Y)))
      metrics$rmse_scaled <- sqrt(mean(res_scaled^2))
    }
  }
  if(length(metrics) == 1) metrics <- unlist(metrics)
  return(metrics)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento - Raíz del error cuadrático medio
#'
#' @description Usada para obtener la medida de rendimiento sobre modelos
#'     definidos por el usuario. La medición se ejecuta sobre el conjunto
#'     de validación. La rutina asume que X existe y es proporcionada.
#'
#'     TODO: Ejemplos.
#'
#' @param model El modelo de regresión ajustado a medir su rendimiento
#' @param split_output La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#'
#' @return El RMSE fuera de muestra del objeto.
#' @export
#' @importFrom stats predict
rmse_metric <- function(model, split_output){
  .Deprecated("linmod_metrics")
  X_val <- get("X_val", split_output$X)
  Y_val <- get("Y_val", split_output$Y)
  res <- stats::predict(model, X_val)-Y_val
  rmse <- sqrt((res%*%res)/length(res))
  return(rmse)
}

#' @encoding UTF-8
#' @title Pseudo-coeficiente de variación para la predicción
#'
#' @description Usada para obtener la medida de rendimiento sobre modelos
#'     definidos por el usuario. La medición se ejecuta sobre el conjunto
#'     de validación. La rutina asume que X existe y es proporcionada.
#'
#'     TODO: Ejemplos.
#'
#' @param model El modelo de regresión ajustado a medir su rendimiento
#' @param split_output La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#'
#' @return El RMSE fuera de muestra del objeto.
#' @export
#' @importFrom stats predict
cvpred_metric <- function(model, split_output){
  .Deprecated("linmod_metrics")
  X_val <- get("X_val", split_output$X)
  Y_val <- get("Y_val", split_output$Y)
  res <- stats::predict(model, X_val)-Y_val
  rmse <- sqrt((res%*%res)/length(res))
  mean_error <- mean(res)
  return(rmse/mean_error)
}

#' @encoding UTF-8
#' @title Inclusión de columnas con rezago.
#'
#' @description A partir de un vector numérico, crea otros que contienen
#'     la variable rezagada p periodos. Se crean NA por construcción
#'     por lo cual debe tenerse esto en consideración al momento de evaluar
#'     el modelo.
#'
#' @param x Un vector de números.
#' @param k La cantidad de rezagos que se adicionan como columnas. Puede
#'     ser cualquier número entero diferente de cero.
#'
#' @return Un vector del tamaño de $x$ con posibles entradas NA.
#' @export
#' @examples
#' # Simular un AR(1)
#' X <- stats::arima.sim(list(order(p=0.7)),n = 100)
#' # Convertir a númerico
#' X <- as.numeric(X)
#' # Crear el vector con un rezago
#' X_1 <- lagpad(X,1)
#' X_1[1:5]
lagpad <- function(x, k) {
  if (!class(x) %in% c("character", "complex",
                       "integer", "logical", "numeric")){
    stop("x debe ser un vector")
  }
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)])
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)))
  }
}

#' @encoding UTF-8
#' @title Adición de rezagos a la matriz de características/diseño.
#'
#' @description Extensión de \code{lagpad} que ayuda al diseño de
#'     modelos de regresión con términos autoregresivos en X. Dado que
#'     \code{lagpad} devuelve vectores con NA, el procedimiento las elimina
#'     a favor de la integridad del modelo final, no obstante, debe
#'     considerarse esto dentro de los análisis pertinentes.
#'
#' @param X La matriz de características.
#' @param Y El vector de respuestas.
#' @param p El rezago máximo de Y que se adicionará a X.
#'
#' @return Una matriz X aumentada con valores rezagados de Y.
#' @export
#' @importFrom stats complete.cases
#' @examples
#' # Simular un AR(1)
#' Y <- stats::arima.sim(list(order(p=0.7)),n = 100)
#' # Convertir a numerico
#' Y <- as.numeric(Y)
#' # Crear una matriz X de dos columnas
#' X <- data.frame(color = sample(c("Azul","Verde"), 100, replace = TRUE))
#' # Adicion de lags
#' X <- add_laggs(X, Y, p = 1)
add_laggs <- function(X, Y, p = 10){
  if(inherits(X, c("numeric","matrix"))) X <- as.data.frame(X)
  if(!inherits(X, c("data.frame", "data.table"))){
    stop("X debe ser un data.frame, data.table o matrix")
  }
  if(length(Y) < p){
    stop("Cantidad insuficiente de observaciones para los rezagos")
  }
  # Ciclo de creacion de columnas
  lag_cols <- lapply(1:p, function(x)lagpad(Y,x))
  df_lag_cols <- as.data.frame(do.call(cbind, lag_cols))
  names(df_lag_cols) <- paste0("Y",1:p)
  result <- cbind(X,df_lag_cols)
  # Eliminacion de nulos. Solo casos completos para modelo
  return(result[stats::complete.cases(result),])
}

#' @encoding UTF-8
#' @title Separar bases de datos en factores únicos.
#'
#' @description Función pensada para datos de tipo longitudinal, la cual se
#'     destina como uso previo a funciones relacionadas con el tiempo como
#'     \code{add_laggs}. De momento solo puede trabajar con columnas
#'     cuyo tipo sea factor o carácter.
#'
#' @param x La base de datos longitudinales a ser particionada. Debe estar en
#'     formato \code{data.frame}.
#' @param factor_split Las columnas a usar para crear las particiones.
#'
#' @return Una lista de \code{data.frame} que tienen nivele o combinaciones
#'     únicas de los factores seleccionados.
#' @export
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @examples
#' # Crear una secuencia de fechas
#' X_fecha <- seq(as.Date("1995-03-26"), length = 5, by = "1 day")
#' # Crear los factores
#' X_color <- rep(c("Azul", "Verde"), each = 5)
#' # Crear el data.frame y mezclarlo
#' X <- data.frame(FECHA = X_fecha, COLOR = X_color)
#' X <- X[sample(1:10,10),]
#' # Particion por color
#' split_by_factors(X,"COLOR")
split_by_factors <- function(x,factor_split){
  if(!inherits(factor_split,"character")) stop("factor_split debe contener nombres",
                                               call. = FALSE)
  if(sum(!factor_split %in% names(x))!=0) stop("Algunas columnas no existen en x",
                                               call. = FALSE)

  if(inherits(x,c("data.frame","data.table"))){
    split_x <- split(x, x[,factor_split], drop = TRUE)
  } else { # No usado de momento
    stop("Se debe tener un data.frame o data.table")
    look_up <- unique(x[,factor_split])
    split_x<- lapply(split(look_up, 1:nrow(look_up)), function(y){
      y <- c(y)
      cond_list <- split(y,1:length(y))
      names(cond_list) <- factor_split
      cond_expr <- purrr::map(cond_list,~paste0(" == ", "'", .[[1]], "'"))
      str_condition <- paste(names(cond_expr),cond_expr,collapse = "&")
      dplyr::filter(.data = x, eval(parse(text = str_condition)))
    })
  }
  return(split_x)
}

#' @encoding UTF-8
#' @title Creación de particiones entrenamiento-validación-prueba para
#'     datos longitudinales.
#'
#' @description Dado que los paquete \code{caret} y \code{tidymodels} no tiene
#'     una función nativa para crear particiones completas del conjunto de
#'     datos en las tres partes mencionadas, se proporciona en este paquete
#'     una rutina que se ajusta a las necesidades del paquete. Este tipo de
#'     partición amplia el alcance de \code{create_split_ts} al
#'     admitir las mediciones repetidas por unidad (factor unicó o
#'     combinaciones)
#'
#'     *Importante:* Se da por hecho que los datos son organizados por una
#'     columna temporal. Se asume que todos los factores estan presentes en
#'     todas las unidades temporales
#'
#' @param Y Un vector que contiene las respuestas.
#' @param X Un \code{data.frame}, \code{data.table} o \code{matrix} que guarda
#'     la información de características o predictoras.
#' @param factors Los factores que determinan los objetos medidos de forma
#'     repetida en el tiempo.
#' @param split_prop La proporción (tamaños) de cada conjunto que se ingresan en
#'     el orden: entrenamiento/validación/prueba. Debe cuidarse que la suma de
#'     exactamente uno.
#'
#' @return Una lista que contiene las particiones de Y y X, si la última es
#'     proporcionada.
#' @export
#' @examples
#' # Se carga una fraccion de bog_hurto_motos
#' df_puntos <- bog_hurto_motos %>%
#'     dplyr::filter(CODIGO_DIV1 == c("01","02")) %>%
#'     data.table::as.data.table()
#' # Se agrega por fechas y codigo de localidad
#' df_agregado <- df_puntos[,.N,.(FECHA,CODIGO_DIV1)]
#' # Definimos Y y X
#' Y <- df_agregado$N
#' X <- df_agregado[,-"N"]
#' # Realizamos la particion
#' split_long <- create_split_long(Y = Y, X = X, factors = "CODIGO_DIV1",
#'     split_prop = c(0.6,0.2,0.2))
create_split_long <- function(Y, X, factors,
                            split_prop = c(0.9,0.05,0.05)){
  if (sum(split_prop) != 1){
    stop("La suma de las proporciones debe ser 1")
  }
  if (!class(Y) %in% c("character", "complex", "factor",
                       "integer", "logical", "numeric")){
    stop("Y debe ser un vector")
  }
  if (!inherits(X,c("data.frame","data.table","matrix"))){
    stop("X debe ser data.frame, data.table o matrix")
  }
  if (nrow(X)!=length(Y)){
    stop(paste("X debe tener la misma cantidad de observaciones que Y",
               "\n X:",nrow(X),", Y:",length(Y)), call. = FALSE)
  }
  if(inherits(X, "data.table")) X <- as.data.frame(X)
  Z <- data.frame(X, Y = Y)
  X_train <- X_val <- X_test <- list()
  Y_train <- Y_val <- Y_test <- list()
  Z_split <- split_by_factors(Z, factor_split = factors)
  for (e in 1:length(Z_split)){
    Z_temp <- Z_split[[e]]
    index_split <- c(0,ceiling(cumsum(nrow(Z_temp)*split_prop)))
    X_split <- subset(Z_temp, select = -Y)
    Y_split <- Z_temp$Y
    X_train[[e]] <- X_split[(1+index_split[1]):index_split[2],]
    X_val[[e]] <- X_split[(1+index_split[2]):index_split[3],]
    X_test[[e]] <- X_split[(1+index_split[3]):index_split[4],]
    Y_train[[e]] <- Y_split[(1+index_split[1]):index_split[2]]
    Y_val[[e]] <- Y_split[(1+index_split[2]):index_split[3]]
    Y_test[[e]] <- Y_split[(1+index_split[3]):index_split[4]]
  }
  X_out <- list(X_train = dplyr::bind_rows(X_train),
                X_val = dplyr::bind_rows(X_val),
                X_test = dplyr::bind_rows(X_test))
  Y_out <- list(Y_train = unlist(Y_train),
                Y_val = unlist(Y_val),
                Y_test = unlist(Y_test))

  return(list(Y = Y_out, X = X_out))
}

#' @encoding UTF-8
#' @title Procesamiento de fechas para adicionar características
#'
#' @description Acepta un vector de fechas, que pueden ser \code{string},
#'     \code{Date} o \code{POSIXct}, del cual se extraen alguna de las
#'     siguientes componentes: año, mes, semana, día de la semana y/o
#'     rango del día.
#'
#' @param x El vector de fechas en un formato apropiado como "AAAA-MM-DD".
#' @param ... Una serie de pares de clave/valor donde los últimos son
#'     \code{TRUE} (por defecto) o \code{FALSE}. Las claves corresponden con
#'     los campos que salida de la función, en este caso son:
#'     \describe{
#'          \item{\code{year}:}{Devolver el año}
#'          \item{\code{month}:}{Devolver el mes númerico}
#'          \item{\code{week}:}{Devolver el número de la semana}
#'          \item{\code{week_day}:}{Devolver el día de la semana}
#'          \item{\code{day_range}:}{Devolver el rango del día}
#'     }
#'
#' @return Una lista con los nuevos campos.
#' @export
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table wday
#' @importFrom data.table isoweek
#' @examples
#' # Crear una secuencia de fechas
#' X <- seq(as.Date("2020-01-01"), length = 10, by = "1 day")
#' # Particion completa
#' date_split(X)
date_split <- function(x, ...){
  fast <- TRUE
  year <- month <- week <- week_day <- day_range <- TRUE
  dots <- list(...)
  if(length(dots)!=0) list2env(dots, envir = environment())
  if(!inherits(x,c("Date","POSIXct","IDate"))) x <- as.Date(x)
  outlist <- list()
  if(year) outlist[["ANNO"]] <- if(fast) factor(data.table::year(x)) else format(x,'%Y')
  if(month) outlist[["MES"]] <- if(fast) factor(data.table::month(x)) else format(x,'%m')
  if(week) outlist[["SEMANA"]] <- if(fast) factor(data.table::isoweek(x)) else format(x,'%W')
  if(week_day) outlist[["DIA_SEMANA"]] <- if(fast) factor(data.table::wday(x)) else format(x,'%A')
  if(day_range){
    day_look_up <- data.frame(
      hpur = seq(6,24,by = 6),
      range = c("MADRUGADA", "MANNANA", "TARDE", "NOCHE")
    )
    day_hour <- as.numeric(format(x,'%H'))
    outlist[["RANGO_DIA"]] <- day_look_up[day_look_up$hour-6 <= day_hour &
                                            day_look_up$hour > day_hour, 2]
    if(length(outlist[["RANGO_DIA"]])==0){
      warning("Las fechas no poseen hora. Eliminando campo...")
      outlist[["RANGO_DIA"]] <- NULL
    }
  }
  return(outlist)
}

#' @encoding UTF-8
#' @title Adicionar columnas relacionadas con variables de tiempo
#'
#' @description Utilidades de pre-procesamiento para modelos, cuya finalidad es
#'     crear nuevas columnas a partir de información condensada en una
#'     correspondiente a fechas.
#'
#' @param df El \code{data.frame} a ser extendido.
#' @param date_col El nombre de la columna que contiene la información de fechas.
#' @param rm_date Eliminar o no la columna de fechas.
#' @param ... Una serie de pares de clave/valor donde los últimos son
#'     \code{TRUE} (por defecto) o \code{FALSE}. Las claves corresponden con
#'     los campos que salida de la función, en este caso son:
#'     \describe{
#'          \item{\code{year}:}{Devolver el año}
#'          \item{\code{month}:}{Devolver el mes númerico}
#'          \item{\code{week}:}{Devolver el número de la semana}
#'          \item{\code{week_day}:}{Devolver el día de la semana}
#'          \item{\code{year}:}{Devolver el año}
#'     }
#'
#' @return Un \code{data.frame} aumentado con las columnas seleccionadas.
#' @export
#' @importFrom dplyr bind_cols
#' @examples
#' # Creacion de un data.frame con una columna de fechas
#' X <- data.frame(FECHA = seq(as.Date("2020-01-01"), length = 10,
#'          by = "1 day"), COLOR = rep(c("Azul", "Verde"), 5))
#' # Aumento de columnas
#' add_date_split(X,"FECHA")
add_date_split <- function(df, date_col, rm_date = TRUE, ...){
  x <- get(date_col, df)
  if(!inherits(x,c("Date","POSIXct"))) x <- as.POSIXct(x)
     y <- date_split(x,...)
     if(length(y) == 0) stop("Ninguna columna retornada")
     y_bind <- dplyr::bind_cols(y)
     if(rm_date) df[[date_col]] <- NULL
     return(dplyr::bind_cols(df,y_bind))
}

#' @encoding UTF-8
#' @title Adicionar columnas relacionadas con agrupaciones espaciales
#'
#' @description Utilidad de pre-procesamiento para modelos, cuya finalidad es
#'     aumentar el conjunto de datos con columnas de información espacial
#'     la cual es obtenida desde un objeto del tipo \code{sf} o \code{sp} cuyas
#'     geometrías corresponden a polígonos bien definidos. Se debe garantizar
#'     que el objeto a ser aumentado tenga en sus filas la información
#'     correspondiente a puntos espaciales (latitud y longitud) usando nombres
#'     apropiados para dichas columnas y que todo punto este contenido en
#'     alguna de las geometrías del objeto espacial.
#'
#' @param df El \code{data.frame} a ser extendido.
#' @param sf_obj El objeto espacial usado como referencia.
#' @param ... Los nombres de las columnas a preservar de \code{sf_obj}.
#'
#' @return Un \code{data.frame} aumentado con las columnas seleccionadas.
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @examples
#' # Usando como ejemplo Bogota, puntos localizados en centros de localidades
#' # Puente Aranda, Teusaquillo y Kennedy
#' X <- data.frame(lng = c(-74.1066, -74.0888, -74.1533),
#'          lat = c(4.61253, 4.63708, 4.6435))
#' # Incluir el codigo y nombre de las localidades
#' add_spatial_f(X, bogota, "CODIGO_DIV1", "NOMBRE_DIV1")
add_spatial_f <- function(df, sf_obj, ...){
  if(!inherits(df, c("data.frame", "sf", "data.tabLe")))
    stop("Formato no reconocido: df", call. = FALSE)
  if(attr(class(sf_obj),"package") == "sp") sf_obj <- sf::st_as_sf(sf_obj)
  if(!inherits(sf_obj,"sf"))
    stop("sf_obj debe ser una clase sf o sp", call. = FALSE)
  dots <- list(...)
  if(length(dots) == 0) stop("Debe elegirse por lo menos una columna")
  if(sum(!dots %in% names(sf_obj)) != 0) stop("Nombres no encontrados en el objeto")
  df_sf <- df
  if(!inherits(df_sf,"sf")){
    cols <- names(df_sf)
    lat <- grep('^?(lat).*$', cols, ignore.case = TRUE, value = TRUE)
    lng <- grep('^?(l).?(n).?(g).*$', cols, ignore.case = TRUE, value = TRUE)
    df_sf <- sf::st_as_sf(df, coords = c(lng, lat), crs = sf::st_crs(sf_obj))
  }
  sf_obj <- sf_obj[,unlist(dots)]
  inters_point <- sf::st_intersects(df_sf, sf_obj)
  inters_point <- lapply(inters_point, function(x) if(length(x)==0) NA else x)
  df[[".temp"]] <- unlist(inters_point)
  df_result <- sf::st_drop_geometry(sf_obj)
  df_result <- cbind(subset(df,select=-.temp),df_result[df$.temp,])
  return(df_result[complete.cases(df_result),])
}

#' @encoding UTF-8
#' @title Completar series univariadas de intervalos regulares
#'
#' @description Función creada para completar series univariadas donde se
#'     disponga de una grilla regular de observación pero en algunos puntos
#'     las observaciones no estén disponibles. El valor usado para completar
#'     las observaciones perdidas se específica como argumento.
#'
#' @param y El vector de valores observados para la serie.
#' @param dates Los tiempos (fechas) donde fue observada la serie.
#' @param time_delta El tamaño de los saltos entre observaciones. Debe ser
#'     un formato reconocido para fechas (i.e. unidades temporales)
#' @param fill El valor constante con el que completar los datos perdidos.
#'
#' @return La serie \code{y} aumentada para el resto de fechas.
#' @export
#' @examples
#' # Creamos una serie de fechas y se toma una fraccion
#' X_fecha <- seq(as.Date("2020-01-01"), length = 10, by = "1 day")
#' X_fecha <- X_fecha[c(1,sample(2:9,4),10)]
#' Y <- rnorm(6)
#' # Serie llenada con ceros
#' fill_series(Y,X_fecha)
fill_series <- function(y, dates, time_delta = "1 day", fill = 0){
  if(length(y) != length(dates))
    stop("La serie debe ser de la misma longitud que las fechas", call. = F)
  if(!inherits(dates, "Date"))
    dates <- as.Date(dates)
  full_dates <- seq(min(dates), max(dates), by = time_delta)
  y_aug <- data.frame(y = y, dates_c = dates)
  look_dates <- data.frame(dates_c = full_dates)
  result <- merge(look_dates, y_aug, by = "dates_c", all.x = TRUE)
  result[is.na(result)] <- 0
  return(result$y)
}

#' @encoding UTF-8
#' @title Completar datos panel/longitudinales por el índice de tiempo
#'
#' @description Función creada para completar múltiples series de tiempo donde se
#'     dispone de una grilla regular de observación pero que en algunos puntos
#'     las observaciones no estén disponibles. Cada serie de tiempo esta asociada
#'     a un *individuo* que esta determinado por la agrupación de ciertos factores
#'     que se definen en el \code{data.frame} pasado como argumento. Se asume
#'     que todos los individuos son observados en el mismo horizonte de tiempo.
#'
#'     El valor usado para completar las observaciones perdidas se específica
#'     como argumento.
#'
#' @param y El vector de valores observados para las series.
#' @param x El \code{data.frame} de datos que contiene tanto la información de
#'     fechas como de los factores complementarios para distinguir los
#'     individuos.
#' @param date_col El nombre que lleva la columna de fechas.
#' @param time_delta El tamaño de los saltos entre observaciones. Debe ser
#'     un formato reconocido para fechas (i.e. unidades temporales)
#' @param fill El valor constante con el que completar los datos perdidos.
#'
#' @return La serie \code{y} aumentada para el resto de fechas.
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom stats complete.cases
#' @examples
#' # Creamos una serie de fechas y se toma una fraccion
#' X_fecha <- seq(as.Date("2020-01-01"), length = 10, by = "1 day")
#' X_fecha <- X_fecha[c(1,sample(2:9,4),10)]
#' X <- data.frame(FECHA = rep(X_fecha), LETRA = rep(c("A","B"), each = 6))
#' Y <- rnorm(12)
#' # Serie llenada con ceros
#' fill_long(Y, X, "FECHA")
fill_long <- function(y, x, date_col, time_delta = "1 day", fill = 0){
  factors <- names(x)
  if(!date_col %in% factors) stop("La columna de fecha no esta en x")
  Z <- data.frame(y = y, x)
  col_selection <- factors[!factors %in% date_col]
  if(!inherits(Z[,date_col], c("Date")))
    Z[,date_col] <- as.Date(Z[,date_col])
  full_dates <- seq(min(Z[,date_col]), max(Z[,date_col]),
                    by = time_delta)
  look_dates <- data.frame(full_dates)
  names(look_dates) <- date_col
  Z_split <- split_by_factors(Z, factor_split = col_selection)
  result_list <- lapply(1:length(Z_split), function(v){
    result <- merge(look_dates, Z_split[[v]], by = date_col, all.x = TRUE)
    result$y[is.na(result$y)] <- fill
    if(class(result[,col_selection]) == "data.frame")
      result[,col_selection] <- result[stats::complete.cases(result),
                                       col_selection][1,]
    else
      result[,col_selection] <- result[stats::complete.cases(result),
                                       col_selection][1]
    result
  })
  dplyr::bind_rows(result_list)
}

#' @encoding UTF-8
#' @title Resumenes para modelos agrupados
#'
#' @param model_list Lista de modelos. Ver \code{arima_mod_selector}.
#' @param n_model El número del modelo a retornar. Usar 0 para devolver todos.
#'
#' @return Imprime en consola el resumen del modelo ajustado.
#' @export
multi_model_output <- function(model_list, n_model = 0){
  if("models" %in% names(model_list))
    stop("No se puede ubicar la lista de modelos", call. = FALSE)
  models <- model_list$models
  if(n_model != 0){
    if(n_model %% 1 != 0 | abs(n_model) > length(models))
      stop("Valor n_model no reconocido como elemento de la lista", call. = FALSE)
    cat(fabletools::tidy(models[[n_model]]$model))
    return(NULL)
  }
  lapply(1:length(models),function(x){
    cat(fabletools::tidy(models[[x]]$model))})
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para modelos ARIMA
#'
#' @description Usada para obtener diferentes medidas para evaluación de
#'     rendimiento sobre modelos definidos por el usuario. La medición
#'     se realiza sobre el conjunto de validación.
#'
#'     Por defecto entrega el RMSE.
#'
#'     TODO: Ejemplos.
#'
#' @importFrom stats predict
#' @param model_list Lista de modelos o modelo único al que se requiere
#'     extrar las medidas de rendimiento un paso adelante
#' @param split_out La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param dates_col El nombre que lleva la columna de fechas
#' @param factor_split El nombre de las columnas usado para realizar las
#'     particiones y asignación de los modelos
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#'
#' @return Salidas de rendimiento para los modelos ARIMA suministrados
#' @export
#' @importFrom stats sd
arima_metrics <- function(model_list, split_out, dates_col = NULL,
                          factor_split = NULL, objective = "validation"){
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  if(length(model_list)==1 | !any("models" %in% names(model_list))){
    models <- model_list
  } else {
    models <- model_list$models
  }
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(X_train,X_val)))
  if(!is.null(factor_split)){
    if(sum(!factor_split %in% names(X_train)) != 0)
      stop("Factores por fuera de X")
    Z_train_split <- split_by_factors(data.frame(y = Y_train, X_train),
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(data.frame(y = Y_val, X_val),
                                    factor_split = factor_split)
    metrics <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      if(!any("model" %in% names(models[[v]])))
        fit <- models[[v]]
      else
        fit <- models[[v]]$model
      observed <- train$y
      if(objective == "validation"){
        if(!any("model" %in% names(models[[v]])))
          model <- models[[v]]
        else
          model <- models[[v]]$model
        fit <- arima_mod_selector_body(train$y, val$y,
                                       dates_mix = dates_col,
                                       model = model)
        observed <- val$y
      }
      output <- fabletools::accuracy(fit)
      output[,c(1,2)] <- NULL
      output$MEAN_Y <- mean(observed)
      output$SD_Y <- stats::sd(observed)
      output
    })
    return(dplyr::bind_rows(metrics))
  }
  fit <- models
  observed <- Y_train
  if(objective == "validation"){
    fit <- arima_mod_selector_body(Y_train, Y_val,
                                   dates_mix = dates_col,
                                   model = models)
    observed <- Y_val
  }
  output <- fabletools::accuracy(fit)
  output[,c(1,2)] <- NULL
  output$MEAN_Y <- mean(observed)
  output$SD_Y <- stats::sd(observed)
  return(output)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para modelos ARIMAX
#'
#' @description Usada para obtener diferentes medidas para evaluación de
#'     rendimiento sobre modelos definidos por el usuario. La medición
#'     se realiza sobre el conjunto de validación.
#'
#'     Por defecto entrega el RMSE.
#'
#'     TODO: Ejemplos.
#'
#' @param model_list Lista de modelos o modelo único al que se requiere
#'     extrar las medidas de rendimiento un paso adelante
#' @param split_out La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param xreg_cols El nombre de las columnas usadas como regresoras exóngenas
#' @param fourier_K El grado del suavizado de fourier usado. NULL para omitir
#' @param dates_col El nombre que lleva la columna de fechas
#' @param factor_split El nombre de las columnas usado para realizar las
#'     particiones y asignación de los modelos
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#'
#' @return Salidas de rendimiento para los modelos ARIMAX suministrados
#' @export
#' @importFrom stats sd
#' @importFrom stats predict
arimax_metrics <- function(model_list, split_out, xreg_cols,
                           fourier_K = NULL,
                           dates_col = NULL,
                           factor_split = NULL,
                           objective = "validation"){
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  if(length(model_list)==1){
    models <- model_list
  } else {
    models <- model_list$models
  }
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(X_train,X_val)))
  if(!is.null(factor_split)){
    if(sum(!factor_split %in% names(X_train)) != 0)
      stop("Factores por fuera de X")
    Z_train_split <- split_by_factors(data.frame(value = Y_train, X_train),
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(data.frame(value = Y_val, X_val),
                                    factor_split = factor_split)
    metrics <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      fit <- models[[v]]$model
      observed <- train$value
      if(objective == "validation"){
        fit <- arimax_mod_selector_body(train, val, xreg_cols,
                                        fourier_K,
                                        dates_mix = dates_col,
                                        model = models[[v]]$model)
        observed <- val$value
      }
      output <- fabletools::accuracy(fit)
      output[,c(1,2)] <- NULL
      output$MEAN_Y <- mean(observed)
      output$SD_Y <- stats::sd(observed)
      output
    })
    return(dplyr::bind_rows(metrics))
  }
  Z_train <- data.frame(value = Y_train, X_train)
  Z_val <- data.frame(value = Y_val, X_val)
  fit <- models
  observed <- Y_train
  if(objective == "validation"){
    fit <- arimax_mod_selector_body(Z_train, Z_val, xreg_cols,
                                    fourier_K,
                                    dates_mix = dates_col,
                                    model = models)
    observed <- Y_val
  }
  output <- fabletools::accuracy(fit)
  output[,c(1,2)] <- NULL
  output$MEAN_Y <- mean(observed)
  output$SD_Y <- stats::sd(observed)
  return(output)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para el modelo experimental de dos etapas
#'     ARIMA+Modelo lineal
#' @name arima_lin
#' @rdname arima_lin
#'
#' @description Usada para obtener diferentes medidas para evaluación de
#'     rendimiento sobre modelos definidos por el usuario. La medición
#'     se realiza sobre el conjunto de entrenamiento o validación.
#'     Se asume que X existe y es proporcionada. Además, se requiere el
#'     ingreso de un conjunto de datos con información de residuales
#'     (no necesario para el conjunto de pruebas)
#'
#'     Por defecto entrega el RMSE.
#'
#'     TODO: Ejemplos.
#'
#' @param arima_fit Una lista de modelos ARIMA (residuales u observaciones)
#' @param lin_fit El modelo de regresión (residuales u observaciones)
#' @param split_y La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param dates_col El nombre de la columna que contiene la fechas
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#' @param ... Los nombres de las medidas adicionales, entre las que se
#'     encuentran:
#'     \describe{
#'         \item{\code{all}}{todas las metricas mencionadas enseguida.}
#'         \item{\code{mean}}{media de los residuos.}
#'         \item{\code{mean_observed}}{media de la variable respuesta.}
#'         \item{\code{mae}}{error absoluto medio de los residuos.}
#'         \item{\code{cve_rmse}}{cociente entre \code{rmse} y \code{mean}.}
#'         \item{\code{sd_observed}}{desviación estándar de la variable respuesta.}
#'         \item{\code{rmse_scaled}}{RMSE obtenido con residuales estandarizados por la suma absoluta de las diferencias de las observaciones.}
#'     }
#' @return Una lista de medidas de rendimiento dentro de muestra o fuera,
#'     según la elección del usuario
#' @export
#' @importFrom stats fitted
#' @importFrom stats predict
arima_lin_metrics <- function(arima_fit, lin_fit,
                              split_y, dates_col,
                              objective = "validation",
                              ...){
  if(objective == "train"){
    if(is.list(arima_fit))
      pred_y <- unlist(lapply(arima_fit, function(x)stats::fitted(x$fit)$.fitted))
    else
      pred_y <- stats::fitted(arima_fit$fit)$.fitted
    pred_res <- stats::fitted(lin_fit$selected_model)
    observed <- split_y$Y$Y_train
  }
  if(objective == "validation"){
    split_y$X$X_val[[dates_col]] <- NULL
    if(is.list(arima_fit))
      pred_y <- unlist(lapply(arima_fit, function(x)stats::fitted(x$fit_val)$.fitted))
    else
      pred_y <- stats::fitted(arima_fit$fit_val)$.fitted
    pred_res <- stats::predict(lin_fit$selected_model,split_y$X$X_val)
    observed <- split_y$Y$Y_val
  }
  ds_mestrics(pred_y, pred_res, observed, ...)
}

#' @rdname arima_lin
#' @export
lin_arima_metrics <- function(arima_fit, lin_fit,
                              split_y, dates_col,
                              objective = "validation",
                              ...){
  if(objective == "train"){
    if(is.list(arima_fit[[1]]))
      pred_res <- unlist(lapply(arima_fit, function(x)stats::fitted(x$fit)$.fitted))
    else
      pred_res <- stats::fitted(arima_fit$fit)$.fitted
    pred_y <- stats::fitted(lin_fit$selected_model)
    observed <- split_y$Y$Y_train
  }
  if(objective == "validation"){
    split_y$X$X_val[[dates_col]] <- NULL
    if(is.list(arima_fit[[1]]))
      pred_res <- unlist(lapply(arima_fit, function(x)stats::fitted(x$fit_val)$.fitted))
    else
      pred_res <- stats::fitted(arima_fit$fit_val)$.fitted
    pred_y <- stats::predict(lin_fit$selected_model,split_y$X$X_val)
    observed <- split_y$Y$Y_val
  }
  ds_mestrics(pred_y, pred_res, observed, ...)
}

# Cuerpo para los modelos combinados
ds_mestrics <- function(pred_y, pred_res, observed, ...){
  res <- pred_y + pred_res - observed
  metrics <- list()
  metrics$rmse <- sqrt(mean(res^2))
  dots <- unlist(list(...))
  if(length(dots) != 0){
    if("mean" %in% dots | "all" %in% dots) metrics$mean <- mean(res)
    if("mean_observed" %in% dots | "all" %in% dots)
      metrics$mean_observed <- mean(observed)
    if("mae" %in% dots | "all" %in% dots) metrics$mae <- mean(abs(res))
    if("cve_rmse" %in% dots | "all" %in% dots)
      metrics$cve_rmse <- metrics$rmse/abs(mean(res))
    if("sd_observed" %in% dots | "all" %in% dots)
      metrics$sd_observed <- sd(observed)
    if("rmse_scaled" %in% dots | "all" %in% dots){
      res_scaled <- res/mean(abs(diff(observed)))
      metrics$rmse_scaled <- sqrt(mean(res_scaled^2))
    }
  }
  if(length(metrics) == 1) metrics <- unlist(metrics)
  return(metrics)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para modelos de clasificación
#'
#' @description Para modelos de clasificación binaría o múltiple se procesan
#'     algunas de las principales métricas obtenidas en entrenamiento y
#'     validación. Las matrices de \code{split_out} deben tener el formato
#'     requerido por el modelo (escalado, solo numéricas, etc) que se
#'     solicitaron durante el ajuste del modelo.
#'
#'     TODO: Ejemplos
#'
#' @param fit El modelos de clasificación ajustado
#' @param split_out La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#' @param metrics_out Vector de caracteres. Incluye los nombres de las
#'     métricas a incluir en la salida
#'     \describe{
#'         \item{all}{Todas las métricas de clasificación.}
#'         \item{accuracy}{La **accuracy** general.}
#'         \item{precision}{La presición binaría o combinada (multi-class)}
#'         \item{micro_precision}{La precisión binaría para clases individuales.
#'         Retorna un vector.}
#'         \item{recall}{La exhaustividad binaría o combinada (multi-class)}
#'         \item{micro_recall}{La exhaustividad binaría para clases individuales.
#'         Retorna un vector.}
#'         \item{macro_f1}{La medida F1 macro obtenida con las medidas macro de
#'         precisión y exhaustividad.}
#'         \item{micro_f1}{La medida F1 micro obtenida de las medidas micro de
#'         precisión y exhaustividad. Retorna un vector.}
#'         \item{kappa}{El coeficiente Kappa de Cohen que mide el grado de concordancía.}
#'         \item{adjusted_rand_index}{Medida de similaridad ajustada por chance.
#'         Puede dar valores negativos.}
#'         \item{confusion_matrix}{La matriz de confusión. En filas se tiene el valor predecido, en columnas los observados}
#'     }
#'
#' @return Una lista con las métricas seleccionadas o solo el número
#'     correspondiente a la medida única elegida.
#' @export
clsf_metrics <- function(fit, split_out,
                         objective = "validation",
                         metrics_out = "all"){
  if(!objective %in% c("validation","train"))
    stop("Objetivo no reconocido")
  pred_y <- NULL
  if(objective == "validation"){
    observed <- split_out$Y$Y_val
    if("multinom" %in% class(fit)){
      pred_y <- predict(fit, split_out$X$X_val)
    }
    if("svm" %in% class(fit) | "rpart" %in% class(fit)
       | "naiveBayes" %in% class(fit) | "randomForest" %in% class(fit) |
       "nnet" %in% class(fit)){
      pred_y <- predict(fit, split_out$X$X_val, type = "class")
    }
    if(sum(grepl("?(keras)", class(fit))) != 0){
      split_keras <- split_nn(split_out)
      pred_y <- predict(fit, split_keras$X$X_val)
      pred_y <- levels(observed)[apply(pred_y,1,which.max)]
    }
  }
  if(objective == "train"){
    observed <- split_out$Y$Y_train
    if("multinom" %in% class(fit) | "nnet" %in% class(fit)){
      pred_y <- levels(observed)[apply(fit$fitted.values,1,which.max)]
    }
    if("svm" %in% class(fit)| "rpart" %in% class(fit)){
      pred_y <- predict(fit, type = "class")
    }
    if("naiveBayes" %in% class(fit)| "randomForest" %in% class(fit)){
      pred_y <- predict(fit, split_out$X$X_train, type = "class")
    }
    if((sum(grepl("?(keras)", class(fit))) != 0)){
      split_keras <- split_nn(split_out)
      pred_y <- predict(fit, split_keras$X$X_train)
      pred_y <- levels(observed)[apply(pred_y,1,which.max)]
    }
  }
  if(is.null(pred_y)) stop("Modelo no reconocido")
  pred_y <- factor(pred_y, levels = levels(observed))
  c_mat <- table(observed, pred_y)
  metrics <- list()
  if("all" %in% metrics_out | "accuracy" %in% metrics_out){
    metrics$accuracy <- sum(pred_y == observed)/length(observed)
  }
  if("all" %in% metrics_out | "recall" %in% metrics_out){
    metrics$recall <- (diag(c_mat)%*%(1/rowSums(c_mat)))/nlevels(observed)
  }
  if("all" %in% metrics_out | "micro_recall" %in% metrics_out){
    metrics$micro_recall <- diag(c_mat)/rowSums(c_mat)
  }
  if("all" %in% metrics_out | "precision" %in% metrics_out){
    metrics$precision <- (diag(c_mat)%*%(1/colSums(c_mat)))/nlevels(observed)
  }
  if("all" %in% metrics_out | "micro_precision" %in% metrics_out){
    metrics$micro_precision <- diag(c_mat)/colSums(c_mat)
  }
  if("all" %in% metrics_out | "macro_f1" %in% metrics_out){
    if("recall" %in% names(metrics) & "precision" %in% names(metrics)){
      prec <- metrics$precision
      rec <- metrics$recall
    } else {
      rec <- (diag(c_mat)%*%(1/rowSums(c_mat)))/nlevels(observed)
      prec <- (diag(c_mat)%*%(1/colSums(c_mat)))/nlevels(observed)
    }
    metrics$f1_macro <- 2*(rec*prec)/((1/prec)+(1/rec))
  }
  if("all" %in% metrics_out | "micro_f1" %in% metrics_out){
    if("micro_recall" %in% names(metrics) & "micro_precision" %in% names(metrics)){
      prec <- metrics$micro_precision
      rec <- metrics$micro_recall
    } else {
      rec <- diag(c_mat)/rowSums(c_mat)
      prec <- diag(c_mat)/colSums(c_mat)
    }
    metrics$f1_micro <- 2*(rec*prec)/((1/prec)+(1/rec))
  }
  if("all" %in% metrics_out | "kappa" %in% metrics_out){
    pxt <- colSums(c_mat)%*%rowSums(c_mat)
    metrics$kappa <- (sum(diag(c_mat))*sum(c_mat)-pxt)/((sum(c_mat)^2)-pxt)
  }
  if("all" %in% metrics_out | "diff_class1" %in% metrics_out){
    upper_diag <- 0
    lower_diag <- 0
    for(i in 1:(nrow(c_mat)-1)){
      upper_diag <- upper_diag + c_mat[i,i+1]
      lower_diag <- lower_diag + c_mat[i+1,i]
    }
    metrics$diff_class1 <- (sum(diag(c_mat))+upper_diag+lower_diag)/sum(c_mat)
  }
  if("all" %in% metrics_out | "adjusted_rand_index" %in% metrics_out){
    a <- sum(choose(c_mat, 2))
    b <- sum(choose(rowSums(c_mat), 2))
    c <- sum(choose(colSums(c_mat), 2))
    d <- choose(sum(c_mat), 2)
    metrics$ARI <- (a - (b * c)/d) /((b + c)/2 - b*c/d)
  }
  if("all" %in% metrics_out | "confusion_matrix" %in% metrics_out){
    metrics$confusion_matrix <- c_mat
  }
  if(length(metrics) == 1) metrics <- unlist(metrics)
  return(metrics)
}

#' @encoding UTF-8
#' @title Particiones de datos para redes neuronales - Keras
#'
#' @param split Un objeto retornado por \code{create_split_long} o
#'     \code{create_split_ts}
#' @param ... Argumentos adicionales. \code{expand_y} controla si la
#'     respuesta (categoríca) debe ser expandida en dummies o no, por
#'     defecto se aplica
#' @export
split_nn <- function(split, ...){
  # Globales
  expand_y <- TRUE
  list2env(list(...),envir = environment())
  # Functiones
  f_x <- function(x) model.matrix(~.-1, data = x)
  f_y_cat <- function(y)  keras::to_categorical(as.numeric(y) - 1)
  fill_cols <- function(x,y){
    abs_x <- colnames(x)
    abs_y <- colnames(y)
    to_fill <- abs_x[!abs_x %in% abs_y]
    y_copy <- y
    if(length(to_fill)>0){
      c_frame <- do.call(cbind,lapply(to_fill,function(x)rep(0,nrow(y))))
      colnames(c_frame) <- to_fill
      y_copy <- cbind(y, c_frame)
      y_copy <- y_copy[,abs_x]
      attr(y_copy, "dim") <- dim(y_copy)
      attr(y_copy, "dimnames")[[1]] <-  attr(y, "dimnames")[[1]]
      attr(y_copy, "dimnames")[[2]] <- attr(x, "dimnames")[[2]]
      attr(y_copy, "assign") <- attr(x, "assign")
      attr(y_copy, "contrasts") <- attr(x, "contrasts")
    }
    y_copy
  }
  # Pase de X
  split$X <- lapply(split$X,f_x)
  split$X <- lapply(split$X,function(z)fill_cols(split$X$X_train,z))
  # Pase de Y
  if("character" %in% class(split$Y$Y_train) | "factor" %in% class(split$Y$Y_train)){
    if(expand_y)
      split$Y <- lapply(split$Y,f_y_cat)
    else
      split$Y <- lapply(split$Y,function(x) as.numeric(x)-1)
  }
  # Retorno
  return(split)
}

#' @encoding UTF-8
#' @title Medidas de rendimiento para modelos de regresión
#'
#' @param fit El modelos de clasificación ajustado
#' @param split_out La partición del conjunto de datos en
#'     entrenamiento/validación/prueba
#' @param objective Seleccionar entrenamiento "train" o validación "validation"
#' @param metrics_out Vector de caracteres. Incluye los nombres de las
#'     métricas a incluir en la salida
#'     \describe{
#'         \item{rmse}{Root mean squared error.}
#'     }
#' @param ... Argumentos adicionales
#'
#' @return Una lista con las métricas seleccionadas o solo el número
#'     correspondiente a la medida única elegida.
#' @export
reg_metrics <- function(fit, split_out,
                       objective = c("validation","train"),
                       metrics_out = "all",
                       ...){
  objective <- match.arg(objective)
  pred_y <- NULL
  dates_col <- NULL
  factor_split <- NULL
  list2env(list(...),envir = environment())
  if(objective == "validation"){
    observed <- split_out$Y$Y_val
    if(sum(grepl("?(keras)", class(fit))) != 0){
      split_keras <- split_nn(split_out)
      pred_y <- predict(fit, split_keras$X$X_val)
    }
    if(class(fit)[1] == "randomForest")
      pred_y <- predict(fit, split_out$X$X_val)
    if(class(fit)[1] == "lm")
      rmse <- linmod_metrics(fit,split_out,objective = "validation")
    if(class(fit)[1] == "list"){
      if(all(c("arima_fit","lin_fit") %in% names(fit)))
        rmse <- lin_arima_metrics(fit$arima_fit,fit$lin_fit,split_out,dates_col,"validation")
      else{
        rmse <- arima_metrics(fit, split_out, dates_col, factor_split,
                              objective = "validation")
        rmse <- sqrt(mean((rmse$RMSE)^2))
      }
    }
  }
  if(objective == "train"){
    observed <- split_out$Y$Y_train
    if((sum(grepl("?(keras)", class(fit))) != 0)){
      split_keras <- split_nn(split_out)
      pred_y <- predict(fit, split_keras$X$X_train)
    }
    if(class(fit)[1] == "randomForest")
      pred_y <- predict(fit, split_out$X$X_train)
    if(class(fit)[1] == "lm")
      rmse <- linmod_metrics(fit,split_out,objective = "train")
    if(class(fit)[1] == "list"){
      if(all(c("arima_fit","lin_fit") %in% names(fit)))
        rmse <- lin_arima_metrics(fit$arima_fit,fit$lin_fit,split_out,dates_col,"train")
      else{
        rmse <- arima_metrics(fit, split_out, dates_col, factor_split,
                              objective = "train")
        rmse <- sqrt(mean((rmse$RMSE)^2))
      }
    }
  }
  metrics <- list()
  if("all" %in% metrics_out | "rmse" %in% metrics_out){
    if(exists("rmse"))
      metrics$rmse <- rmse
    else
      metrics$rmse <- sqrt(mean((pred_y-observed)^2))
  }
  if(is.null(metrics)) stop("Modelo no reconocido")
  if(length(metrics) == 1) metrics <- unlist(metrics)
  return(metrics)
}

#' @encoding UTF-8
#' @title Asistente para validación cruzada para series de tiempo
#'
#' @description Siguiendo a [Hyndman y Athanasopoulos (2013)](https://www.otexts.org/fpp/2/5)
#'     con esta función se crea proporciona una técnica de partición del conjunto
#'     de datos en bloques contiguos para la realización de validación cruzada
#'     en series de tiempo y datos longitudinales/panel.
#'
#'     NOTA: El tamaño de entrenamiento y del horizonte de predicción pueden
#'     variar si las fechas están repetidas en el conjunto de datos. Esto se
#'     realiza para poder trabajar con datos longitudinales/panel.
#'
#'     Se recomienda en versiones futuras reducir el tamaño de los objetos guardados
#'     por esta función.
#'
#' @param Y El vector de respuesta. Actualmente no usado, pero será implementado
#'     en el algoritmo para clasificación (integridad de clases)
#' @param X La matriz de características. Debe contener una columna con fechas
#' @param dates_col El nombre de la columna de fechas en \code{X}
#' @param size Entero. La cantidad de periodos usados para entrenamiento
#' @param horizon Entero. La cantidad de periodos usados como horizonte de
#'     predicción
#' @param fixed_window Lógico. Usar o no una ventana de tamaño \code{size} para
#'     entrenamiento, en caso de no usarse se tomarán todas las observaciones
#'     disponibles
#' @param ... Argumentos adicionales. Control del delta de tiempo \code{time_delta}
#'
#' @return Una lista de indices \code{train} y \code{val} para usar en algoritmos
#'     de validación cruzada
#' @export
#'
#' @examples
#' redux <- siedco_full[Fecha < "2015-01-01",]
#' cv_op <- rolling_cv(redux$`Numero Hechos`, redux[,-"Numero Hechos"], "Fecha",1510, 300, TRUE)
rolling_cv <- function(Y, X, dates_col, size, horizon,
                      fixed_window = FALSE, ...){
  if (!class(Y) %in% c("character", "complex", "factor",
                       "integer", "logical", "numeric")){
    stop("Y debe ser un vector")
  }
  if (!inherits(X,c("data.frame","data.table","matrix"))){
    stop("X debe ser data.frame, data.table o matrix")
  }
  if (nrow(X)!=length(Y)){
    stop(paste("X debe tener la misma cantidad de observaciones que Y",
               "\n X:",nrow(X),", Y:",length(Y)), call. = FALSE)
  }
  if(inherits(X, "data.table")) X <- as.data.frame(X)
  # glob var
  time_delta <- "1 day"
  list2env(list(...),envir = environment())
  # Sort por fecha y factores
  Z <- data.frame(X, Y = Y) %>% mutate(.ids = 1:nrow(.))
  # Ventana de tiempo
  if(any(class(Z[[dates_col]]) %in% "Date")) Z[[dates_col]] <- as.Date(Z[[dates_col]])
  obs_dates <- seq(min(Z[[dates_col]]), max(Z[[dates_col]]),by=time_delta)
  n_periods <- length(obs_dates)
  if(n_periods-horizon<size) stop("Incorrect size specification")
  splits_train <- lapply(size:(n_periods-horizon),
                         function(x){
                           if(fixed_window) obs_dates[(x-size+1):x]
                           else obs_dates[1:x]})
  splits_val <- lapply(size:(n_periods-horizon),
                       function(x) obs_dates[(x+1):(x+horizon)])
  # Obtener
  ind_train <- lapply(splits_train,
                      function(x)
                        Z %>%
                        dplyr::filter(!!as.name(dates_col) %in% x) %>%
                        dplyr::select(.ids) %>%
                        unlist() %>%
                        unname())
  ind_val <- lapply(splits_val,
                    function(x)
                      Z %>%
                      dplyr::filter(!!as.name(dates_col) %in% x) %>%
                      dplyr::select(.ids) %>%
                      unlist() %>%
                      unname())
  # Output
  return(list(train = ind_train, val = ind_val))
}

#' @encoding UTF-8
#' @title Escalamiento de variables
#'
#' @description Ideal para modelos dependientes de el escalado de datos.
#'     Solamente en las características. No aplica para pruebas
#'
#' @param split_out Lista de partición en entrenamiento, validación y prueba
#'
#' @return Lista de partición con características númericas normalizadas
#' @export
#'
#' @examples
#' # Definimos Y y X
#' Y <- iris["Species"]
#' X <- subset(iris, select = -Species)
#' # Realizamos la particion
#' test_split <- list()
#' n_train <- 80
#' test_split$X$X_train <- X[1:80,]
#' test_split$X$X_val <- X[-(1:80),]
#' test_split$Y$Y_train <- Y[1:80,]
#' test_split$Y$Y_val <- Y[-(1:80),]
#' # Normalizacion
#' test_split <- scale_split(test_split)
scale_split <- function(split_out){
  num_cols <- unlist(lapply(split_out$X$X_train,is.numeric))
  if(length(num_cols)==0) next
  sc_num <- lapply(split_out$X$X_train[num_cols], scale)
  sc_center <- lapply(sc_num, function(x) attr(x, "scaled:center"))
  sc_scale <- lapply(sc_num, function(x) attr(x, "scaled:scale"))
  split_out$X$X_train[num_cols] <- lapply(sc_num, as.numeric)
  split_out$X$X_val[num_cols] <- Map(function(x,y,z){(x-y)/z},
                                     x = split_out$X$X_val[num_cols],
                                     y = sc_center,
                                     z = sc_scale)
  split_out
}

# Reduccion de randomForest
redx_rf <- function(cm) {
  cm$oob.times <- NULL
  cm$y <- NULL
  cm$votes <- NULL
  cm$predicted <- NULL
  cm$err.rate <- NULL
  cm$test <- NULL
  cm$proximity <- NULL
  cm$confusion <- NULL
  cm$localImportance <- NULL
  cm$importanceSD <- NULL

  attr(cm$terms,".Environment") <- c()
  attr(cm$formula,".Environment") <- c()

  cm
}
