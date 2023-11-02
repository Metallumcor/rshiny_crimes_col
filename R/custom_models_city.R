#' @encoding UTF-8
#' @title Evaluación de modelos creados por el usuario para delitos seleccionados.
#'
#' @description Primera versión de la rutina conjunta para el reporte completo
#'     de modelos personalizados para una ciudad de elección y una lista de
#'     delitos. De momento se debe supervisar que la combinación ciudad+delito
#'     devuelva el nombre de una base de datos existente, para ello se recomienda
#'     consultar \code{data} y \code{data_col_crim}.
#'
#'     En esta versión se evaluán los modelos de \code{lin_mod_selector}
#'     y \code{ar_mod_selector}.
#'
#'     TODO: Permitir que dots admita la especificación de modelo(s). Ejemplos.
#'
#' @param city La ciudad a evaluar
#' @param crimes Un vector con el nombre de los delitos a consultar.
#' @param ... Los parámetros con nombres de los modelos corridos. Los parámetros
#'     admitidos (con su nombre) son \code{p_max}, \code{date_mix},
#'     \code{tunning_intercept} y \code{split_prop}. Consultar las documentaciones
#'     correspondientes para más detalles.
#'
#' @return Una lista con el nombre de cada conjunto de datos analizado que
#'     incluye la salida del mejor modelo. Para más detalles ver la
#'     documentación respectiva de cada algoritmo de selección.
#' @export
custom_models_city <- function(city, crimes, ...){
  tryCatch({get(city)},error = function(e) stop("Ciudad no encontrada",
                                                call. = FALSE))
  p_max <- 6
  dates_col <- NULL
  step_selection <- TRUE
  split_prop <- c(0.9,0.05,0.05)
  factors <- "CODIGO_DIV1"
  dots <- list(...)
  if(length(dots) == 0) warning("Se usaran los argumentos por defecto")
  if(length(dots) != 0) list2env(dots, envir = environment())
  to_delete <- " a | de | en | y "
  all_fits <- list()
  for(crime in crimes){
    to_seek <- gsub(to_delete,"_", tolower(crime))
    to_seek <- iconv(to_seek,to="ASCII//TRANSLIT")
    to_seek <- paste(tolower(substr(city,1,3)), gsub(" ","_",to_seek), sep = "_")
    tryCatch({data_set <- get(to_seek)}, error = function(e) data_set <- NULL)
    if(is.null(data_set)) next
    dt_wrk <- data.table::setDT(data_set)[, .N,by = .(CODIGO_DIV1, FECHA)]
    X <- dt_wrk[,-"N"]
    date_X <- try(as.Date(X$FECHA), silent = TRUE)
    if("try-error" %in% class(date_X)) date_X <- as.Date(paste0(X$FECHA,"-01"))
    X$FECHA <- date_X
    X <- add_date_split(X, "FECHA", day_range = FALSE, week_day = FALSE)
    Y <- dt_wrk$N
    Z <- data.frame(X,Y=Y)
    fct_split <- split_by_factors(Z, factors)
    Y_aug <- unlist(lapply(fct_split, function(z) tail(z$Y,-p_max)))
    X_aug <- lapply(fct_split, function(z){
      Y <- z$Y
      X <- subset(z, select = -Y)
      data_aug <- add_laggs(X = X, Y = Y, p = p_max)
      data_aug})
    X_aug <- dplyr::bind_rows(X_aug)
    X_arima <- X
    X_arima$FECHA <- date_X
    split_aug <- create_split_long(Y = Y_aug, X = X_aug, split_prop = split_prop,
                                  factors = factors)
    split_arima <- create_split_long(Y = Y, X = X_arima,
                                     split_prop = split_prop,
                                     factors = factors)
    fits <- list()
    fits$lin <- lin_mod_selector(split_out = split_aug,
                                   step_selection =  step_selection,
                                   quiet = TRUE)
    fits$arima <- arima_mod_selector(split_out = split_arima,
                                     dates_col = dates_col,
                                     factor_split = factors,
                                     quiet = TRUE)
    rmse <- list()
    rmse$lin <- fits$lin$rmse_val
    rmse$arima <- sqrt(sum(sapply(fits$arima$models, function(x){x$rmse_val})^2))
    all_fits[[to_seek]] <- fits[[which.min(rmse)]]
  }
  if(length(all_fits) == 0) stop("No se encontro base de datos asociada",
                                 call. = FALSE)
  return(all_fits)
}
