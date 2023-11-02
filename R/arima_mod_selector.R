#' @encoding UTF-8
#' @title Ajuste de modelos ARIMA con validación por RMSE.
#'
#' @description Modelos ARIMA automáticos para una serie de tiempo o múltiples
#'     que son identificadas por un conjunto de factores únicos (datos
#'     longitudinales).
#'
#'     No se realizan múltiples pases de validación.
#'
#'     TODO: Ejemplos y múltiples pases de validación.
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#' @param dates_col El nombre de la columna que contiene las fechas.
#' @param factor_split Los nombres de las columnas usadas para identificar
#'     los objetos medidos en el tiempo.
#' @param quiet Callar las salidas en consola de los modelos.
#' @param ... Argumentos adicionales para componete estacional
#'
#' @return Una lista que incluye el mejor modelo ARIMA (para una serie) ó
#'     una lista que incluye los factores que identifican cada serie
#'     \code{factor_list} y una lista de los mejores modelos para cada
#'     serie individual. Las salidas de modelos incluyen los RMSE en
#'     entrenamiento y validación.
#' @export
#' @importFrom utils install.packages
arima_mod_selector <- function(split_out, dates_col = NULL,
                               factor_split = NULL, quiet = FALSE,
                               ...){
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  if(!is_inst("feasts")){
    warning("El paquete feasts es requerido, instalando...")
    install.packages("feasts", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
    if(!is_inst("feasts")) stop("La instalacion fallo, intente instalar feasts manualmente")
  }
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(X_train,X_val)))
  if(!is.null(factor_split)){
    if(sum(!factor_split %in% names(X_train)) != 0) stop("Factores por fuera de X")
    Z_train_split <- split_by_factors(data.frame(y = Y_train, X_train),
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(data.frame(y = Y_val, X_val),
                                    factor_split = factor_split)
    factor_levels <- unique(X_train[,factor_split])
    model_list <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      arima_mod_selector_output(train$y, val$y, dates_mix = dates_col,
                              quiet = quiet,...)
    })
    return(list(factor_levels = factor_levels, models = model_list))
  }
  arima_mod_selector_output(Y_train, Y_val, dates_mix = dates_col, quiet = quiet,
                            ...)
}

# Funcion auxiliar para arima_mod_selector
# date_mix debe estar en formato Date necesariamente.
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom tsibble as_tsibble
#' @importFrom fable ARIMA
#' @importFrom dplyr tibble
#' @importFrom fabletools refit
#' @importFrom fabletools model
arima_mod_selector_body <- function(Y_train, Y_val, dates_mix = NULL,
                                    model = NULL,...){
  # GLOB
  seasons <- FALSE
  # DOTS
  list2env(list(...), envir = environment())
  #
  if(is.null(dates_mix)){
    dates_mix <- as.Date("2013-03-27")+0:(length(Y_train)+length(Y_val)-1)
  }
  Y_tibble <- dplyr::tibble(
    date = dates_mix,
    value = c(Y_train, Y_val)
  )
  Y_tsibble <- tsibble::as_tsibble(Y_tibble)
  fit <- model
  if(is.null(fit)){
    if(!seasons){
      fit <- utils::head(Y_tsibble, length(Y_train)) %>%
        fabletools::model(fable::ARIMA(value~PDQ(0,0,0)))
    } else {
      fit <- utils::head(Y_tsibble, length(Y_train)) %>%
        fabletools::model(fable::ARIMA(value~PDQ(0:2,0:1,0:2)))
    }
  }
  fit_val <- fabletools::refit(fit, utils::tail(Y_tsibble, length(Y_val)),
                                 reestimate = FALSE)

  if(!is.null(model)) return(fit_val)
  return(list(fit = fit, fit_val = fit_val))
}

# Funcion auxiliar para arima_mod_selector
# date_mix debe estar en formato Date necesariamente.
#' @importFrom stats residuals
#' @importFrom stats complete.cases
#' @importFrom fabletools accuracy
#' @importFrom fabletools refit
arima_mod_selector_output <- function(Y_train, Y_val, dates_mix = NULL,
                                      quiet = FALSE, ...){
  arima_fit <- arima_mod_selector_body(Y_train, Y_val, dates_mix,
                                       model = NULL, ...)
  metrics_val <- fabletools::accuracy(arima_fit$fit_val)
  res_train <- stats::residuals(arima_fit$fit)
  res_train <- res_train$.resid[stats::complete.cases(res_train$.resid)]
  rmse_train <- sqrt(mean(res_train^2))
  if(!quiet)
    cat("\nModelo base","\n-----","\nRMSE train: ",rmse_train,
        "\nRMSE val: ",metrics_val$RMSE,"\n")
  return(list(model = arima_fit$fit, rmse_train = rmse_train,
              rmse_val = metrics_val$RMSE))
}
