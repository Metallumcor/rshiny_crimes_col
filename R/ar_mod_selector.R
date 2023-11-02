#' @encoding UTF-8
#' @title Ajuste automático de modelos AR con validación por RMSE.
#'
#' @description Modelos AR automáticos para una serie de tiempo o múltiples
#'     que son identificadas por un conjunto de factores únicos (datos
#'     longitudinales).
#'
#'     No se realizan múltiples pases de validación.
#'
#'     TODO: Ejemplos y múltiples pases de validación.
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#' @param p_max El máximo rezago a usar al explorar modelos.
#' @param dates_col El nombre que tienen la columna de fechas. Opcional.
#' @param factor_split El nombre de las columnas que crean las particiones
#'     por factores. Ideal para múltiples series de tiempo, no necesariamente
#'     en una misma ventana de tiempo.
#' @param quiet Callar las salidas en consola de los modelos.
#'
#' @return Una lista que incluye el mejor modelo y los RMSE en entrenamiento
#'     y validación (un pase)
#' @export
ar_mod_selector <- function(split_out, p_max = 5, dates_col = NULL,
                               factor_split = NULL, quiet = FALSE){
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
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
      ar_mod_selector_body(train$y, val$y, p_max = p_max, dates_mix = dates_col,
                              quiet = quiet)
    })
    return(list(factor_levels = factor_levels, models = model_list))
  }
  ar_mod_selector_body(Y_train, Y_val, dates_mix = dates_col, quiet = quiet)
}

# Funcion auxiliar para arima_mod_selector
# date_mix debe estar en formato Date necesariamente.
#' @importFrom stats residuals
#' @importFrom stats complete.cases
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom tsibble as_tsibble
#' @importFrom fable AR
#' @importFrom dplyr tibble
#' @importFrom fabletools accuracy
#' @importFrom fabletools refit
#' @importFrom fabletools model
#' @importFrom fabletools model_sum
ar_mod_selector_body <- function(Y_train, Y_val, p_max, dates_mix = NULL,
                                 quiet = FALSE){
  if(is.null(dates_mix)){
    dates_mix <- as.Date("2013-03-27")+0:(length(Y_train)+length(Y_val)-1)
  }
  Y_tibble <- dplyr::tibble(
    date = dates_mix,
    value = c(Y_train, Y_val)
  )
  Y_tsibble <- tsibble::as_tsibble(Y_tibble)
  fit <- utils::head(Y_tsibble, length(Y_val)) %>%
    fabletools::model(fable::AR(value~order(0:p_max)))
  fit_val <- fabletools::refit(fit, utils::tail(Y_tsibble, length(Y_val)),
                               reestimate = FALSE)
  metrics_val <- fabletools::accuracy(fit_val)
  res_train <- stats::residuals(fit)
  res_train <- res_train$.resid[stats::complete.cases(res_train$.resid)]
  rmse_train <- sqrt(sum(res_train^2))
  if(!quiet)
    cat("\nModelo base","\n-----","\nRMSE train: ",rmse_train,
        "\nRMSE val: ",metrics_val$RMSE,"\n")
  return(list(model = fit, rmse_train = rmse_train, rmse_val = metrics_val$RMSE))
}
