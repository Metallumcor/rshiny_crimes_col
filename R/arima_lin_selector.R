#' @encoding UTF-8
#' @title Ajuste de modelo experimental en dos etapas ARIMA + lineal
#'     con validación por RMSE.
#'
#' @description Este es un algoritmo experimental que corre en primer lugar
#'     los modelos ARIMA automáticos (\code{arima_mod_selector})
#'     para el conjunto de datos solicitado
#'     y a los cuales se les extrae sus residuales para ajustar un modelo
#'     de regresión lineal (\code{lin_mod_selector}). La variable obtenida
#'     es una combinación lineal del pronosticó ARIMA más el residual
#'     estimado.
#'
#'     No se realizan múltiples pases de validación.
#'
#'     TODO: Ejemplos y múltiples pases de validación.
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#' @param factor_split Los nombres de las columnas usadas para identificar
#'     los objetos medidos en el tiempo.
#' @param dates_col El nombre de la columna que contiene las fechas.
#' @param step_selection ¿Se debe probar subconjuntos de variables para el
#'     modelo lineal de los residuales?
#' @param quiet Callar las salidas en consola de los modelos.
#'
#' @return Una lista que incluye los modelos ARIMA elegidos (\code{arima_fit}),
#'     el modelo lineal de los residuales (\code{lin_fit}), y los
#'     RMSE conjuntos obtenidos en entrenamiento y validación
#'
#' @export
#' @importFrom utils install.packages
arima_lin_selector <- function(split_out,
                               factor_split = NULL,
                               dates_col = NULL,
                               step_selection = TRUE,
                               quiet = FALSE){
  if(!is_inst("feasts")){
    warning("El paquete feasts es requerido, instalando...")
    install.packages("feasts", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
    if(!is_inst("feasts")) stop("La instalacion fallo, intente instalar feasts manualmente")
  }
  # Obtener el ajuste ARIMA
  Z_train <- data.frame(y = split_out$Y$Y_train,split_out$X$X_train)
  Z_val <- data.frame(y = split_out$Y$Y_val,split_out$X$X_val)
  dates_name <- dates_col
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(Z_train,Z_val)))
  if(!is.null(factor_split)){
    if(sum(!factor_split %in% names(Z_train)) != 0) stop("Factores por fuera de X")
    Z_train_split <- split_by_factors(Z_train,
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(Z_val,
                                    factor_split = factor_split)
    factor_levels <- unique(Z_train[,factor_split])
    fit_arima_list <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      arima_mod_selector_body(train$y, val$y, dates_mix = dates_col)
    })
  } else {
    fit_arima <- arima_mod_selector_body(Z_train$y, Z_val$y, dates_mix = dates_col)
  }
  # Obtener los residuales del ajuste ARIMA
  split_residuals <- split_out
  if(!is.null(factor_split)){
    split_residuals$Y$Y_train <- unlist(lapply(fit_arima_list,
                                               function(x)
                                                 stats::residuals(x$fit)$.resid))
    split_residuals$Y$Y_val <- unlist(lapply(fit_arima_list,
                                             function(x)
                                               stats::residuals(x$fit_val)$.resid))
  } else {
    split_residuals$Y$Y_train <- stats::residuals(fit_arima_list$fit)$.resid
    split_residuals$Y$Y_val <- stats::residuals(fit_arima_list$fit_val)$.resid
  }
  # Crear el ajuste lineal
  if(!is.null(dates_col)){
    split_residuals$X$X_train[[dates_name]] <- NULL
    split_residuals$X$X_val[[dates_name]] <- NULL
  }
  fit_lin <- lin_mod_selector(split_residuals,
                              step_selection = step_selection,
                              quiet = quiet)
  rmse_train <- arima_lin_metrics(fit_arima_list, fit_lin,
                                  split_out, dates_name,
                                  objective = "train")
  rmse_val <- arima_lin_metrics(fit_arima_list, fit_lin,
                                split_out, dates_name,
                                objective = "validation")
  return(list(lin_fit = fit_lin, arima_fit = fit_arima_list,
              rmse_train = rmse_train, rmse_val = rmse_val))
}
