#' @encoding UTF-8
#' @title Ajuste de modelo experimental en dos etapas lineal + ARIMA
#'     con validación por RMSE.
#'
#' @description Este es un algoritmo experimental que corre en primer lugar
#'     los modelos lineales automáticos (\code{lin_mod_selector})
#'     para el conjunto de datos solicitado
#'     y a los cuales se les extrae sus residuales para ajustar modelos
#'     ARIMA (\code{arima_mod_selector}). La variable obtenida
#'     es una combinación lineal del pronosticó lineal más los residuales
#'     ARIMA estimados.
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
#'     modelo lineal?
#' @param quiet Callar las salidas en consola de los modelos.
#'
#' @return Una lista que incluye los modelos ARIMA de residuos (\code{arima_fit}),
#'     el modelo lineal (\code{lin_fit}), y los
#'     RMSE conjuntos obtenidos en entrenamiento y validación
#'
#' @export
#' @importFrom utils install.packages
lin_arima_selector <- function(split_out,
                               factor_split,
                               dates_col = NULL,
                               step_selection = TRUE,
                               quiet = FALSE){
  if(!is_inst("feasts")){
    warning("El paquete feasts es requerido, instalando...")
    utils::install.packages("feasts", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
    if(!is_inst("feasts")) stop("La instalacion fallo, intente instalar feasts manualmente")
  }
  # Obtener el ajuste lineal
  split_lin <- split_out
  if(!is.null(dates_col)) split_lin$X$X_train[[dates_col]] <- NULL
  lin_fit <- lin_mod_selector(split_lin, step_selection, quiet)
  # Obtener los residuales del modelo lineal
  split_res <- split_out
  split_res$Y$Y_train <- stats::residuals(lin_fit$selected_model)
  split_res$Y$Y_val <- stats::predict(lin_fit$selected_model,split_res$X$X_val)-
    split_res$Y$Y_val
  # Obtener el ajuste ARIMA
  Z_train <- data.frame(y = split_res$Y$Y_train, split_res$X$X_train)
  Z_val <- data.frame(y = split_res$Y$Y_val, split_res$X$X_val)
  dates_name <- dates_col
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(Z_train,Z_val)))
  if(sum(!factor_split %in% names(Z_train)) != 0) stop("Factores por fuera de X")
  if(!is.null(factor_split)){
    Z_train_split <- split_by_factors(Z_train,
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(Z_val,
                                    factor_split = factor_split)
    factor_levels <- unique(Z_train[,factor_split])
    arima_fit_list <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      arima_mod_selector_body(train$y, val$y, dates_mix = dates_col)
    })
  } else {
    arima_fit_list <- arima_mod_selector_body(Z_train$y, Z_val$y, dates_mix = dates_col)
  }
  rmse_train <- lin_arima_metrics(arima_fit_list, lin_fit,
                                  split_out, dates_name,
                                  objective = "train")
  rmse_val <- lin_arima_metrics(arima_fit_list, lin_fit,
                                split_out, dates_name,
                                objective = "validation")
  return(list(lin_fit = lin_fit, arima_fit = arima_fit_list,
              rmse_train = rmse_train, rmse_val = rmse_val))
}
