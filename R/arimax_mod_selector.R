#' @encoding UTF-8
#' @title Ajuste de modelos ARIMAX con validación por RMSE.
#'
#' @description Modelos ARIMAX automáticos para una serie de tiempo o múltiples
#'     que son identificadas por un conjunto de factores únicos (datos
#'     longitudinales). Acepta variables exógenas e inclusión de
#'     series de fourier para capturar patrones estacionales.
#'
#'     No se realizan múltiples pases de validación.
#'
#'     TODO: Ejemplos y múltiples pases de validación.
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#' @param xreg_cols Los nombres de las columnas que contienen las
#'     variables exógenas.
#' @param fourier_K El grado del suavizado de fourier para capturar
#'     patrones estacionales. Dejar NULL para omitirlo.
#' @param dates_col El nombre de la columna que contiene las fechas.
#' @param factor_split Los nombres de las columnas usadas para identificar
#'     los objetos medidos en el tiempo.
#' @param quiet Callar las salidas en consola de los modelos.
#'
#' @return Una lista que incluye el mejor modelo ARIMA (para una serie) ó
#'     una lista que incluye los factores que identifican cada serie
#'     \code{factor_list} y una lista de los mejores modelos para cada
#'     serie individual. Las salidas de modelos incluyen los RMSE en
#'     entrenamiento y validación.
#' @export
#' @importFrom utils install.packages
arimax_mod_selector <- function(split_out, xreg_cols,
                                fourier_K = NULL,
                                dates_col = NULL,
                                factor_split = NULL,
                                quiet = FALSE){
  if(!is_inst("feasts")){
    warning("El paquete feasts es requerido, instalando...")
    install.packages("feasts", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
    if(!is_inst("feasts")) stop("La instalacion fallo, intente instalar feasts manualmente")
  }
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  if(!is.null(dates_col)) dates_col <- unique(get(dates_col,
                                                  dplyr::bind_rows(X_train,X_val)))
  if(!is.null(factor_split)){
    if(sum(!factor_split %in% names(X_train)) != 0) stop("Factores por fuera de X")
    Z_train_split <- split_by_factors(data.frame(value = Y_train, X_train),
                                      factor_split = factor_split)
    Z_val_split <- split_by_factors(data.frame(value = Y_val, X_val),
                                    factor_split = factor_split)
    factor_levels <- unique(X_train[,factor_split])
    model_list <- lapply(1:length(Z_train_split), function(v){
      train <- Z_train_split[[v]]
      val <- Z_val_split[[v]]
      arimax_mod_selector_output(train, val, xreg_cols, fourier_K,
                                 dates_mix = dates_col,
                                 quiet = quiet)
    })
    return(list(factor_levels = factor_levels, models = model_list))
  }
  Z_train <- data.frame(y = Y_train, X_train)
  Z_val <- data.frame(y = Y_val, X_val)
  arimax_mod_selector_output(Z_train, Z_val, xreg_cols, fourier_K,
                             dates_mix = dates_col, quiet = quiet)
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
#' @importFrom stats as.formula
arimax_mod_selector_body <- function(Z_train, Z_val, xreg_cols,
                                     dates_mix = NULL,
                                     fourier_k = NULL,
                                     model = NULL){
  if(is.null(dates_mix)){
    dates_mix <- as.Date("2013-03-27")+0:(nrow(Z_train)+nrow(Z_val)-1)
    Z_train$FECHA <- utils::head(dates_mix,nrow(Z_train))
    Z_val$FECHA <- utils::tail(dates_mix,nrow(Z_val))
  }
  fit <- model
  if(is.null(model)){
    str_params <- paste(c(xreg_cols,"PDQ(0,0,0)"),collapse = "+")
    if(!is.null(fourier_k))
      str_params <- paste(c(paste0("fourier(K=",fourier_k,")"), str_params),
                          collapse = "+")
    f_mod <- stats::as.formula(paste("value",str_params,sep="~"))
    fit <- Z_train %>%
      dplyr::tibble() %>%
      tsibble::as_tsibble() %>%
      fabletools::model(fable::ARIMA(f_mod))
  }
  Z_val <- Z_val %>%
    dplyr::tibble() %>%
    tsibble::as_tsibble()
  fit_val <- fabletools::refit(fit, Z_val,
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
arimax_mod_selector_output <- function(Z_train, Z_val, xreg_cols,
                                       fourier_K,
                                       dates_mix = NULL,
                                       quiet = FALSE){
  arimax_fit <- arimax_mod_selector_body(Z_train, Z_val, xreg_cols,
                                        dates_mix,
                                        fourier_K,
                                        model = NULL)
  metrics_val <- fabletools::accuracy(arimax_fit$fit_val)
  res_train <- stats::residuals(arimax_fit$fit)
  res_train <- res_train$.resid[stats::complete.cases(res_train$.resid)]
  rmse_train <- sqrt(mean(res_train^2))
  if(!quiet)
    cat("\nModelo base","\n-----","\nRMSE train: ",rmse_train,
        "\nRMSE val: ",metrics_val$RMSE,"\n")
  return(list(model = arimax_fit$fit, rmse_train = rmse_train,
              rmse_val = metrics_val$RMSE))
}
