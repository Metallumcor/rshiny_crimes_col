#' @encoding UTF-8
#' @title Visualización del RMSE de un modelo por sectores
#'
#' @description Función experimental usada para representar gráficamente
#'     en \code{leaflet} los RMSE de validación obtenidos de una salida
#'     de selección de modelo.
#'
#'     TODO: Ejemplos.
#'
#' @param city El objeto donde se guarda la información espacial (geometrías)
#' @param type El tipo de modelo usado. Al momento de realización se reconoce
#'    "arima", "arimax", "linear" y "arima_linear"
#' @param fit_object El objeto retornado por la salida de ajuste automático
#'    correspondiente
#' @param merge_on El nombre de la columna usada para juntar las salidas de
#'    los modelos y el objeto de \code{city}. Debe identificar los agregados
#'    espaciales a visualizar, en el caso de modelos "arima", "arimax" y
#'    "arima_lin" debe ser igual al parámetro \code{factor_split} suministrado.
#' @param split_out La partición de los datos en entrenamiento, validación
#'    y prueba
#' @param ... Otros parámetros adicionales que pueden variar según el tipo
#'    de modelo suministrado. Para todos se requiere un parámetro
#'    \code{names_col} que sea igual al nombre de la columna (dentro del
#'    objeto espacial) que contenga los **labels** deseados de las agregaciones
#'    espaciales.
#'
#' @return Un mapa interactivo que muestra los RMSE
#' @export
#' @importFrom stats cor
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom sf st_as_sf
#' @importFrom stats predict
rmse_map <- function(city,
                     type = c("arima","arimax","linear","arima_linear",
                              "linear_arima"),
                     fit_object,
                     merge_on,
                     split_out,
                     ...){
  # Dots
  names_col <- "NOMBRE_DIV1"
  dots <- list(...)
  list2env(dots, envir = environment())
  # Creacion de sf
  sf_obj <- sf::st_as_sf(city)
  sf_merged <- sf_obj %>%
    dplyr::group_by(!!as.name(merge_on), !!as.name(names_col)) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))
  # Base para correlacion y mapa
  Z <- rmse_local(fit_object = fit_object, type = type,
                  split_out = split_out, factor_split = merge_on,
                  kind = "validation", ...)
  # Merge
  sf_rmse <- merge(sf_merged, Z, by = merge_on)
  # Paleta
  pal_cont <- colorNumeric(
    palette = "YlOrRd",
    reverse = FALSE,
    domain = sf_rmse$RMSE)
  # Grafico
  p <- leaflet(sf_rmse) %>%
    addTiles() %>%
    addPolygons(
      color = 'black',
      weight = 1,
      fillColor = ~pal_cont(RMSE),
      fillOpacity = 0.4, smoothFactor = 0.5,
      highlight = highlightOptions(
        weight = 3,
        color = "black",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      # Ojo con el nombre
      label = get(names_col, sf_merged),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend("bottomright",
              pal = pal_cont,
              title = "RMSE de validacion",
              bins = 5,
              values = ~RMSE
    )
  # Correlacion de Spearman y Kendall
  correlation <- cor(Z$RMSE, Z$Obs_sd, method = "spearman")
  return(list(map = p, correlation = correlation))
}

#' @encoding UTF-8
#' @title Obtención de RMSE especificó de acuerdo a un factor
#'
#' @description Los modelos de regresión/predicción de tipo numérica pueden
#'     son capaces de exponer su medida de desempeño característica (RMSE)
#'     a través de los niveles de un factor que permite clasificar las
#'     observaciones en grupos mutuamente excluyentes y esta función permite
#'     dicha presentación
#'
#' @param fit_object El objeto retornado por la salida de ajuste automático
#'    correspondiente
#' @param type El tipo de modelo usado. Al momento de realización se reconoce
#'    "arima", "arimax", "linear", "arima_linear", "nn_1hl", "nn_2hl" y "var"
#' @param split_out La partición de los datos en entrenamiento, validación
#'    y prueba
#' @param factor_split El nombre de la columna que contiene el factor usado
#'    para distribuir el RMSE
#' @param kind El objetivo del cálculo. Puede ser "train" o "validation"
#' @param ... Parámetros adicionales
#'
#' @return Un \code{tibble} que incluye el factor, el RMSE por nivel y la
#'    desviación estandar de la variable objetivo en cada nivel
#' @export
rmse_local <- function(fit_object, type,
                       split_out,
                       factor_split,
                       kind = c("validation","train"),
                       ...){
  kind <- match.arg(kind)
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Base para correlacion y mapa
  if(!exists("split_out")) stop("Debe proporcionarse la variable split_out")
  if(kind == "validation")
    df_pred <- data.frame(Y_obs = split_out$Y$Y_val,
                          level_pred = split_out$X$X_val[[factor_split]])
  else
    df_pred <- data.frame(Y_obs = split_out$Y$Y_train,
                          level_pred = split_out$X$X_train[[factor_split]])
  names(df_pred)[2] <- factor_split
  # Obtener los datos
  if(type[1] == "arima" | type[1] == "arimax"){
    Z_factors <- fit_object$factor_levels
    if(!inherits(Z_factors,"data.frame")){
      Z_factors <- data.frame(Z_factors)
      names(Z_factors) <- factor_split
    }
    if(kind == "validation")
      Z_response <- unlist(lapply(fit_object$models,
                                  function(x)x$rmse_val))
    else
      Z_response <- unlist(lapply(fit_object$models,
                                  function(x)x$rmse_train))
    Z_1 <- data.frame(Z_response, Z_factors)
    names(Z_1)[1] <- "RMSE"
    Z_2 <- df_pred %>%
      dplyr::group_by(!!as.name(factor_split)) %>%
      dplyr::summarise(Obs_sd = sd(Y_obs))
    Z <- merge(Z_1, Z_2, by = factor_split)
  }
  if(type[1] == "arima_linear"){
    if(kind == "validation"){
      Y_pred_pre <- unlist(lapply(fit_object$arima_fit,
                                  function(x)
                                    stats::fitted(x$fit_val)$.fitted))
      res_pred <- stats::predict(fit_object$lin_fit$selected_model,
                                 split_out$X$X_val)
    }
    else{
      Y_pred_pre <- unlist(lapply(fit_object$arima_fit,
                                  function(x)
                                    stats::fitted(x$fit)$.fitted))
      res_pred <- stats::predict(fit_object$lin_fit$selected_model,
                                 split_out$X$X_train)
    }
    Y_pred <- Y_pred_pre + res_pred
    df_pred <- data.frame(Y_pred = Y_pred, df_pred)
    names(df_pred)[3] <- factor_split
    Z <- df_pred %>%
      dplyr::group_by(!!as.name(factor_split)) %>%
      dplyr::summarise(RMSE = sqrt(mean((Y_pred-Y_obs)^2)),
                       Obs_sd = sd(Y_obs))
  }
  if(type[1] == "linear"){
    if(kind == "validation")
      Y_pred <- stats::predict(fit_object$selected_model, split_out$X$X_val)
    else
      Y_pred <- stats::predict(fit_object$selected_model, split_out$X$X_train)
    df_pred <- data.frame(Y_pred = Y_pred, df_pred)
    names(df_pred)[3] <- factor_split
    Z <- df_pred %>%
      dplyr::group_by(!!as.name(factor_split)) %>%
      dplyr::summarise(RMSE = sqrt(mean((Y_pred-Y_obs)^2)),
                       Obs_sd = sd(Y_obs))
  }
  if(type[1] == "linear_arima"){
    if(kind == "validation"){
      res_pred <- unlist(lapply(fit_object$arima_fit,
                                function(x)
                                  stats::fitted(x$fit_val)$.fitted))
      Y_pred_pre <- stats::predict(fit_object$lin_fit$selected_model,
                                   split_out$X$X_val)
    }
    else{
      res_pred <- unlist(lapply(fit_object$arima_fit,
                                function(x)
                                  stats::fitted(x$fit)$.fitted))
      Y_pred_pre <- stats::predict(fit_object$lin_fit$selected_model,
                                   split_out$X$X_train)
    }
    Y_pred <- Y_pred_pre + res_pred
    df_pred <- data.frame(Y_pred = Y_pred, df_pred)
    names(df_pred)[3] <- factor_split
    Z <- df_pred %>%
      dplyr::group_by(!!as.name(factor_split)) %>%
      dplyr::summarise(RMSE = sqrt(mean((Y_pred-Y_obs)^2)),
                       Obs_sd = sd(Y_obs))
  }
  if(type[1] == "nn_1hl" | type[1] == "nn_2hl" | type[1] == "nn_3hl"){
    split_keras <- split_nn(split_out)
    if(kind == "validation")
      Y_pred <- stats::predict(fit_object$fit, split_keras$X$X_val)
    else
      Y_pred <- stats::predict(fit_object$fit, split_keras$X$X_train)
    df_pred <- data.frame(Y_pred = Y_pred, df_pred)
    names(df_pred)[3] <- factor_split
    Z <- df_pred %>%
      dplyr::group_by(!!as.name(factor_split)) %>%
      dplyr::summarise(RMSE = sqrt(mean((Y_pred-Y_obs)^2)),
                       Obs_sd = sd(Y_obs))
  }
  if(type[1] == "var"){
    if(kind == "validation"){
      if(!exists("dates_col")) stop("Especificar argumento dates_col")
      test_df <- data.frame(y = split_out$Y$Y_val, split_out$X$X_val) %>%
        tidyr::pivot_wider(id_cols = !!as.name(dates_col),
                           names_from = !!as.name(factor_split),
                           values_from = y)
      pred_test <- predict.varest(fit_object$fit, test = test_df[,-1])
      desv <- (pred_test$fcst-test_df[,-1])^2
      fs_names <- colnames(pred_test)
      rmse_res <- apply(desv, 2, function(x) sqrt(mean(x)))
      rmse_res <- rmse_res[order(match(fs_names,
                                       as.character(df_pred$level_pred)))]
      names(df_pred)[2] <- factor_split
      Z <- df_pred %>%
        dplyr::group_by(!!as.name(factor_split)) %>%
        dplyr::summarise(Obs_sd = sd(Y_obs))
      rmse_res <- rmse_res[order(match(fs_names, as.character(Z[,1])))]
      Z <- Z %>%
        dplyr::mutate(RMSE = rmse_res)
    } else {
      # Pendiente
    }
  }
  return(Z)
}
