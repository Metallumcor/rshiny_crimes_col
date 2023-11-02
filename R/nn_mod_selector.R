#' @encoding UTF-8
#' @title Redes neuoronales de una capa oculta
#'
#' @description Por defecto usadas para clasificación con salida softmax. Se
#'     asume que los datos vienen estandarizados (numéricos)
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param split_transform Lógico. Aplicar encoding a los datos de ser necesario
#' @param early_stop Lógico. Usar para temprana para mejorar generalización
#' @param ... argumentos adicionales pasados a Keras
#'
#' @return Un modelo ajustado en Keras
#' @export
#'
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras compile
#' @importFrom keras fit
#' @examples
#' split_example <- create_split_ts(mtcars$vs, subset(mtcars, select = -vs),
#' split_prop = c(0.7,0.25,0.05))
#' split_example$Y$Y_train <- as.factor(split_example$Y$Y_train)
#' split_example$Y$Y_val <- as.factor(split_example$Y$Y_val)
#' split_example$Y$Y_test <- as.factor(split_example$Y$Y_test)
#' modelo <- nn_1hl(split_example)
nn_1hl <- function(split_out,
                   split_transform = TRUE,
                   early_stop = TRUE,
                   ...){
  # Transformacion
  split_keras <- split_out
  if(split_transform)
    split_keras <- split_nn(split_out)
  # Parametros globales
  n_inp <- ncol(split_keras$X$X_train)
  n_out <- ncol(split_keras$Y$Y_train)
  if(is.null(n_out)) n_out <- 1
  nods_lay1 <- n_inp
  n_epoch <- 200
  batch_size <- 512
  patience <- 5
  verbose_parm <- 0
  rate_1 <- 0
  activ_lay1 <- "relu"
  activ_out <- "softmax"
  metric_call <- metric <- "accuracy"
  loss <- "categorical_crossentropy"
  # Custom fun
  if(n_out  == 1){
    metric <- keras::custom_metric("root_mean_squared_error", function(y_true, y_pred) {
      keras::k_sqrt(keras::k_mean(keras::k_square(y_pred - y_true)))
    })
    metric_call <- "root_mean_squared_error"
    loss <- "mse"
    activ_out <- "linear"
  }
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Configuracion del callback
  callback_list <- NULL
  if(early_stop){
    callback_list <- list(
      keras::callback_early_stopping(
        monitor = metric_call,
        patience = patience
      )
    )
  }
  # Modelo
  model <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = nods_lay1, activation = activ_lay1, input_shape = c(n_inp)) %>%
    keras::layer_dropout(rate = rate_1) %>%
    keras::layer_dense(units = n_out, activation = activ_out)
  model %>%
    keras::compile(
      loss = loss,
      optimizer = keras::optimizer_adam(),
      metrics = metric
    )
  model %>%
    fit(
      split_keras$X$X_train,
      split_keras$Y$Y_train,
      epochs = n_epoch,
      batch_size = batch_size,
      view_metrics = FALSE,
      callbacks = callback_list,
      verbose = verbose_parm,
      validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
    )
  if(n_out > 1){
    acc_train <- clsf_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "accuracy")
    acc_val <- clsf_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "accuracy")
    return(list(fit = model, acc_train = acc_train, acc_val = acc_val))
  } else {
    rmse_train <- reg_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "rmse")
    rmse_val <- reg_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "rmse")
    return(list(fit = model, rmse_train = rmse_train, rmse_val = rmse_val))
  }
}

#' @encoding UTF-8
#' @title Redes neuoronales de dos capas ocultas
#'
#' @description Por defecto usadas para clasificación con salida softmax. Se
#'     asume que los datos vienen estandarizados (numéricos)
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param split_transform Lógico. Aplicar encoding a los datos de ser necesario
#' @param early_stop Lógico. Usar para temprana para mejorar generalización
#' @param ... argumentos adicionales pasados a Keras
#'
#' @return Un modelo ajustado en Keras
#' @export
#'
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras compile
#' @importFrom keras fit
#' @examples
#' split_example <- create_split_ts(mtcars$vs, subset(mtcars, select = -vs),
#' split_prop = c(0.7,0.25,0.05))
#' split_example$Y$Y_train <- as.factor(split_example$Y$Y_train)
#' split_example$Y$Y_val <- as.factor(split_example$Y$Y_val)
#' split_example$Y$Y_test <- as.factor(split_example$Y$Y_test)
#' modelo <- nn_2hl(split_example)
nn_2hl <- function(split_out,
                   split_transform = TRUE,
                   early_stop = TRUE,
                   ...){
  # Transformacion
  split_keras <- split_out
  if(split_transform)
    split_keras <- split_nn(split_out)
  # Parametros globales
  n_inp <- ncol(split_keras$X$X_train)
  n_out <- ncol(split_keras$Y$Y_train)
  if(is.null(n_out)) n_out <- 1
  nods_lay1 <- n_inp
  nods_lay2 <- floor(n_inp/2)
  n_epoch <- 200
  batch_size <- 512
  patience <- 5
  verbose_parm <- 0
  rate_1 <- rate_2 <- 0
  activ_out <- "softmax"
  activ_lay1 <- activ_lay2 <- "relu"
  metric_call <- metric <- "accuracy"
  loss <- "categorical_crossentropy"
  # Custom fun
  if(n_out  == 1){
    metric <- keras::custom_metric("root_mean_squared_error", function(y_true, y_pred) {
      keras::k_sqrt(keras::k_mean(keras::k_square(y_pred - y_true)))
    })
    metric_call <- "root_mean_squared_error"
    loss <- "mse"
    activ_out <- "linear"
  }
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Configuracion del callback
  callback_list <- NULL
  if(early_stop){
    callback_list <- list(
      keras::callback_early_stopping(
        monitor = metric_call,
        patience = patience
      )
    )
  }
  # Modelo
  model <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = nods_lay1, activation = activ_lay1, input_shape = c(n_inp)) %>%
    keras::layer_dropout(rate = rate_1) %>%
    keras::layer_dense(units = nods_lay2, activation = activ_lay2) %>%
    keras::layer_dropout(rate = rate_2) %>%
    keras::layer_dense(units = n_out, activation = activ_out) %>%
    keras::compile(
      loss = loss,
      optimizer = keras::optimizer_adam(),
      metrics = metric
    )
  model %>%
    fit(
      split_keras$X$X_train,
      split_keras$Y$Y_train,
      epochs = n_epoch,
      batch_size = batch_size,
      view_metrics = FALSE,
      callbacks = callback_list,
      verbose = verbose_parm,
      validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
    )
  if(n_out > 1){
    acc_train <- clsf_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "accuracy")
    acc_val <- clsf_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "accuracy")
    return(list(fit = model, acc_train = acc_train, acc_val = acc_val))
  } else {
    rmse_train <- reg_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "rmse")
    rmse_val <- reg_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "rmse")
    return(list(fit = model, rmse_train = rmse_train, rmse_val = rmse_val))
  }
}

#' @encoding UTF-8
#' @title Redes neuoronales de tres capas ocultas
#'
#' @description Por defecto usadas para clasificación con salida softmax. Se
#'     asume que los datos vienen estandarizados (numéricos)
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param split_transform Lógico. Aplicar encoding a los datos de ser necesario
#' @param early_stop Lógico. Usar para temprana para mejorar generalización
#' @param ... argumentos adicionales pasados a Keras
#'
#' @return Un modelo ajustado en Keras
#' @export
#'
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras compile
#' @importFrom keras fit
#' @examples
#' split_example <- create_split_ts(mtcars$vs, subset(mtcars, select = -vs),
#' split_prop = c(0.7,0.25,0.05))
#' split_example$Y$Y_train <- as.factor(split_example$Y$Y_train)
#' split_example$Y$Y_val <- as.factor(split_example$Y$Y_val)
#' split_example$Y$Y_test <- as.factor(split_example$Y$Y_test)
#' modelo <- nn_3hl(split_example)
nn_3hl <- function(split_out,
                   split_transform = TRUE,
                   early_stop = TRUE,
                   ...){
  # Transformacion
  split_keras <- split_out
  if(split_transform)
    split_keras <- split_nn(split_out)
  # Parametros globales
  n_inp <- ncol(split_keras$X$X_train)
  n_out <- ncol(split_keras$Y$Y_train)
  if(is.null(n_out)) n_out <- 1
  nods_lay1 <- n_inp
  nods_lay2 <- floor(n_inp/2)
  nods_lay3 <- floor(n_inp/4)
  n_epoch <- 200
  batch_size <- 512
  patience <- 5
  verbose_parm <- 0
  rate_1 <- rate_2 <- rate_3 <- 0
  activ_out <- "softmax"
  activ_lay1 <- activ_lay2 <- activ_lay3 <- "relu"
  metric_call <- metric <- "accuracy"
  loss <- "categorical_crossentropy"
  # Custom fun
  if(n_out  == 1){
    metric <- keras::custom_metric("root_mean_squared_error", function(y_true, y_pred) {
      keras::k_sqrt(keras::k_mean(keras::k_square(y_pred - y_true)))
    })
    metric_call <- "root_mean_squared_error"
    loss <- "mse"
    activ_out <- "linear"
  }
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Configuracion del callback
  callback_list <- NULL
  if(early_stop){
    callback_list <- list(
      keras::callback_early_stopping(
        monitor = metric_call,
        patience = patience
      )
    )
  }
  # Modelo
  model <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = nods_lay1, activation = activ_lay1, input_shape = c(n_inp)) %>%
    keras::layer_dropout(rate = rate_1) %>%
    keras::layer_dense(units = nods_lay2, activation = activ_lay2) %>%
    keras::layer_dropout(rate = rate_2) %>%
    keras::layer_dense(units = nods_lay3, activation = activ_lay3) %>%
    keras::layer_dropout(rate = rate_3) %>%
    keras::layer_dense(units = n_out, activation = activ_out) %>%
    keras::compile(
      loss = loss,
      optimizer = keras::optimizer_adam(),
      metrics = metric
    )
  model %>%
    fit(
      split_keras$X$X_train,
      split_keras$Y$Y_train,
      epochs = n_epoch,
      batch_size = batch_size,
      view_metrics = FALSE,
      callbacks = callback_list,
      verbose = verbose_parm,
      validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
    )
  if(n_out > 1){
    acc_train <- clsf_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "accuracy")
    acc_val <- clsf_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "accuracy")
    return(list(fit = model, acc_train = acc_train, acc_val = acc_val))
  } else {
    rmse_train <- reg_metrics(model, split_out = split_out,
                              objective = "train",
                              metrics_out = "rmse")
    rmse_val <- reg_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "rmse")
    return(list(fit = model, rmse_train = rmse_train, rmse_val = rmse_val))
  }
}

