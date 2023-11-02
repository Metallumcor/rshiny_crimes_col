#' @encoding UTF-8
#' @title Modelos de regresión - Mapa de datos
#'
#' @description Prototipo de función principal que integra parte de la pipeline
#'     de modelos de regresión: selección de modelo, ajuste de modelo y
#'     reporte de métricas principales. Actualmente la función es capaz de
#'     procesar los siguientes modelos con sus respectivas especificaciones:
#'     \describe{
#'        \item{Modelo lineal:}{Un modelo \code{lm} estándar. Puede incluir
#'        selección stepwise}
#'        \item{ARIMA:}{Modelos de series de tiempo lineales ARIMA sin
#'        componente estacional}
#'        \item{ARIMAX:}{Modelos ARIMA con variables númericas exogenas}
#'        \item{ARIMA+Lineal}{Modelos ARIMA complementados con un modelo
#'        lineal para los residuos derivados}
#'        \item{Lineal+ARIMA}{Modelos lineales complementados con modelos
#'        ARIMA para los residuos derivados}
#'        \item{NN:}{Redes neuronales con \code{keras} de una o dos capas ocultas}
#'     }
#'     Las métricas reportadas son las descritas en \code{reg_metrics}
#'
#'     TODO: crear función reg_metrics y selector NN
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param factor_split Los nombres de las columnas usadas para identificar
#'     los objetos medidos en el tiempo.
#' @param dates_col El nombre de la columna que contiene las fechas.
#' @param models El listado de modelos a evaluar/retornar
#' @param only_best Lógico. Retornar solamente el modelo con el mayor
#'     RMSE en validación
#' @param save_obj Lógico. Guardar el objeto retornado en disco duro. Se
#'     seleccionado el directorio de trabajo por defecto
#' @param ... argumentos adicionales pasados a la función principal o a los modelos
#'
#' @return Una lista que contiene los modelos ajustados y las metrícas por modelo
#' @export
reg_models <- function(split_out, factor_split, dates_col,
                       models = c("arima","arimax","linear","arima_linear",
                                  "linear_arima", "nn_1hl", "nn_2hl", "nn_3hl"),
                       only_best = FALSE,
                       save_obj = FALSE,
                       ...){
  # Listado posibles modelos
  all_mod <- c("arima","arimax","linear","arima_linear",
               "linear_arima", "nn_1hl", "nn_2hl", "nn_3hl")
  # Logico - prueba de seleccion de modelos
  if(prod(models %in% all_mod)!=1) stop("Algunos modelos no son reconocidos")
  ### Cuerpo de la funcion
  # Parametros fijos de las funciones
  step_selection <- TRUE
  p_selection <- TRUE
  quiet <- TRUE
  .x_numcols <- sapply(split_out$X$X_train, is.numeric)
  xreg_cols <- NULL
  if(sum(.x_numcols)>0){
    xreg_cols <- names(split_out$X$X_train)[.x_numcols]
    fourier_K <- NULL
  }
  save_location <- getwd()
  file_name <- "reg_results"
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Check de file existente
  if(save_obj){
    f <- paste0(save_location,"//",file_name,".rds")
    if(file.exists(f)) stop("El archivo de guardado ya existe en la localizacion. Elegir otro nombre o eliminar existente")
  }
  # Flag de modelo ARIMAX
  if("arimax" %in% models & is.null(xreg_cols)){
    models <- models[models != "arimax"]
    warning("No es posible ajustar un ARIMAX. Modelo omitido", call. = FALSE)
  }
  # Funcion auxiliar metricas
  melt_list <- function(x){
    r_names <- names(x)
    keys <- unique(unlist(lapply(x, names)))
    o_list <- do.call(mapply, c(FUN=rbind, lapply(x, `[`, keys)))
    o_list <- lapply(o_list, as.data.frame)
    o_list <- lapply(o_list, function(y){rownames(y)<-r_names;y})
    setNames(o_list, keys)
    o_list
  }
  # Funcion auxiliar para rmse
  joint_rmse <- function(y, z){
    if("models" %in% names(y))
      rmse <- sqrt(mean(sapply(y$models, function(x){get(z,x)})^2))
    else
      rmse <- get(z,y)
    return(rmse)
  }
  # Modelos
  models_names <- models
  object_list <- list()
  models <- list()
  if("linear" %in% models_names){
    object_list$linear <- lin_mod_selector(split_out = split_out,
                                           p_selection = p_selection,
                                           step_selection = step_selection,
                                           quiet = quiet)
    models$linear <- object_list$linear$selected_model
  }
  if("arima" %in% models_names){
    object_list$arima <- arima_mod_selector(split_out = split_out,
                                            factor_split = factor_split,
                                            dates_col = dates_col,
                                            quiet = quiet)
    if("models" %in% names(object_list$arima)){
      models$arima <- lapply(object_list$arima$models,function(x) x$model)
      if(class(object_list$arima$factor_levels) == "data.frame")
        names(models$arima) <- sapply(object_list$arima$factor_levels,
                                      function(x)paste(x,collapse = "-"))
      else
        names(models$arima) <- object_list$arima$factor_levels
    } else {
      models$arima <- object_list$arima$model
    }
  }
  if("arimax" %in% models_names){
    object_list$arimax <- arimax_mod_selector(split_out = split_out,
                                              xreg_cols = xreg_cols,
                                              fourier_K = fourier_K,
                                              dates_col = dates_col,
                                              factor_split = factor_split,
                                              quiet = quiet)
    models$arimax <- object_list$arimax$model
    if("models" %in% names(object_list$arimax)){
      models$arimax <- lapply(object_list$arimax$models,function(x) x$model)
      if(class(object_list$arimax$factor_levels) == "data.frame")
        names(models$arimax) <- sapply(object_list$arimax$factor_levels,
                                       function(x)paste(x,collapse = "-"))
      else
        names(models$arimax) <- object_list$arimax$factor_levels
    } else {
      models$arimax <- object_list$arimax$model
    }
  }
  if("arima_linear" %in% models_names){
    object_list$arima_linear <- arima_lin_selector(split_out = split_out,
                                                   factor_split = factor_split,
                                                   dates_col = dates_col,
                                                   step_selection = step_selection,
                                                   quiet = quiet)
    models$arima_linear <- list(lin_fit = object_list$arima_linear$lin_fit,
                                arima_fit = object_list$arima_linear$arima_fit)
  }
  if("linear_arima" %in% models_names){
    object_list$linear_arima <- lin_arima_selector(split_out = split_out,
                                                   factor_split = factor_split,
                                                   dates_col = dates_col,
                                                   step_selection = step_selection,
                                                   quiet = quiet)
    models$linear_arima <- list(lin_fit = object_list$linear_arima$lin_fit,
                                arima_fit = object_list$linear_arima$arima_fit)
  }
  if("nn_1hl" %in% models_names){
    object_list$nn_1hl <- nn_1hl(split_out = split_out,...)
    models$nn_1hl <- object_list$nn_1hl$fit
  }
  if("nn_2hl" %in% models_names){
    object_list$nn_2hl <- nn_2hl(split_out = split_out,...)
    models$nn_2hl <- object_list$nn_2hl$fit
  }
  if("nn_3hl" %in% models_names){
    object_list$nn_3hl <- nn_3hl(split_out = split_out,...)
    models$nn_3hl <- object_list$nn_3hl$fit
  }
  # Pasar los rmse y generales un solo df
  rmse_train <- unlist(lapply(object_list, function(x) joint_rmse(x, "rmse_train")))
  rmse_val <- unlist(lapply(object_list, function(x) joint_rmse(x, "rmse_val")))
  rmse_df <- data.frame(train = rmse_train, val = rmse_val)
  rownames(rmse_df) <- gsub(".rmse","",rownames(rmse_df))
  # rownames(acc) <- gsub(".rmse","",rownames(acc))
  # rmse por sector/ TODO: Forma de hacerlo para multiples sectores
  if(!is.null(factor_split)){
    if(length(factor_split)>1) warning("Aun se esta desarrollando la funcion para multiples factores")
    else{
      rmse_loc_train <- mapply(
        function(x,y){
          Z <- rmse_local(x, y,
                          split_out = split_out, factor_split = factor_split,
                          kind = "train")
          return(Z[,c(1,2)])
        },
        x = object_list, y = names(object_list), SIMPLIFY = FALSE
      )
      rmse_loc_train <- Reduce(function(...) merge(..., by = factor_split), rmse_loc_train)
      rmse_loc_val <- mapply(
        function(x,y){
          Z <- rmse_local(x, y,
                          split_out = split_out, factor_split = factor_split,
                          kind = "validation")
          return(Z[,c(1,2)])
        },
        x = object_list, y = names(object_list), SIMPLIFY = FALSE
      )
      rmse_loc_val <- Reduce(function(...) merge(..., by = factor_split), rmse_loc_val)
      colnames(rmse_loc_val) <- colnames(rmse_loc_train) <- c(factor_split,names(object_list))
    }
  }
  # Metricas - Pendiente
  # Retorno
  if(!only_best){
    reg_result <- list(models = models, rmse_global = rmse_df)
    if(exists("rmse_loc_train") & exists("rmse_loc_val")){
      reg_result$rmse_loc_train <- rmse_loc_train
      reg_result$rmse_loc_val <- rmse_loc_val
    }
    f <- paste0(save_location,"//",file_name,".rds")
    if(save_obj) saveRDS(clsf_result,file = f)
    return(reg_result)
  }
  # Encontrar el que max acc en val, pero tambien en train
  max_val <- which(rmse_df$val == max(rmse_df$val))
  best_mod <- models[[max_val[which.max(rmse_df[max_val,"train"])]]]
  f <- paste0(save_location,"//",file_name,".rds")
  if(save_obj) saveRDS(best_mod,file = f)
  return(best_mod)
}
