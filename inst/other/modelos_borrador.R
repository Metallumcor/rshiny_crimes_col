###### Borrador para realizar predicciones

### Idea
#' En lineas generales debera contarse con los conjuntos de
#' a. Entrenamiento: Aca vendra toda la informacion usada para ajustar el modelo
#' b. Validacion: Contra este conjunto se evaluan los modelos ajustados para
#' obtener una medida insesgada al tiempo que se ajustan los hiperparametros
#' (de existir) o la eleccion de variables involucradas en el modelo.
#' c. Test: Este conjunto es el intocable. Ya es la evaluacion final y el modelo
#' no debe ser modificado al llegar a esta etapa.
#'
#' No se dispone de tiempo para hacer ingenieria de features de forma manual,
#' ni de realizar un seguimiento personal a cada modelo en la etapa de
#' validacion. Es asi, que se necesita aplicar un metodo de seleccion auto
#' para las variables a dejar e hiperparametros.
#'
#' La medida de rendimiento para evaluar en validacion sera el MSE de las
#' predicciones un paso adelante. Esto implica que el algoritmo puede
#' alimentarse de la informacion futura para sus predicciones, siempre y
#' cuando no este en el periodo a predicir o posterior.
#'
#' Se crearan dos modelos de benchmark
#' 1. Modelo de regresion: En este modelo se usara la informacion disponible
#' en la base de datos de cada delito para predecir sus conteos en una
#' fecha determinada. Se considera usar informacion de la regresora en
#' periodos anteriores (sin ser un modelo ARX).
#' 2. Modelo AR(MA): En este modelo se usara solamente la informacion de
#' periodos previos.

### A) Funcion de split train-val-test
create_split_ts <- function(Y, X = NULL, split_prop = c(0.9,0.05,0.05)){
  if (sum(split_prop) != 1){
    stop("La suma de las proporciones debe ser 1")
  }
  if (!class(Y) %in% c("character", "complex",
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
    return(list(Y = Y_out, X = X_out))
  }
  return(Y_out)
}

### B) Metrica de rendimiento - No usar con ajuste de ARMA (Ver fable)
rmse_metric <- function(model, split_output){
  X_val <- get("X_val", split_output$X)
  Y_val <- get("Y_val", split_output$Y)
  res <- predict(model, X_val)-Y_val
  rmse <- sqrt(res%*%res)
  return(rmse)
}

### C) Inclusion de rezagos en X. Debe realizarse pre split
# Funcion para obtener columna lageada
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
# Funcion de inclusion de lags en X
add_laggs <- function(X, Y, p = 10){
  if(!inherits(X, c("data.frame", "data.table", "matrix"))){
    stop("X debe ser un data.frame, data.table o matrix")
  }
  if(inherits(X, "matrix")){
    X <- as.data.frame(X)
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
  return(result[complete.cases(result),])
}
### D) Procesamiento de FECHA
date_split <- function(x){
  x$ANNO <- as.numeric(substr(x$FECHA, 1, 4))
  x$MES <- substr(x$FECHA,6,7)
  x$FECHA <- NULL
  return(x)
}

### 1) Primer modelo
#' Asumiremos que X ya tiene incluido los terminos AR por input del usuario.
#' Se sugiere que el orden AR sea elegido en simultaneo para el AR puro.
#' Ver funcion de C)
lin_mod_selector <- function(split_out, tunning_intercept = TRUE,
                             quiet = FALSE){
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  Y_val <- split_out$Y$Y_val
  Y_train <- split_out$Y$Y_train
  train <- cbind(data.frame(Y_train = Y_train), X_train)
  base_model <- lm(Y_train ~ ., data = train)
  rmse_base_val <- rmse_metric(model = base_model, split_output = split_out)
  rmse_base_train <- sqrt(sum(residuals(base_model)^2))
  if(!quiet) cat("\nModelo base","\n-----","\nRMSE train: ",rmse_base_train,
      "\nRMSE val: ",rmse_base_val,"\n")
  if (tunning_intercept){
    noint_model <- lm(Y_train ~ -1 + ., data = train)
    rmse_noint_train <- sqrt(sum(residuals(noint_model)^2))
    rmse_noint_val <- rmse_metric(model = noint_model, split_output = split_out)
    if(!quiet) cat("\nModelo sin intercepto","\n-----","\nRMSE train: ",
                    rmse_noint_train,"\nRMSE val: ",rmse_noint_val)
    if (rmse_base_val > rmse_noint_val){
      return(list(selected_model = noint_model, rmse_train = rmse_noint_train,
             rmse_val = rmse_noint_train))
    }
  }
  return(list(selected_model = base_model, rmse_train = rmse_base_train,
         rmse_val = rmse_base_val))
}

### 2) Segundo modelo
#' Usamos la rutina auto.arima de forecast
#' Se sugiere transicion a AR de fable
ar_mod_selector <- function(split_out, p_max = 10, dates_mix = NULL,
                            quiet = FALSE){
  Y_train <- split_out$Y$Y_train
  Y_val <- split_out$Y$Y_val
  if(is.null(dates_mix)){
    dates_mix <- as.Date("2013-03-27")+0:(length(Y_train)+length(Y_val)-1)
  }
  Y_tibble <- dplyr::tibble(
    date = dates_mix,
    value = c(Y_train, Y_val)
  )
  Y_tsibble <- fable::as_tsibble(Y_tibble)
  fit <- head(Y_tsibble, length(Y_val)) %>%
    fabletools::model(fable::AR(value ~ order(0:p_max)))
  fit_val <- fabletools::refit(fit, tail(Y_tsibble, length(Y_val)),
                                reestimate = FALSE)
  metrics_val <- fabletools::accuracy(fit_val)
  res_train <- residuals(fit)
  res_train <- res_train$.resid[complete.cases(res_train$.resid)]
  rmse_train <- sqrt(sum(res_train^2))
  if(!quiet) cat("\nModelo base","\n-----","\nRMSE train: ",rmse_train,
               "\nRMSE val: ",metrics_val$RMSE,"\n")
  return(list(model = fit, rmse_train = rmse_train, rmse_val = metrics_val$RMSE))
}

### 3) Funciones para guardar todos los modelos en una lista
#' Se recomienda realizar dos listas separadas. Una para cada tipo de modelo
#' Ojo, es necesario que cada objeto de la lista tenga un nombre que asocie el
#' delito correspondiente
#' Nota: Podriamos considerar usar una clase s3 o s4 para los resultados?
evaluate_models_city <- function(city, ...){
  tryCatch({get(city)},error = function(e) stop("Ciudad no encontrada",
                                                call. = FALSE))
  dots <- list(...)
  if(length(dots) == 0) stop("Ningun modelo fue pasado a la funcion")
  for(model in dots){
    print("Esta funcion queda pendiente pues su generalizacion requiere truco")
  }
}
custom_models_city <- function(city, crimes, ...){
  tryCatch({get(city)},error = function(e) stop("Ciudad no encontrada",
                                                call. = FALSE))
  p_max <- 12
  dates_mix <- NULL
  tunning_intercept <- TRUE
  split_prop <- c(0.9,0.05,0.05)
  dots <- list(...)
  if(length(dots) == 0) warning("Se usaran los argumentos por defecto")
  if(length(dots) != 0) list2env(dots)
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
    X <- date_split(X)
    Y <- dt_wrk$N
    X_aug <- add_laggs(X = X, Y = Y, p = p_max)
    split_base <- create_split_ts(Y = Y, X = X, split_prop = split_prop)
    split_aug <- create_split_ts(Y = tail(Y,-p_max), X = X_aug)
    fits <- list()
    fits$lin_1 <- lin_mod_selector(split_out = split_base,
                              tunning_intercept = tunning_intercept,
                              quiet = TRUE)
    fits$lin_2 <- lin_mod_selector(split_out = split_aug,
                              tunning_intercept = tunning_intercept,
                              quiet = TRUE)
    fits$ar <- ar_mod_selector(split_out = split_base, p_max = p_max,
                             dates_mix = dates_mix, quiet = TRUE)
    rmse <- sapply(fits, function(x) x[["rmse_val"]])
    all_fits[[to_seek]] <- fits[[which.min(rmse)]]
  }
  if(length(all_fits) == 0) stop("No se encontro base de datos asociada",
                                 call. = FALSE)
  return(all_fits)
}
