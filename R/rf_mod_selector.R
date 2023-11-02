#' @encoding UTF-8
#' @title Ajuste ensambles para clasificación - Random Forest.
#'
#' @description Mediante la rutina \code{randomForest} de su paquete homónimo
#'     se realiza ajuste del ensamble con sus parámetros por defecto, aunque
#'     es posible que estos sean modificados mediante el ingreso de los
#'     argumentos necesarios.
#'
#'     Usando \code{tuneRF} se produce de forma alternativa un mejor modelo
#'     donde se explora la cantidad de variables incluidas en los modelos
#'     base del ensamble, de acuerdo al error fuera de muestra del bootstrap
#'     (OOB)
#'
#'     El objeto resultante contiene el ajuste de \code{randomForest}, más
#'     las **accuracy** del ensamble en entrenamiento y validación.
#'
#'     *Nota* en versiones futuras se incluirá ajuste por CV para otros
#'     parámetros (usando \code{caret}, por ejemplo)
#'
#'     TODO: Ejemplos
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param scaled ¿Los datos están escalados? De usar falso se realizará
#'     su escalamiento. *Nota:* El escalamiento se realiza usando la
#'     información disponible en entrenamiento.
#' @param quiet Lógico. Callar las salidas en consola de los modelos.
#' @param tuning Lógico. Retornar o no el mejor modelo posible.
#' @param ... argumentos adicionales pasados a \code{randomForest} o
#'     \code{tuneRF}. Consultar la función \code{\link[randomForest]{randomForest}}
#'     para mayor información.
#'
#' @return Una lista que incluye el mejor ensamble Random Forest,
#'     junto a la **accuracy** reportada en entrenamiento y validación
#' @export
#' @importFrom randomForest randomForest
#' @importFrom randomForest tuneRF
rf_mod_selector <- function(split_out,
                              scaled = FALSE,
                              quiet = FALSE,
                              tuning = TRUE,
                              ...){
  # Escalamiento
  if(!scaled){
    num_cols <- unlist(lapply(split_out$X$X_train,is.numeric))
    if(length(num_cols)==0) next
    sc_num <- lapply(split_out$X$X_train[num_cols], scale)
    sc_center <- lapply(sc_num, function(x) attr(x, "scaled:center"))
    sc_scale <- lapply(sc_num, function(x) attr(x, "scaled:scale"))
    split_out$X$X_train[num_cols] <- lapply(sc_num, as.numeric)
    split_out$X$X_val[num_cols] <- Map(function(x,y,z){(x-y)/z},
                                       x = split_out$X$X_val[num_cols],
                                       y = sc_center,
                                       z = sc_scale)
  }
  # Ajuste del modelo
  if(!tuning){
    rf_fit <-
      randomForest::randomForest(x = split_out$X$X_train, y = split_out$Y$Y_train,
                                 ...)
  } else {
    rf_fit <-
      randomForest::tuneRF(x = split_out$X$X_train, y = split_out$Y$Y_train,
                           doBest = TRUE, plot = FALSE, ...)
  }
  if(!is.numeric(split_out$Y$Y_train)){
    acc_train <- clsf_metrics(rf_fit, split_out = split_out,
                              objective = "train",
                              metrics_out = "accuracy")
    acc_val <- clsf_metrics(rf_fit, split_out = split_out,
                            objective = "validation",
                            metrics_out = "accuracy")
    rf_fit <- redx_rf(rf_fit)
    return(list(fit = rf_fit, acc_train = acc_train, acc_val = acc_val))
  } else {
    rmse_train <- reg_metrics(rf_fit, split_out = split_out,
                              objective = "train",
                              metrics_out = "rmse")
    rmse_val <- reg_metrics(model, split_out = split_out,
                            objective = "validation",
                            metrics_out = "rmse")
    rf_fit <- redx_rf(rf_fit)
    return(list(fit = rf_fit, rmse_train = rmse_train, rmse_val = rmse_val))
  }
}
