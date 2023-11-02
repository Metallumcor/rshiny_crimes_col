#' @encoding UTF-8
#' @title Ajuste de modelos multinomiales log-lineales con RN
#'
#' @description Modelos log-lineales multinomiales ajustados usando redes
#'     neuronales con la rutina \code{multinomial} y \code{nnet}
#'     del paquete \code{nnet}. Se reporta la **accuracy** en entrenamiento
#'     y validación.
#'
#'     Dado que esto es un modelo de clasificación, la respuesta esperada
#'     debe ser un \code{factor}. Además, por como funciona las redes
#'     neuronales de \code{nnet} las columnas numéricas deben estar escaladas
#'     para asegurar rápida convergencia.
#'
#'     Para seleccionar el número de nodos en la red neuronal se recomienda
#'     elegir una cantidad entre el número de *inputs* y el de *outputs*.
#'
#'     *Nota* en versiones futuras se incluirá ajuste por CV para el
#'     pesos de los parámetros y el número de nodos
#'     (usando \code{caret}, por ejemplo)
#'
#'     TODO: Ejemplos
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param scaled ¿Los datos están escalados? De usar falso se realizará
#'     su escalamiento. *Nota:* El escalamiento se realiza usando la
#'     información disponible en entrenamiento.
#' @param quiet Lógico. Callar las salidas en consola de los modelos.
#' @param size Entero positivo. Determina el número de nodos de la capa oculta.
#'     Elegir cero para usar una NN sin capa oculta.
#' @param ... argumentos adicionales pasados a \code{multinom} o \code{nnet}
#'
#' @return Una lista que incluye el mejor modelo multinomial junto a la
#'     **accuracy** reportada en entrenamiento y validación.
#' @export
#' @importFrom nnet multinom
#' @importFrom nnet nnet
multinom_mod_selector <- function(split_out,
                                  scaled = FALSE,
                                  quiet = FALSE,
                                  size = 0,
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
  Z_train <- data.frame(CLASS = split_out$Y$Y_train,split_out$X$X_train)
  # Posible particion en otra funcion que implemente grid search para pesos
  # Uso de caret/tidymodels
  if(size == 0) multinom_fit <- nnet::multinom(CLASS~.,Z_train, ...)
  if(size != 0) multinom_fit <- nnet::nnet(CLASS~.,Z_train, size = size, ...)
  acc_train <- clsf_metrics(multinom_fit, split_out = split_out,
                            objective = "train",
                            metrics_out = "accuracy")
  acc_val <- clsf_metrics(multinom_fit, split_out = split_out,
                            objective = "validation",
                            metrics_out = "accuracy")
  return(list(fit = multinom_fit, acc_train = acc_train, acc_val = acc_val))
}
