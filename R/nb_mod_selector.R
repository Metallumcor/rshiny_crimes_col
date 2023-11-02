#' @encoding UTF-8
#' @title Ajuste de clasificador probabilistico bayesiano (Naïve Bayes)
#'
#' @description Mediante la rutina \code{naiveBayes} se obtienen las
#'     probabilidades posteriores de cada clase en función de los predictores
#'     independientes suministrados. Se asume gaussianidad para las variables
#'     continuas. En la salida se reportan las **accuracy** del árbol simplificado
#'     en entrenamiento y validación.
#'
#'     *Nota* en versiones futuras se incluirá ajuste por CV
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
#' @param ... argumentos adicionales (en desarrollo)
#'
#' @return Una lista que incluye el mejor clasificador probabilistico
#'     junto a la **accuracy** reportada en entrenamiento y validación.
#' @export
#' @importFrom e1071 naiveBayes
nb_mod_selector <- function(split_out,
                            scaled = FALSE,
                            quiet = FALSE,
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
  nb_fit <- e1071::naiveBayes(CLASS~.,Z_train,...)
  acc_train <- clsf_metrics(nb_fit, split_out = split_out,
                            objective = "train",
                            metrics_out = "accuracy")
  acc_val <- clsf_metrics(nb_fit, split_out = split_out,
                          objective = "validation",
                          metrics_out = "accuracy")
  return(list(fit = nb_fit, acc_train = acc_train, acc_val = acc_val))
}
