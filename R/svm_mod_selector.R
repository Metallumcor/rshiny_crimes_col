#' @encoding UTF-8
#' @title Ajuste de maquinas de vectores de soporte para clasificación.
#'
#' @description Se usa la rutina \code{svm} de la librería \code{e1071} para
#'     crear un clasificador o múltiples, según la naturaleza de la respuesta.
#'     En caso de múltiples categorías se usa el método uno contra todas con
#'     selección de predicción mediante votación. Se reporta la **accuracy**
#'     en entrenamiento y validación.
#'
#'     Dado que esto es un modelo de clasificación, la respuesta esperada
#'     debe ser un \code{factor}. Además, por como funciona la rutina
#'     \code{svm} las columnas numéricas deben estar escaladas
#'     para asegurar rápida convergencia.
#'
#'     *Nota* en versiones futuras se incluirá ajuste por CV para el
#'     pesos de los parámetros (usando \code{caret}, por ejemplo)
#'
#'     TODO: Ejemplos.
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param scaled ¿Los datos están escalados? De usar falso se realizará
#'     su escalamiento internamente en \code{svm}.
#' @param quiet Lógico. Callar las salidas en consola de los modelos.
#' @param tuning Lógico. Retornar o no el mejor modelo posible.
#' @param ... argumentos adicionales pasados a \code{svm} o \code{best.svm}
#'     consultar la función \code{\link[e1071]{svm}} para mayor información.
#'
#' @return Una lista que incluye la mejor SVM (o mejores en multi-clases)
#'     junto a la **accuracy** reportada en entrenamiento y validación
#' @export
#' @importFrom e1071 svm
#' @importFrom e1071 best.svm
svm_mod_selector <- function(split_out,
                             scaled = FALSE,
                             quiet = FALSE,
                             tuning = FALSE,
                             ...){
  # Ajuste del modelo
  Z_train <- data.frame(CLASS = split_out$Y$Y_train,split_out$X$X_train)
  # Modelos sin/con search
  if(!tuning){
    svm_fit <- e1071::svm(CLASS~.,Z_train, probability = TRUE, scale = scaled)
  } else {
    svm_fit <- e1071::best.svm(CLASS~., data = Z_train, scale = scaled,
                               probability = TRUE,...)
  }
  acc_train <- clsf_metrics(svm_fit, split_out = split_out,
                            objective = "train",
                            metrics_out = "accuracy")
  acc_val <- clsf_metrics(svm_fit, split_out = split_out,
                          objective = "validation",
                          metrics_out = "accuracy")
  return(list(fit = svm_fit, acc_train = acc_train, acc_val = acc_val))
}
