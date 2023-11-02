#' @encoding UTF-8
#' @title Ajuste de arboles de decisión para clasificación.
#'
#' @description Se utilizan las rutinas de particiones recursivas \code{rpart}
#'     del paquete homónimo para el **crecimiento** de arboles de clasificación
#'     binarios o múltiples. Cada árbol es **podado** para evitar el
#'     sobre-ajuste que sucede naturalmente en este tipo de clasificadores.
#'     En la salida se reportan las **accuracy** del árbol simplificado
#'     en entrenamiento y validación.
#'
#'     *Nota* en versiones futuras se incluirá ajuste por CV para el
#'     pesos de los parámetros (usando \code{caret}, por ejemplo)
#'
#'     TODO: Ejemplos
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param scaled ¿Los datos están escalados? De usar falso se realizará
#'     su escalamiento. *Nota:* El escalamiento se realiza usando la
#'     información disponible en entrenamiento.
#' @param quiet Lógico. Callar las salidas en consola de los modelos.
#' @param auto_prune Lógico. Realizar selección automática del parámetro de
#'     complejidad para el podado.
#' @param controls Nulo o una función \code{rpart.control} con argumentos.
#'     Consultar \code{\link[rpart]{rpart}} y \code{\link[rpart]{rpart.control}}.
#' @param ... Argumentos adicionales pasados a \code{rpart} y otras funciones.
#'     Uno de los casos especifico es el parámetro \code{cp} cuando
#'     \code{auto_prune} es falso.
#'
#' @return Una lista que incluye el mejor árbol de decisión junto a la
#'     **accuracy** reportada en entrenamiento y validación.
#' @export
#' @importFrom rpart rpart
#' @importFrom rpart prune
tree_mod_selector <- function(split_out,
                                scaled = FALSE,
                                quiet = FALSE,
                                auto_prune = TRUE,
                                controls = NULL,
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
  # Modelos sin/con opciones de control
  if(is.null(controls)){
    tree_fit <- rpart::rpart(CLASS~.,Z_train, method = "class")
  } else {
    tree_fit <- rpart::rpart(CLASS~.,Z_train, method = "class", control = controls)
  }
  # Pruning
  if(auto_prune){
    tree_fit <-
      rpart::prune(tree_fit, cp = tree_fit$cptable[which.min(tree_fit$cptable[,"xerror"]),"CP"])
  } else {
    if(!exists("cp")) stop("Para realizar prune manual se requiere la variable cp")
    tree_fit <- rpart::prune(tree_fit, cp = cp)
  }
  acc_train <- clsf_metrics(tree_fit, split_out = split_out,
                            objective = "train",
                            metrics_out = "accuracy")
  acc_val <- clsf_metrics(tree_fit, split_out = split_out,
                          objective = "validation",
                          metrics_out = "accuracy")
  return(list(fit = tree_fit, acc_train = acc_train, acc_val = acc_val))
}
