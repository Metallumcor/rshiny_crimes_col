#' @encoding UTF-8
#' @title Modelos de clasificación - Mapa de datos
#'
#' @description Prototipo de función principal que integra parte de la pipeline
#'     de modelos de clasificación: selección de modelo, ajuste de modelo y
#'     reporte de métricas principales. Actualmente la función es capaz de
#'     procesar los siguientes modelos con sus respectivas especificaciones:
#'     \describe{
#'        \item{Multinomial:}{Usando \code{nnet} sin o con una capa oculta}
#'        \item{Naive Baye:}{Con \code{e1071}}
#'        \item{SVM}{Con \code{e1071}. Acepta tuning}
#'        \item{Decision tree}{Con \code{rpart} y pruning automaticó}
#'        \item{Radom Forest}{Mediante \code{randomForest}}
#'        \item{NN:}{Redes neuronales con \code{keras} de una o dos capas ocultas}
#'     }
#'     Las métricas reportadas son las descritas en \code{clsf_metrics}
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param models El listado de modelos a evaluar/retornar
#' @param only_best Lógico. Retornar solamente el modelo con la mayor
#'     accuracy en validación
#' @param save_obj Lógico. Guardar el objeto retornado en disco duro. Se
#'     seleccionado el directorio de trabajo por defecto
#' @param ... argumentos adicionales pasados a la función principal o a los modelos
#'
#' @return Una lista que contiene los modelos ajustados y las metrícas por modelo
#' @export
clsf_models <- function(split_out,
                        models = c("multinom_nnet", "multinom_1hl_nnet",
                                   "naive_bayes", "svm", "decision_tree",
                                   "random_forest", "nn_1hl", "nn_2hl"),
                        only_best = FALSE,
                        save_obj = FALSE,
                        ...){
  # Listado posibles modelos
  all_mod <- c("multinom_nnet", "multinom_1hl_nnet", "naive_bayes", "svm",
               "decision_tree", "random_forest", "nn_1hl", "nn_2hl")
  # Logico - prueba de seleccion de modelos
  if(prod(models %in% all_mod)!=1) stop("Algunos modelos no son reconocidos")
  ### Cuerpo de la funcion
  # Parametros fijos de las funciones
  size_multi <- ncol(split_out$X$X_train)
  tuning_svm <- TRUE
  gamma_svm <- 10^(-5:4)
  cost_svm <- 10^(-5:4)
  tuning_rf <- TRUE
  scaled <- TRUE
  save_location <- getwd()
  file_name <- "clsf_results"
  # Dots
  dots <- list(...)
  list2env(dots, envir = environment())
  # Check de file existente
  if(save_obj){
    f <- paste0(save_location,"//",file_name,".rds")
    if(file.exists(f)) stop("El archivo de guardado ya existe en la localizacion. Elegir otro nombre o eliminar existente")
  }
  # Funcion auxiliar metricas
  melt_list <- function(x){
    r_names <- names(x)
    keys <- unique(unlist(lapply(x, names)))
    keys <- keys[!keys %in% "confusion_matrix"]
    o_list <- do.call(mapply, c(FUN=rbind, lapply(x, `[`, keys)))
    o_list <- lapply(o_list, as.data.frame)
    o_list <- lapply(o_list, function(y){rownames(y)<-r_names;y})
    setNames(o_list, keys)
    o_list
  }
  # Modelos
  model_list <- list()
  if("multinom_nnet" %in% models){
    model_list$multinom <- multinom_mod_selector(split_out = split_out,
                                                 scaled = scaled,
                                                 quiet = TRUE,
                                                 size = 0, ...)
  }
  if("multinom_1hl_nnet" %in% models){
    model_list$multinom_1hl <- multinom_mod_selector(split_out = split_out,
                                                     scaled = scaled,
                                                     quiet = TRUE,
                                                     size = size_multi,
                                                     ...)
  }
  if("naive_bayes" %in% models){
    model_list$naive_bayes <- nb_mod_selector(split_out = split_out,
                                              scaled = scaled,
                                              quiet = TRUE,
                                              ...)
  }
  if("svm" %in% models){
    model_list$svm <- svm_mod_selector(split_out = split_out,
                                       scaled = scaled,
                                       quiet = TRUE,
                                       tuning = tuning_svm,
                                       gamma = gamma_svm,
                                       cost = cost_svm,
                                       ...)
  }
  if("decision_tree" %in% models){
    model_list$decision_tree <- tree_mod_selector(split_out = split_out,
                                                  scaled = scaled,
                                                  quiet = TRUE,
                                                  auto_prune = TRUE,
                                                  ...)
  }
  if("random_forest" %in% models){
    model_list$random_forest <- rf_mod_selector(split_out = split_out,
                                                scaled = scaled,
                                                quiet = TRUE,
                                                tuning = tuning_rf,
                                                ...)
  }
  if("nn_1hl" %in% models){
    model_list$nn_1hl <- nn_1hl(split_out = split_out,...)
  }
  if("nn_2hl" %in% models){
    model_list$nn_2hl <- nn_2hl(split_out = split_out,...)
  }
  # Pasar los modelos al listado
  models <- lapply(model_list, function(x) x$fit)
  names(models) <- names(model_list)
  # Pasar las accuracy a una sola matriz
  acc_train <- unlist(lapply(model_list, function(x) x$acc_train))
  acc_val <- unlist(lapply(model_list, function(x) x$acc_val))
  acc <- data.frame(train = acc_train, val = acc_val)
  rownames(acc) <- gsub(".accuracy","",rownames(acc))
  # Metricas
  train_met <-
    lapply(model_list, function(x) clsf_metrics(x$fit, split_out, objective = "train"))
  val_met <-
    lapply(model_list, function(x) clsf_metrics(x$fit, split_out))
  train_met <- melt_list(train_met)
  val_met <- melt_list(val_met)
  # Retorno
  if(!only_best){
    clsf_result <- list(models = models, accuracy = acc,
                        train_metrics = train_met, val_metrics = val_met)
    f <- paste0(save_location,"//",file_name,".rds")
    if(save_obj) saveRDS(clsf_result,file = f)
    return(clsf_result)
  }
  # Encontrar el que max acc en val, pero tambien en train
  max_val <- which(acc$val == max(acc$val))
  best_mod <- models[[max_val[which.max(acc[max_val,"train"])]]]
  f <- paste0(save_location,"//",file_name,".rds")
  if(save_obj) saveRDS(best_mod,file = f)
  return(best_mod)
}
