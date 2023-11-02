#' @encoding UTF-8
#' @title Ajuste de un modelo de regresión múltiple con validación por RMSE.
#'
#' @description Modelo personalizado que compara un ajuste de un modelo
#'     contra uno reducido, seleccionado por procedimientos automáticos.
#'     Se entregan los RMSE de entrenamiento y validación, junto al
#'     modelo seleccionado.
#'
#'     No se realizan múltiples pases de validación.
#'
#'     TODO: Ejemplos y múltiples pases de validación.
#'     TODO: Es necesario corregir la asignacion global de train...
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#' @param step_selection ¿Se debe probar subconjuntos de variables?
#' @param p_selection ¿Se debe eliminar las variables con p-valores no
#'     significativos?
#' @param quiet Callar las salidas en consola de los modelos.
#'
#' @return Una lista que incluye el mejor modelo y los RMSE en entrenamiento
#'     y validación (un pase)
#' @export
#' @importFrom stats lm
#' @importFrom stats step
lin_mod_selector <- function(split_out, step_selection = TRUE,
                             p_selection = TRUE,
                             quiet = FALSE){
  X_train <- split_out$X$X_train
  X_val <- split_out$X$X_val
  Y_val <- split_out$Y$Y_val
  Y_train <- split_out$Y$Y_train
  .train <<- cbind(data.frame(Y_train = Y_train), X_train)
  base_model <- stats::lm(Y_train ~ ., data = .train)
  if(p_selection){
    base_model <- model_select(base_model)
  }
  rmse_base_val <- linmod_metrics(model = base_model, split_output = split_out)
  rmse_base_train <- sqrt(mean(residuals(base_model)^2))
  if(!quiet) cat("\nModelo completo","\n-----","\nRMSE train: ",rmse_base_train,
                 "\nRMSE val: ",rmse_base_val,"\n")
  if (step_selection){
    step_model <- stats::step(base_model,trace = 0)
    if(p_selection){
      step_model <- model_select(step_model)
    }
    rmse_step_train <- sqrt(mean(residuals(step_model)^2))
    rmse_step_val <- linmod_metrics(model = step_model, split_output = split_out)
    if(!quiet) cat("\nModelo reducido","\n-----","\nRMSE train: ",
                   rmse_step_train,"\nRMSE val: ",rmse_step_val)
    if (rmse_base_val > rmse_step_val){
      if(!quiet) print(summary(step_model))
      return(list(selected_model = step_model, rmse_train = rmse_step_train,
                  rmse_val = rmse_step_val))
    }
  }
  if(!quiet) print(summary(base_model))
  return(list(selected_model = base_model, rmse_train = rmse_base_train,
              rmse_val = rmse_base_val))
}

# Credito https://stackoverflow.com/questions/3701170/stepwise-regression-using-p-values-to-drop-variables-with-nonsignificant-p-value
# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}
# Credito https://stackoverflow.com/questions/3701170/stepwise-regression-using-p-values-to-drop-variables-with-nonsignificant-p-value
# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after
model_select <- function(model,keep,sig=0.05,verbose=F){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }

  # Backward model selection :

  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test="F")

    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }

    pval <- test[,dim(test)[2]]

    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)

    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))

    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.

    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}
    } # end while(T) drop var

    if(pval[i]<sig) break # stops the loop if var to remove is significant

    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")
    }

    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)

    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}
