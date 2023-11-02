for(delito in names(model_list_3)){
  fit <- model_list_3[[delito]]
  if(sum(grepl("?(keras)", class(fit))) != 0){
    keras::save_model_hdf5(fit, paste0(PATH,"/",delito,"_reg_lstm_v3.h5"))
  }
}
# Lecturas
metric <- keras::custom_metric("root_mean_squared_error", function(y_true, y_pred) {
  keras::k_sqrt(keras::k_mean(keras::k_square(y_pred - y_true)))
})
PATH <- "D:/R_projects/models_lstm_v3"
delitos <- gsub("_reg_lstm_v3.h5","",list.files(PATH))
for(i in delitos){
  #if(length(class(bogota_lstm[[i]]))>1)
  bogota_lstm[[i]] <-
      keras::load_model_hdf5(filepath = paste0(PATH,"/",i,"_reg_lstm_v3.h5"))
  #                           custom_objects = reticulate::dict(root_mean_squared_error = metric))
}
### Rendimiento de LSTM - Modelo 5
train_list <- list()
val_list <- list()
for (delito in delitos) {
  serie_completa <- test2021::siedco_wc[Delito == delito,
                                        .(N = sum(`Numero Hechos`)),
                                        .(CODIGO_DIV1,FECHA)]
  serie_completa <- serie_completa %>%
    tidyr::pivot_wider(id_cols = FECHA, names_from = c(CODIGO_DIV1),
                       values_from = N) %>%
    dplyr::arrange(FECHA) %>%
    replace(is.na(.),0)
  # Model inputs
  laged_vals  <- 208
  #
  sizes <- floor(c(nrow(serie_completa)*0.7,nrow(serie_completa)*0.85))
  train <- serie_completa[1:sizes[1],-1]
  val <- serie_completa[c((sizes[1]-laged_vals+1):(sizes[2])),-1]
  # escalas
  scales <- matrix(c(apply(train,2,min),apply(train,2,max)),byrow = FALSE,ncol=2)
  # transf
  train_sc <- lapply(1:ncol(train),
                     function(x) 2*(train[,x]-scales[x,1])/(scales[x,2]-scales[x,1])-1) %>%
    dplyr::bind_cols()
  val_sc <- lapply(1:ncol(train),
                   function(x) 2*(val[,x]-scales[x,1])/(scales[x,2]-scales[x,1])-1) %>%
    dplyr::bind_cols()
  train_tf <- lstm_reshape_2(as.matrix(train_sc),laged_vals)
  val_tf <- lstm_reshape_2(as.matrix(val_sc),laged_vals)
  model <- bogota_lstm[[delito]]
  pred_train <- model %>%
    predict(train_tf$X)
  tlist <- lapply(1:ncol(train_tf$y),function(x) 0.5*(train_tf$y[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1])
  tlist <- dplyr::bind_cols(tlist)
  plist <- lapply(1:ncol(pred_train),function(x){
    y <- 0.5*(pred_train[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1]
    for(i in 1:length(y)){
      y[i] <- max(y[i],0)
    }
    y
  })
  plist <- dplyr::bind_cols(plist)
  train_list[[delito]] <- c(sqrt(mean(as.matrix((plist-tlist)^2))),
                            sd(unlist(tlist)), mean(unlist(tlist)),
                            100*sd(unlist(tlist))/mean(unlist(tlist)))
  pred_val <- model %>%
    predict(val_tf$X)
  tlist <- lapply(1:ncol(train_tf$y),function(x) 0.5*(val_tf$y[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1])
  tlist <- dplyr::bind_cols(tlist)
  plist <- lapply(1:ncol(pred_val),function(x){
    y <- 0.5*(pred_val[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1]
    for(i in 1:length(y)){
      y[i] <- max(y[i],0)
    }
    y
  })
  plist <- dplyr::bind_cols(plist)
  val_list[[delito]] <- c(sqrt(mean(as.matrix((plist-tlist)^2))),
                          sd(unlist(tlist)), mean(unlist(tlist)),
                          100*sd(unlist(tlist))/mean(unlist(tlist)))
}
table <- cbind(do.call(rbind,train_list),do.call(rbind,val_list))
#### Modelo 3
train_list <- list()
val_list <- list()
for (delito in delitos) {
  serie_completa <- test2021::siedco_wc[Delito == delito,
                                        .(N = sum(`Numero Hechos`)),
                                        .(CODIGO_DIV1,FECHA)]
  serie_completa <- serie_completa %>%
    tidyr::pivot_wider(id_cols = FECHA, names_from = c(CODIGO_DIV1),
                       values_from = N) %>%
    dplyr::arrange(FECHA) %>%
    replace(is.na(.),0)
  # Model inputs
  laged_vals  <- 208
  #
  sizes <- floor(c(nrow(serie_completa)*0.7,nrow(serie_completa)*0.85))
  train <- serie_completa[1:sizes[1],-1]
  val <- serie_completa[c((sizes[1]-laged_vals):(sizes[2])),-1] # Se quita el +1 por diff
  # diff
  train_diff <- apply(train,2,diff)
  val_diff <- apply(val,2,diff)
  # escalas
  scales <- matrix(c(apply(train_diff,2,min),apply(train_diff,2,max)),byrow = FALSE,ncol=2)
  # transf
  train_sc <- lapply(1:ncol(train_diff),
                     function(x) 2*(train_diff[,x]-scales[x,1])/(scales[x,2]-scales[x,1])-1) %>%
    dplyr::bind_cols()
  val_sc <- lapply(1:ncol(train_diff),
                   function(x) 2*(val_diff[,x]-scales[x,1])/(scales[x,2]-scales[x,1])-1) %>%
    dplyr::bind_cols()
  train_tf <- lstm_reshape_2(as.matrix(train_sc),laged_vals)
  val_tf <- lstm_reshape_2(as.matrix(val_sc),laged_vals)
  model <- bogota_lstm[[delito]]
  pred_train <- model %>%
    predict(train_tf$X)
  tlist <- lapply(1:ncol(train_tf$y),function(x) 0.5*(train_tf$y[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1])
  tlist <- lapply(1:length(tlist),function(x) cumsum(tlist[[x]])+
                    (colSums(train_diff[1:(laged_vals),])+as.numeric(train[1,]))[x])
  tlist <- dplyr::bind_cols(tlist)
  plist <- lapply(1:ncol(pred_train),function(x){
    y <- 0.5*(pred_train[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1]
    for(i in 1:length(y)){
      y[i] <- max(y[i],0)
    }
    y
  })
  plist <- lapply(1:length(plist),function(x) plist[[x]] +
                    cumsum(train_diff[c(laged_vals:(nrow(train_diff)-1)),x]) +
                    (colSums(train_diff[1:(laged_vals-1),])+as.numeric(train[1,]))[x])
  plist <- dplyr::bind_cols(plist)
  train_list[[delito]] <- c(sqrt(mean(as.matrix((plist-tlist)^2))),
                            sd(unlist(tlist)), mean(unlist(tlist)),
                            100*sd(unlist(tlist))/mean(unlist(tlist)))
  pred_val <- model %>%
    predict(val_tf$X)
  tlist <- lapply(1:ncol(train_tf$y),function(x) 0.5*(val_tf$y[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1])
  tlist <- lapply(1:length(tlist),function(x) cumsum(tlist[[x]])+
                    (colSums(val_diff[1:(laged_vals),])+as.numeric(val[1,]))[x])
  tlist <- dplyr::bind_cols(tlist)
  plist <- lapply(1:ncol(pred_val),function(x){
    y <- 0.5*(pred_val[,x]+1)*(scales[x,2]-scales[x,1])+scales[x,1]
    for(i in 1:length(y)){
      y[i] <- max(y[i],0)
    }
    y
  })
  plist <- lapply(1:length(plist),function(x) plist[[x]] +
                    cumsum(val_diff[c(laged_vals:(nrow(val_diff)-1)),x]) +
                    (colSums(val_diff[1:(laged_vals-1),])+as.numeric(val[1,]))[x])
  plist <- dplyr::bind_cols(plist)
  val_list[[delito]] <- c(sqrt(mean(as.matrix((plist-tlist)^2))),
                          sd(unlist(tlist)), mean(unlist(tlist)),
                          100*sd(unlist(tlist))/mean(unlist(tlist)))
}
table <- cbind(do.call(rbind,train_list),do.call(rbind,val_list))
