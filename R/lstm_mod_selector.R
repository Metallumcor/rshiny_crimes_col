#### VERSIONES DE PRUEBAS
lstm_reshape <- function(X,y = NULL,steps){
  if(is.null(y)){
    Xs <- array(NA, dim = c(nrow(X)-steps,steps,ncol(X)))
    ys <- array(NA, dim = c(nrow(X)-steps,steps))
    for(i in 1:(nrow(X)-steps)){
      Xs[i,,] <- array(as.numeric(unlist(X[i:(i+steps),])),
                       dim = c(1,steps,ncol(X)))
      ys[,i] <- y[i:(i+steps),]
    }
  } else {
    if(any(class(X) %in% c("array","matrix"))){
      if(length(dim(X))>2) stop("Sequ debe tener dim 1 o 2")
      Xs <- array(NA, dim = c(dim(X)[1]-steps,steps,dim(X)[2]))
      ys <- array(NA, dim = c(dim(X)[1]-steps,dim(X)[2]))
      for(i in 1:length(X)){
        # find the end of seq
        end_ix <- i + steps - 1
        # check if seq exced the seq
        if (end_ix+1>dim(sequ)[1]) break
        Xs[i,,] <- X[i:end_ix,]
        ys[i,] <- X[end_ix+1,]
      }
    }else{
      if(!is.numeric(X)) stop("Seq debe ser array, matrix, o vector numerico")
      Xs <- array(NA, dim = c(length(X)-steps,steps,1))
      ys <- numeric()
      for(i in 1:length(X)){
        # find the end of seq
        end_ix <- i + steps - 1
        # check if seq exced the seq
        if (end_ix+1>length(sequ)) break
        Xs[i,,] <- X[i:end_ix]
        ys[i] <- X[end_ix+1]
      }
    }
  }
  return(list(X=Xs,y=ys))
}

split_tswindow <- function(series,
                           split_prop = c(0.9,0.05,0.05),
                           window = 0){
  if (sum(split_prop) != 1)
    stop("La suma de las proporciones debe ser 1")
  if (!any(class(series) %in% c("numeric","array","matrix")))
    stop("La serie debe ser numerica")
  #
  len <- ifelse(is.numeric(series),length(series),nrow(series))
  index_split <- cumsum(ceiling(len*split_prop))
  #
  if((index_split[1]+1-window) < 0)
    stop("La ventana es mas grande que el tamagno de train")
  #
  if(is.numeric(series))
    Y_out <- list(Y_train = Y[1:index_split[1]],
                  Y_val = Y[(index_split[1]+1-window):index_split[2]],
                  Y_test = Y[(index_split[2]+1-window):len])
  else
    Y_out <- list(Y_train = Y[1:index_split[1],],
                  Y_val = Y[(index_split[1]+1-window):index_split[2],],
                  Y_test = Y[(index_split[2]+1-window):len,])

  return(Y_out)
}

## scaler
scaler_transform <- function(x,
                             transformation = c("normalization",
                                                "scaling",
                                                "tanh")){
  #
  transformation <- match.arg(transformation)
  #
  if(transformation == "normalization"){
    params <- matrix(c(colMeans(x), apply(x,2,sd)), byrow = FALSE, ncol=2)
    x_out <- apply(1:ncol(x),2,function(t) (t-params[x,1])/params[x,2])
  } else {
    params <- matrix(c(apply(x,2,min), apply(x,2,max)), byrow = FALSE, ncol=2)
    x_out <- apply(1:ncol(x),2,function(t) (t-params[x,1])/(params[x,2]-params[x,1]))
    if(transformation == "tanh")
      x_out <- apply(1:ncol(x),2,function(t) 2*t-1)
  }
  #
  return(list(x_out = x_out, params = params, cl = transformation))
}

## inverse scaler
scaler_inverse <- function(x_out, params = NULL, cl = NULL){
  #
  if(is.null(params)) params <- x_out$params
  if(is.null(cl)) cl <- x_out$cl
  #
  if(transformation == "normalization"){
    params <- matrix(c(colMeans(x), apply(x,2,sd)), byrow = FALSE, ncol=2)
    x_out <- apply(1:ncol(x),2,function(t) (t-params[x,1])/params[x,2])
  } else {
    params <- matrix(c(apply(x,2,min), apply(x,2,max)), byrow = FALSE, ncol=2)
    x_out <- apply(1:ncol(x),2,function(t) (t-params[x,1])/(params[x,2]-params[x,1]))
    if(transformation == "tanh")
      x_out <- apply(1:ncol(x),2,function(t) 2*t-1)
  }
  #
  return(x_out)
}

