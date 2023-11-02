#' @encoding UTF-8
#' @title PENDIENTE
#'
#' @description PENDIENTE
#'
#'     TODO: Ejemplos
#'
#' @param split_out La partición entrenamiento/validación/prueba de los datos.
#'     Las columnas de factores deben estar en su correspondiente formato.
#' @param factor_split Carácter. Nombre de la columna que separa las variables
#'     endógenas
#' @param dates_col Carácter. Nombre de la columna que contiene las fechas.
#'     No requiere un formato determinado pero requiere que sean las mismas
#'     en todos los niveles de \code{factor_split}
#' @param season Entero. La frecuencia estacional (dummy).
#' @param xreg_cols Los nombres de las columnas que contienen las
#'     variables exógenas. No usado de momento.
#' @param auto_sel Lógico. Realizar selección automática del orden VAR. Por
#'     defecto se busca hasta p = 10. Configurar en \code{...} con las opciones
#'     \code{lag.max} (p máximo) y \code{ic} (criterio de información usado).
#' @param restriction Matriz usada para crear restricciones sobre el objecto
#'     estimado. En este caso se recomienda asignar p manualmente
#' @param quiet Callar las salidas en consola de los modelos.
#' @param experimental Lógico. Restricción usando correlaciones parciales
#' @param ... Argumentos adicionales
#'
#' @return Una lista que incluye los modelos lineales equivalentes que
#'     de la estimación del VAR (OLS). La cantidad de modelos de regresión
#'     es igual al total de variables endógenas declaradas. Se incluye además
#'     los RMSE totales en entrenamiento y validación.
#' @export
#' @importFrom tidyr pivot_wider
var_mod_selector <- function(split_out,
                             factor_split,
                             dates_col,
                             season = NULL,
                             xreg_cols = NULL,
                             auto_sel = TRUE,
                             restriction = NULL,
                             quiet = FALSE,
                             experimental = FALSE,
                             ...)
{
  # Constantes
  type = "both"
  p = 1
  if(auto_sel) lag.max = 10 else lag.max = NULL
  ic = "AIC"
  # Dots
  list2env(list(...),envir = environment())
  # Particion
  train_df <- data.frame(y = split_out$Y$Y_train, split_out$X$X_train) %>%
    tidyr::pivot_wider(id_cols = !!as.name(dates_col),
                       names_from = !!as.name(factor_split),
                       values_from = y)
  test_df <- data.frame(y = split_out$Y$Y_val, split_out$X$X_val) %>%
    tidyr::pivot_wider(id_cols = !!as.name(dates_col),
                       names_from = !!as.name(factor_split),
                       values_from = y)
  if(!is.null(xreg_cols)) stop("De momento no implementado") else exogen = NULL
  # Ajuste de modelo
  model <- VAR(y = train_df[,-1], p = p, type = type, season = season,
               exogen = NULL, lag.max = lag.max, ic = ic)
  if(!is.null(restriction))
    model <- restrict(model, method = "manual", resmat = restriction)
  if(is.null(restriction) & experimental){
    mat_pcor <- 1*pcor(model$y)$p.value < 0.05
    mat_pcor <- t(rep(1,model$p)) %x% mat_pcor
    if(any(c("const","trend") %in% colnames(model$datamat)))
      mat_pcor <- cbind(mat_pcor,model$datamat[,c("const","trend") %in% colnames(model$datamat)])
    model <- restrict(model, method = "manual", resmat = mat_pcor)
  }
  # RMSE
  rmse_train <- sqrt(mean(residuals(model)^2))
  pred_test <- predict.varest(model, test = test_df[,-1])
  rmse_val <- sqrt(mean((as.matrix(pred_test$fcst-test_df[,-1])^2)))
  # Check
  if(is.na(rmse_train) | is.na(rmse_val)) stop("Hubo un problema en la obtencion del VAR")
  # Retorno
  if(!quiet)
    cat("\nModelo base","\n-----","\nRMSE train: ",rmse_train,
        "\nRMSE val: ",rmse_val,"\n")
  list(fit = model, rmse_train = rmse_train, rmse_val = rmse_val)
}

#' @encoding UTF-8
#' @title Estimación de un modelo VAR(p)
#'
#' @description Objeto exportado del paquete \code{vars}.
#'
#' @param y Datos que contienen la información endógena
#' @param p Entero que indica el orden de rezago (1 por defecto)
#' @param type Tipo de regresión determinista a incluir
#' @param season Inclusión de variables dummy centradas estacionales (entero)
#' @param exogen Inclusión de variables exógenas
#' @param lag.max Entero, determina el orden máximo de rezago para selección
#'     automática
#' @param ic Carácter, selección del criterio de información. \code{lag.max}
#'     debe ser no nulo
#' @param experimental Pendiente
#'
#' @return Una lista con atributo de clase \code{varest} que contiene diferentes
#'     elementos. Consultar documentación de \code{vars} para ampliar
#' @export
VAR <- function (y, p = 1, type = c("const", "trend", "both", "none"),
          season = NULL,
          exogen = NULL, lag.max = NULL,
          ic = c("AIC","HQ", "SC", "FPE"),
          experimental = FALSE)
{
  y <- as.matrix(y)
  if (any(is.na(y)))
    stop("\nNAs in y.\n")
  if (ncol(y) < 2)
    stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
  if (is.null(colnames(y))) {
    colnames(y) <- paste("y", 1:ncol(y), sep = "")
    warning(paste("No column names supplied in y, using:",
                  paste(colnames(y), collapse = ", "), ", instead.\n"))
  }
  colnames(y) <- make.names(colnames(y))
  y.orig <- y
  type <- match.arg(type)
  obs <- dim(y)[1]
  K <- dim(y)[2]
  if (!is.null(lag.max)) {
    lag.max <- abs(as.integer(lag.max))
    ic <- paste(match.arg(ic), "(n)", sep = "")
    p <- VARselect(y, lag.max = lag.max, type = type, season = season,
                   exogen = exogen)$selection[ic]
  }
  sample <- obs - p
  ylags <- embed(y, dimension = p + 1)[, -(1:K)]
  temp1 <- NULL
  for (i in 1:p) {
    temp <- paste(colnames(y), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
  }
  colnames(ylags) <- temp1
  yend <- y[-c(1:p), ]
  if (type == "const") {
    rhs <- cbind(ylags, rep(1, sample))
    colnames(rhs) <- c(colnames(ylags), "const")
  }
  else if (type == "trend") {
    rhs <- cbind(ylags, seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "trend")
  }
  else if (type == "both") {
    rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "const", "trend")
  }
  else if (type == "none") {
    rhs <- ylags
    colnames(rhs) <- colnames(ylags)
  }
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < obs) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:obs, ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
    rhs <- cbind(rhs, dums[-c(1:p), ])
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen),
                                sep = "")
      warning(paste("No column names supplied in exogen, using:",
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    tmp <- colnames(rhs)
    rhs <- cbind(rhs, exogen[-c(1:p), ])
    colnames(rhs) <- c(tmp, colnames(exogen))
  }
  datamat <- as.data.frame(rhs)
  colnames(datamat) <- colnames(rhs)
  equation <- list()
  #### Experimental
  if(experimental){
    stop("Experimental isn't supported yet")
    if(!is.null(season) | !is.null(exogen)) stop("Experimental don't support seasonaliry nor exogen vars")
    for (i in 1:K) {
      mat_pcor <- pcor(y)$p.value < 0.05
      if(any(c("const", "both", "trend") %in% type)){
        datdet <- datamat[,-c("const","trend")]
      }
      datum <- 1*mat_pcor
      if(any(c("const","trend","both") %in% type)){
        datum <- cbind(datum,datdet)
      }
      y <- yend[, i]
      equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datacopy)
      if (any(c("const", "both") %in% type)) {
        attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
      }
    }
  } else {
  #### ORIGINAL POR ACA
    for (i in 1:K) {
      y <- yend[, i]
      equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamat)
      if (any(c("const", "both") %in% type)) {
        attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
      }
    } ### FIN ORIGINAL
  } ### FIN EXPERIMENTAL
  call <- match.call()
  if ("season" %in% names(call))
    call$season <- eval(season)
  result <- list(varresult = equation, datamat = data.frame(cbind(yend,
                                                                  rhs)), y = y.orig, type = type, p = p, K = K, obs = sample,
                 totobs = sample + p, restrictions = NULL, call = call)
  class(result) <- "varest"
  return(result)
}

#' @encoding UTF-8
#' @title Criterios de información y FPE para diferentes VAR(p)
#'
#' @description Objeto exportado del paquete \code{vars}.
#'
#' @param y Datos que contienen la información endógena
#' @param lag.max Entero, determina el orden máximo de rezago para selección
#'     automática
#' @param type Tipo de regresión determinista a incluir
#' @param season Inclusión de variables dummy centradas estacionales (entero)
#' @param exogen Inclusión de variables exógenas
#'
#' @return Una lista. Consultar detalles en \code{vars}
#' @export
VARselect <- function (y, lag.max = 10, type = c("const", "trend", "both",
                                    "none"), season = NULL, exogen = NULL)
{
  y <- as.matrix(y)
  if (any(is.na(y)))
    stop("\nNAs in y.\n")
  colnames(y) <- make.names(colnames(y))
  K <- ncol(y)
  lag.max <- abs(as.integer(lag.max))
  type <- match.arg(type)
  lag <- abs(as.integer(lag.max + 1))
  ylagged <- embed(y, lag)[, -c(1:K)]
  yendog <- y[-c(1:lag.max), ]
  sample <- nrow(ylagged)
  rhs <- switch(type, const = rep(1, sample), trend = seq(lag.max +
                                                            1, length = sample), both = cbind(rep(1, sample), seq(lag.max +
                                                                                                                    1, length = sample)), none = NULL)
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < sample) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:sample, ]
    rhs <- cbind(rhs, dums)
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen),
                                sep = "")
      warning(paste("No column names supplied in exogen, using:",
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    rhs <- cbind(rhs, exogen[-c(1:lag.max), ])
  }
  idx <- seq(K, K * lag.max, K)
  if (!is.null(rhs)) {
    detint <- ncol(as.matrix(rhs))
  }
  else {
    detint <- 0
  }
  criteria <- matrix(NA, nrow = 4, ncol = lag.max)
  rownames(criteria) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
  colnames(criteria) <- paste(seq(1:lag.max))
  for (i in 1:lag.max) {
    ys.lagged <- cbind(ylagged[, c(1:idx[i])], rhs)
    sampletot <- nrow(y)
    nstar <- ncol(ys.lagged)
    resids <- lm.fit(x = ys.lagged, y = yendog)$residuals
    sigma.det <- det(crossprod(resids)/sample)
    criteria[1, i] <- log(sigma.det) + (2/sample) * (i *
                                                       K^2 + K * detint)
    criteria[2, i] <- log(sigma.det) + (2 * log(log(sample))/sample) *
      (i * K^2 + K * detint)
    criteria[3, i] <- log(sigma.det) + (log(sample)/sample) *
      (i * K^2 + K * detint)
    criteria[4, i] <- ((sample + nstar)/(sample - nstar))^K *
      sigma.det
  }
  order <- apply(criteria, 1, which.min)
  return(list(selection = order, criteria = criteria))
}


#' @encoding UTF-8
#' @title Extracción de coeficientes de objeto VAR
#'
#' @description Objeto exportado del paquete \code{vars}
#'
#' @param x Un objeto de clase \code{varest}
#'
#' @return Matriz de coeficientes VAR
#' @export
Bcoef <- function (x)
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'var()'.\n")
  }
  y.names <- colnames(x$datamat[, c(1:x$K)])
  Z <- x$datamat[, -c(1:x$K)]
  B <- matrix(0, nrow = x$K, ncol = ncol(Z))
  if (is.null(x$restriction)) {
    for (i in 1:x$K) {
      B[i, ] <- coef(x$varresult[[i]])
    }
  }
  else if (!(is.null(x$restriction))) {
    for (i in 1:x$K) {
      restrictions <- x$restrictions
      restrictions[i, restrictions[i, ] == TRUE] <- coef(x$varresult[[i]])
      temp <- restrictions[i, ]
      B[i, ] <- temp
    }
  }
  colnames(B) <- colnames(Z)
  rownames(B) <- y.names
  return(B)
}

#' @encoding UTF-8
#' @title Método de predicción para objetos de clase varest (principalmente)
#'
#' @description Objeto exportado del paquete \code{vars}. Puede realizar
#'     pronósticos a objetos de clases \code{vars} y \code{vec2var} (no
#'     incluidos acá de momento). Su código fuente fue alterado para omitir
#'     la creación de intervalos de confianza, como a su vez su uso
#'     para obtener predicciones un paso adelante con información de prueba
#'     disponible.
#'
#' @param object Un objeto de clase \code{varest}
#' @param n.ahead Un entero especificando la cantidad de pasos adelante a
#'    predecir. No usado en one-step-ahead forecast
#' @param test Matriz de datos con las observaciones de prueba. \code{NULL}
#'    por defecto. De suministrarse se omite \code{n.ahead} y se realiza
#'    el procedimiento one-step-ahead forecast
#' @param dumvar Objeto que contiene las variables exógenas (de haber sido
#'    usadas en el ajuste VAR)
#' @return Una lista que contiene
#'    \describe{
#'        \item{fcst}{Una matriz cuyas filas representan los puntos futuros
#'        y las columnas las variables endógenas pronosticadas}
#'        \item{endog}{Matriz con la muestra - solo variables endógenas}
#'        \item{model}{El objeto VAR estimado}
#'        \item{exo.fcst}{Los valores pronosticados de las variables exónegas.
#'        Si no se suministraron devuelve \code{NULL}. De momento no se puede
#'        usar para one-step-ahead forecast}
#'    }
#' @param ... Argumentos adicionales. No usados
#'
#' @export
predict.varest <- function (object, ..., n.ahead = 10, test = NULL, dumvar = NULL)
{
  K <- object$K
  p <- object$p
  obs <- object$obs
  type <- object$type
  data.all <- object$datamat
  ynames <- colnames(object$y)
  n.ahead <- as.integer(n.ahead)
  Z <- object$datamat[, -c(1:K)]
  B <- Bcoef(object)
  season <- object$call$season
  if (type == "const") {
    Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
    colnames(Zdet) <- "const"
  }
  else if (type == "trend") {
    trdstart <- nrow(Z) + 1 + p
    Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead,
                   ncol = 1)
    colnames(Zdet) <- "trend"
  }
  else if (type == "both") {
    trdstart <- nrow(Z) + 1 + p
    Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)),
                   nrow = n.ahead, ncol = 2)
    colnames(Zdet) <- c("const", "trend")
  }
  else if (type == "none") {
    Zdet <- NULL
  }
  if (!is.null(eval(object$call$season))) {
    season <- eval(object$call$season)
    seas.names <- paste("sd", 1:(season - 1), sep = "")
    cycle <- tail(data.all[, seas.names], season)
    seasonal <- as.matrix(cycle, nrow = season, ncol = season -
                            1)
    if (nrow(seasonal) >= n.ahead) {
      seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead,
                            ncol = season - 1)
    }
    else {
      while (nrow(seasonal) < n.ahead) {
        seasonal <- rbind(seasonal, cycle)
      }
      seasonal <- seasonal[1:n.ahead, ]
    }
    rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
    if (!is.null(Zdet)) {
      Zdet <- as.matrix(cbind(Zdet, seasonal))
    }
    else {
      Zdet <- as.matrix(seasonal)
    }
  }
  if (!is.null(eval(object$call$exogen))) {
    if (is.null(dumvar)) {
      stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
    }
    if (!all(colnames(dumvar) %in% colnames(data.all))) {
      stop("\nColumn names of dumvar do not coincide with exogen.\n")
    }
    if (!identical(nrow(dumvar), n.ahead)) {
      stop("\nRow number of dumvar is unequal to n.ahead.\n")
    }
    if (!is.null(Zdet)) {
      Zdet <- as.matrix(cbind(Zdet, dumvar))
    }
    else {
      Zdet <- as.matrix(dumvar)
    }
  }
  if(is.null(test)){
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))])
    lasty <- c(Zy[nrow(Zy), ])
    for (i in 1:n.ahead) {
      lasty <- lasty[1:(K * p)]
      Z <- c(lasty, Zdet[i, ])
      forecast[i, ] <- B %*% Z
      temp <- forecast[i, ]
      lasty <- c(temp, lasty)
    }
  } else {
    forecast <- matrix(NA, ncol = K, nrow = nrow(test))
    test <- as.matrix(test)
    Ztest <- VAR_proc(y = test, p = p, K = K, type = type,
                      season = season, exogen = NULL)
    Zy <- as.matrix(object$datamat[, -((K*p+1):(K*(p+1)))])
    Z <- rbind(Zy[nrow(Zy),],Ztest[-nrow(Ztest),-((K*p+1):(K*(p+1)))])
    forecast <- Z%*%t(B)
  }
  colnames(forecast) <- paste(ynames, ".fcst", sep = "")
  result <- list(fcst = forecast, endog = object$y, model = object,
                 exo.fcst = dumvar)
  class(result) <- "varprd"
  return(result)
}

restrict <- function (x, method = c("ser", "manual"), thresh = 2, resmat = NULL)
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'var()'.\n")
  }
  method <- match.arg(method)
  thresh <- abs(thresh)
  K <- x$K
  p <- x$p
  datasub <- x$datamat[, -c(1:K)]
  namesall <- colnames(datasub)
  yendog <- x$datamat[, c(1:K)]
  sample <- x$obs
  ser <- function(x, y) {
    tvals <- abs(coef(summary(x))[, 3])
    datares <- datasub
    if (min(tvals) >= thresh) {
      lmres <- x
      datares <- datasub
    }
    else {
      while (min(tvals) < thresh) {
        if (ncol(datares) > 1) {
          cnames <- colnames(datares)
          datares <- as.data.frame(datares[, -1 * which.min(tvals)])
          colnames(datares) <- cnames[-1 * which.min(tvals)]
          lmres <- lm(y ~ -1 + ., data = datares)
          tvals <- abs(coef(summary(lmres))[, 3])
        }
        else {
          lmres <- NULL
          datares <- NULL
          break
        }
      }
    }
    return(list(lmres = lmres, datares = datares))
  }
  if (method == "ser") {
    x$restrictions <- matrix(0, nrow = K, ncol = ncol(datasub))
    colnames(x$restrictions) <- namesall
    rownames(x$restrictions) <- colnames(yendog)
    for (i in 1:K) {
      temp <- ser(x$varresult[[i]], yendog[, i])
      if (is.null(temp$lmres)) {
        stop(paste("\nNo significant regressors remaining in equation for",
                   colnames(yendog)[i], ".\n"))
      }
      x$varresult[[i]] <- temp[[1]]
      namessub <- colnames(temp[[2]])
      x$restrictions[i, namesall %in% namessub] <- 1
    }
  }
  else if (method == "manual") {
    resmat <- as.matrix(resmat)
    if (!(nrow(resmat) == K) | !(ncol(resmat) == ncol(datasub))) {
      stop(paste("\n Please provide resmat with dimensions:",
                 K, "x", ncol(datasub), "\n"))
    }
    x$restrictions <- resmat
    colnames(x$restrictions) <- namesall
    rownames(x$restrictions) <- colnames(yendog)
    for (i in 1:K) {
      datares <- data.frame(datasub[, which(x$restrictions[i,
      ] == 1)])
      colnames(datares) <- colnames(datasub)[which(x$restrictions[i,
      ] == 1)]
      y <- yendog[, i]
      lmres <- lm(y ~ -1 + ., data = datares)
      x$varresult[[i]] <- lmres
    }
  }
  return(x)
}

# Interno para usar dentro del metodo predict
VAR_proc <- function(y, p, K, type = c("const", "trend", "both", "none"),
                     season = NULL, exogen = NULL)
{
  obs <- dim(y)[1]
  sample <- obs - p
  ylags <- embed(y, dimension = p + 1)[, -(1:K)]
  temp1 <- NULL
  for (i in 1:p) {
    temp <- paste(colnames(y), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
  }
  colnames(ylags) <- temp1
  if (type == "const") {
    rhs <- cbind(ylags, rep(1, sample))
    colnames(rhs) <- c(colnames(ylags), "const")
  }
  else if (type == "trend") {
    rhs <- cbind(ylags, seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "trend")
  }
  else if (type == "both") {
    rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "const", "trend")
  }
  else if (type == "none") {
    rhs <- ylags
    colnames(rhs) <- colnames(ylags)
  }
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < obs) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:obs, ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
    rhs <- cbind(rhs, dums[-c(1:p), ])
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen),
                                sep = "")
      warning(paste("No column names supplied in exogen, using:",
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    tmp <- colnames(rhs)
    rhs <- cbind(rhs, exogen[-c(1:p), ])
    colnames(rhs) <- c(tmp, colnames(exogen))
  }
  cbind(y[-c(1:p), ],rhs)
}

residuals.varest <- function (object, ...)
{
  return(sapply(object$varresult, residuals))
}
