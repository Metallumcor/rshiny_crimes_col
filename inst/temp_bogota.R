PATH <- "C:/Users/predi/Downloads" # PATH - Modificar
siedco_redux <- readRDS(paste0(PATH,"/siedco_redux.rds"))

library(test2021)

summary(siedco_redux) # Resumen
class(siedco_redux) # Resumen

table(siedco_redux$Delito)

# Tratamiento menor
siedco_redux$Delito[grepl("HURTO DE CELULARES",siedco_redux$Delito)] <- "HURTO DE CELULARES"

# Es necesario seleccionar el delito. Los modelos solo corren con un regresor
DELITO <- "HURTO DE CELULARES"
siedco_f <- siedco_redux %>%
  dplyr::filter(Delito == DELITO) %>%
  subset(select = -c(NOM_DIV1,NOM_DIV2,Delito))

temp <- dplyr::bind_rows(date_split(siedco_f$FECHA))
siedco_f <- cbind(siedco_f,temp)
names(siedco_f) <- gsub(" ","_",iconv(toupper(names(siedco_f)),to = "UTF-8"))
siedco_f$ANNO <- as.numeric(siedco_f$ANNO)
# Tratamiento no trivial de niveles - Posible funcion nueva
proc_levels <- function(x, thr, aggr){
  x <- as.character(x)
  freqs <- table(x)/length(x)
  t_change <- names(freqs)[freqs <= thr]
  if(!is.null(t_change))
    x[x %in% t_change] <- aggr
  x
}
siedco_f$MODALIDAD <- proc_levels(siedco_f$MODALIDAD, 0.1,"OTRO")
siedco_f$ARMA_EMPLEADA <- proc_levels(siedco_f$ARMA_EMPLEADA, 0.1,"OTRO")
# Problema: A que nivel completar las series de tiempo
# A nivel de una localidad/UPZ se pueden repetir muchas veces un delito por dia
# fill_long(siedco_f$NUMERO_HECHOS, siedco_f[,c("FECHA","CODIGO_DIV1")], "FECHA")
# Al agregar se pierde informacion para ciertos modelos. Lo mismo al llenar
#### Solucion principal, continuar sin usar series de tiempo
# FALLA
#### Solucion secundaria, agregar por variables
# Eliminar algunas variables que serian mejor predecirlas, no usarlas como predictor
siedco_pred <- subset(siedco_f, select = -c(ARMA_EMPLEADA,MODALIDAD))
siedco_pred[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
            .(FECHA,SEXO,DIA_SEMANA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA,ANNO,MES,SEMANA)]
siedco_pred <- unique(siedco_pred)
# Modelos considerados lineal y NN
# La siguiente syntax es valida por ser siedco_f data.table
split_bogota <- create_split_long(Y = siedco_pred$NUMERO_HECHOS,
                                  X = siedco_pred[,-"NUMERO_HECHOS"],
                                  factors = "CODIGO_DIV1",
                                  split_prop = c(0.8,0.1,0.1))
fits <- reg_models(split_out = split_bogota,
                   factor_split = NULL,dates_col = "FECHA",
                   models = c("linear","nn_1hl","nn_2hl"))
#### Solucion secundaria, agregar por variables
# Eliminar algunas variables que serian mejor predecirlas, no usarlas como predictor
siedco_pred <- subset(siedco_f, select = -c(ARMA_EMPLEADA,MODALIDAD))
siedco_pred[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
            .(FECHA,SEXO,DIA_SEMANA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA,ANNO,MES,SEMANA)]
siedco_pred <- unique(siedco_pred)
# Modelos considerados lineal y NN
# La siguiente syntax es valida por ser siedco_f data.table
split_bogota <- create_split_long(Y = siedco_pred$NUMERO_HECHOS,
                                  X = siedco_pred[,-"NUMERO_HECHOS"],
                                  factors = "CODIGO_DIV1",
                                  split_prop = c(0.8,0.1,0.1))
fits <- reg_models(split_out = split_bogota,
                   factor_split = NULL,dates_col = "FECHA",
                   models = c("linear","nn_1hl","nn_2hl"))
# El modelo no tiene valor la verdad
#### Tercera solucion, agregar ceros y eliminar variable de sexo (necesario)
# Se elimina ceros por ser imposible reconstruirla y no quiero imponer valores
# Asumo que existe registro completo de los eventos y los no causados son los generados
siedco_pred2 <- subset(siedco_f,
                       select = c(NUMERO_HECHOS,FECHA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA))
siedco_pred2[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
             .(FECHA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA)]
siedco_pred2 <- unique(siedco_pred2)

siedco_pred2 <- fill_long(siedco_pred2$NUMERO_HECHOS, siedco_pred2[,-"NUMERO_HECHOS"],"FECHA")
siedco_pred2 <- add_date_split(siedco_pred2,"FECHA",rm_date = FALSE)
siedco_pred2$ANNO <- as.numeric(siedco_pred2$ANNO)
# Este conjunto de datos esta claramente desbalanceado
split_bogota <- create_split_long(Y = siedco_pred2$y,
                                  X = subset(siedco_pred2,select = -y),
                                  factors = "CODIGO_DIV1",
                                  split_prop = c(0.8,0.1,0.1))

prueba <- arima_lin_selector(split_out = split_bogota,factor_split = c("CODIGO_DIV1","CODIGO_DIV2","RANG_DIA"),dates_col = "FECHA",step_selection = TRUE)
#fits <- reg_models(split_out = split_bogota,
#                   factor_split = c("CODIGO_DIV1","CODIGO_DIV2","RANG_DIA"),
#                   dates_col = "FECHA",
#                   models = c("arima","linear","arima_linear",
#                              "linear_arima", "nn_1hl", "nn_2hl"))
# Tomar el problema como si se tratase de clasificacion roban/no roban. Elegante
siedco_clasf <- siedco_pred2
siedco_clasf_list <- test2021::split_by_factors(siedco_clasf,factor_split = c("CODIGO_DIV1","CODIGO_DIV2","RANG_DIA"))
siedco_clasf_list <- lapply(siedco_clasf_list, function(x) add_laggs(subset(x,select=-y),x$y,30))
siedco_clasf <- dplyr::bind_rows(siedco_clasf)
siedco_clasf$y <- test2021::num2ctgr(siedco_clasf$y,"subjective",breaks=c(0,0.5,max(siedco_clasf$y)))
siedco_clasf$y <- test2021::ctgr2nmd(siedco_clasf$y,c("NO","SI"))
split_bogota <- create_split_long(Y = siedco_clasf$y,
                                  X = subset(siedco_clasf,select = -y),
                                  factors = "CODIGO_DIV1",
                                  split_prop = c(0.8,0.1,0.1))
fit_svm <- svm_mod_selector(split_bogota, tuning = TRUE)
fit <- test2021::clsf_models(split_bogota,
                             models = c("naive_bayes", "svm","random_forest", "nn_1hl", "nn_2hl"),
                             only_best = TRUE)

proc_bogota <- function(base, delito = siedco_redux$Delito){
  delito <- match.arg(delito)
  base_f <- base %>%
    dplyr::filter(Delito == DELITO) %>%
    subset(select = -c(NOM_DIV1,NOM_DIV2,Delito))
  names(base_f) <- gsub(" ","_",iconv(toupper(names(base_f)),to = "UTF-8"))
  base_pred <- subset(base_f,
                      select = c(NUMERO_HECHOS,FECHA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA))
  base_pred[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
            .(FECHA,CODIGO_DIV1,CODIGO_DIV2,RANG_DIA)]
  base_pred <- unique(base_pred)
  base_pred <- fill_long(base_pred$NUMERO_HECHOS,
                         base_pred[,-"NUMERO_HECHOS"],"FECHA")
  base_pred <- add_date_split(base_pred,"FECHA",rm_date = FALSE)
  base_pred$ANNO <- as.numeric(base_pred$ANNO)
  base_list <- test2021::split_by_factors(base_pred,factor_split = c("CODIGO_DIV1","CODIGO_DIV2","RANG_DIA"))
  base_list <- lapply(base_list, function(x) add_laggs(x,x$y,7))
  base_pred <- dplyr::bind_rows(base_list)
  base_pred$y <- test2021::num2ctgr(base_pred$y,"subjective",breaks=c(0,0.5,max(base_pred$y)))
  base_pred$y <- test2021::ctgr2nmd(base_pred$y,c("NO","SI"))
  base_pred
}

siedco_clasf <- proc_bogota(siedco_redux, DELITO)
split_bogota <- create_split_long(Y = siedco_clasf$y,
                                  X = subset(siedco_clasf,select = -y),
                                  factors = c("CODIGO_DIV1","CODIGO_DIV2"),
                                  split_prop = c(0.8,0.1,0.1))
fit_rf <- rf_mod_selector(split_bogota,ntree = 100)

fit_rf <- ranger::csrf(y ~ .,
                       training_data = data.frame(y = split_bogota$Y$Y_train, split_bogota$X$X_train),
                       test_data = data.frame(y = split_bogota$Y$Y_val, split_bogota$X$X_val),
                       params1 = list(num.trees = 50, mtry = 4),
                       params2 = list(num.trees = 50, mtry = 4))

proc_bogota2 <- function(base, delito = siedco_redux$Delito){
  delito <- match.arg(delito)
  base_f <- base %>%
    dplyr::filter(Delito == DELITO) %>%
    subset(select = -c(NOM_DIV1,NOM_DIV2,Delito))
  names(base_f) <- gsub(" ","_",iconv(toupper(names(base_f)),to = "UTF-8"))
  base_pred <- subset(base_f,
                      select = c(NUMERO_HECHOS,FECHA,CODIGO_DIV1,RANG_DIA))
  base_pred[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
            .(FECHA,CODIGO_DIV1,RANG_DIA)]
  base_pred <- unique(base_pred)
  base_pred <- fill_long(base_pred$NUMERO_HECHOS,
                         base_pred[,-"NUMERO_HECHOS"],"FECHA")
  base_pred <- add_date_split(base_pred,"FECHA",rm_date = FALSE)
  base_pred$ANNO <- as.numeric(base_pred$ANNO)
  base_list <- test2021::split_by_factors(base_pred,factor_split = c("CODIGO_DIV1","RANG_DIA"))
  base_list <- lapply(base_list, function(x) add_laggs(x,x$y,7))
  base_pred <- dplyr::bind_rows(base_list)
  base_pred
}

siedco_regloc <- proc_bogota2(siedco_redux, DELITO)
split_bogota <- create_split_long(Y = siedco_regloc$y,
                                  X = subset(siedco_regloc,select = -y),
                                  factors = c("CODIGO_DIV1","RANG_DIA"),
                                  split_prop = c(0.8,0.1,0.1))
ejemplo <- arima_mod_selector(split_bogota, dates_col = "FECHA",
                              factor_split = c("CODIGO_DIV1","RANG_DIA"))
ejemplo <- arima_lin_selector(split_bogota, dates_col = "FECHA",
                              factor_split = c("CODIGO_DIV1","RANG_DIA"),
                              step_selection = T)

proc_bogota3 <- function(base, delito = siedco_redux$Delito){
  delito <- match.arg(delito)
  base_f <- base %>%
    dplyr::filter(Delito == DELITO) %>%
    subset(select = -c(NOM_DIV1,NOM_DIV2,Delito))
  names(base_f) <- gsub(" ","_",iconv(toupper(names(base_f)),to = "UTF-8"))
  base_pred <- subset(base_f,
                      select = c(NUMERO_HECHOS,FECHA,CODIGO_DIV1,RANG_DIA))
  base_pred[,NUMERO_HECHOS := sum(NUMERO_HECHOS),
            .(FECHA,CODIGO_DIV1,RANG_DIA)]
  base_pred <- unique(base_pred)
  base_pred <- fill_long(base_pred$NUMERO_HECHOS,
                         base_pred[,-"NUMERO_HECHOS"],"FECHA")
  base_pred <- add_date_split(base_pred,"FECHA",rm_date = FALSE)
  base_pred$ANNO <- as.numeric(base_pred$ANNO)
  base_list <- test2021::split_by_factors(base_pred,factor_split = c("CODIGO_DIV1","RANG_DIA"))
  base_list <- lapply(base_list, function(x) add_laggs(x,x$y,7))
  base_pred <- dplyr::bind_rows(base_list)
  base_pred$y <- test2021::num2ctgr(base_pred$y,"subjective",breaks=c(0,0.5,max(base_pred$y)))
  base_pred$y <- test2021::ctgr2nmd(base_pred$y,c("NO","SI"))
  base_pred
}
