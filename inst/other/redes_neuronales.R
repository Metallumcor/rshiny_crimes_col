#### GUIA PARA CREACION DE LAS FUNCIONES DE REDES NEURONALES

#### Lectura de datos - copiada de clasificacion.R
### test2021 para todas las utilidades y tidyverse para uso general
require(test2021, quietly = TRUE)
require(dplyr, quietly = TRUE)

### Carga de datos
# Cambiar de acuerdo a la localizacion de los archivos externos
PATH <- "D:/R_projects/r_packages/path/work_pkg/test2021"
## Conjunto de datos existentes
bici_db <- lon_hurto_bicicletas
bici_db <- subset(bici_db, select = c(FECHA, CODIGO_DIV1,
                                      CODIGO_DIV2, SEXO,
                                      RANG_DIA))
## Proporcionalidad por sexo - Tomado de robo de bicis Bogota
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(1579, 6332, 112))
)
## Proporcionalidad por rango del dia - Tomado de robo de bicis Bogota
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(1868, 2387, 1915, 1791))
)
## Conjunto de datos externo
# Lectura, se realiza por partes
lon_part1 <- read.csv(paste0(PATH,"/extdata/londres_part1.csv"))
lon_part2 <- read.csv(paste0(PATH,"/extdata/londres_part2.csv"))
lon_part3 <- read.csv(paste0(PATH,"/extdata/londres_part3.csv"))
lon_part4 <- read.csv(paste0(PATH,"/extdata/londres_part4.csv"))
# Acoplamiento
lon_full <- dplyr::bind_rows(lon_part1, lon_part2, lon_part3, lon_part4)
# Depurar memoria
rm(lon_part1,  lon_part2, lon_part3, lon_part4)
# Filtar para robo de bicis
lon_filter_bici <- lon_full %>%
  dplyr::filter(Crime.type == "Bicycle theft") %>%
  dplyr::select(Month, Longitude, Latitude)
# Otra depuracion
rm(lon_full)
# Cambiar nombres para acoplar bases
names(lon_filter_bici) <- c("FECHA", "longitude", "latitude")
# Usar solo casos completos
lon_filter_bici <- lon_filter_bici[complete.cases(lon_filter_bici),]
## Procesamiento con las funciones del paquete
# Agregar informacion espacial - Distritos y barrios
lon_filter_bici <- add_spatial_f(lon_filter_bici, londres, "CODIGO_DIV1", "CODIGO_DIV2")
# Agregar informacion de sexo y rango dia
lon_filter_bici <- simular_variables_adicionales(lon_filter_bici, df_prop_sexo = df_prop_sexo,
                                                 df_prop_rang_dia = df_prop_rang_dia)
# Sintetizar todo en una sola base de datos
bici_db <- dplyr::bind_rows(lon_filter_bici, bici_db)
# Transformar columna de fecha en Date
bici_db$FECHA <- as.Date(paste0(bici_db$FECHA,"-01"))
# Depuracion
rm(lon_filter_bici)

### Definir los data.tables a usar
bici_dt <- data.table::as.data.table(bici_db)

### Se requiere una separacion por distrito, en principio. Completando ceros
db_1a <- as.data.frame(bici_dt[,.N,.(FECHA, CODIGO_DIV1)])
db_1a <- fill_long(db_1a$N, subset(db_1a, select = -N),
                   date_col = "FECHA", time_delta = "1 month")

## 1.2. Normalizacion por densidades
lon_pop <- read.csv(paste0(PATH,"/extdata/londres_pop.csv"))
# Eliminar las agregaciones
agg_cat <- c("London", "Greater London", "Outer London", "Inner London")
lon_pop <- lon_pop[!lon_pop$Name %in% agg_cat,]
# Retener solo campos necesarios
lon_pop <- lon_pop[,c("Name","Year","Population")]
# Obtener los CODIGOS_DIV1
lon_pop <- merge(lon_pop,
                 unique(londres@data[,c("CODIGO_DIV1", "NOMBRE_DIV1")]),
                 by.x = "Name", by.y =  "NOMBRE_DIV1")
# Merge con la tabla
db_1a$Year <- format(db_1a$FECHA,"%Y")
db_1a_norm <- merge(db_1a, lon_pop, by = c("CODIGO_DIV1","Year"))
# Corregir poblacion flotante (Tomando 542.000 a 2020 y tasa de
# crecimiento exponencial constante de 1.35% anual)
db_1a_norm <- db_1a_norm %>%
  mutate(Population = Population +
           ifelse(Name == "City of London",
                  506851.2*(1+0.0135)^(as.numeric(Year)-2015),0))
# Tasa de delitos por 100 mil habitantes
db_1a_norm$y <- 100000*db_1a_norm$y/db_1a_norm$Population
db_1a_norm$y <- 100000*db_1a_norm$y/db_1a_norm$Population

### Adicion de lags
db_1b_norm <- add_laggs(db_1a_norm, db_1a_norm$y, p = 3)
### Adicion de secuencia numerica de seguimiento de fecha
fecha_look <- data.frame(fecha = unique(db_1b_norm$FECHA),
                         num_fecha = 1:length(unique(db_1b_norm$FECHA)))
db_1b_norm$FECHA_NUM <- fecha_look[match(db_1b_norm$FECHA,fecha_look$fecha),"num_fecha"]

# Renombrar respuesta por conveniencia
db_1a_norm$CLASS <- db_1a_norm$y
db_1b_norm$CLASS <- db_1b_norm$y
# Drop de variables no requeridas
db_1a_norm <- subset(db_1a_norm, select = c(CODIGO_DIV1, FECHA, CLASS))
db_1b_norm <- subset(db_1b_norm, select = c(CODIGO_DIV1, FECHA, CLASS,
                                            Y1, Y2, Y3, FECHA_NUM))
# Tratamiento de fechas
db_1a_norm <- add_date_split(db_1a_norm, "FECHA",
                             week = FALSE,
                             week_day = FALSE)
db_1a_norm$ANNO <- as.numeric(db_1a_norm$ANNO)
db_1b_norm <- add_date_split(db_1b_norm, "FECHA",
                             week = FALSE,
                             week_day = FALSE)
db_1b_norm$ANNO <- as.numeric(db_1b_norm$ANNO)
# Crear splits
db_1a_norm$CODIGO_DIV1 <- factor(db_1a_norm$CODIGO_DIV1)
db_1a_norm$MES <- factor(db_1a_norm$MES)
split_class <- create_split_long(db_1a_norm$CLASS,
                                 db_1a_norm[,-2],
                                 factors = "CODIGO_DIV1",
                                 split_prop = c(0.7,0.15,0.15))
db_1b_norm$CODIGO_DIV1 <- factor(db_1b_norm$CODIGO_DIV1)
db_1b_norm$MES <- factor(db_1b_norm$MES)
split_class_b <- create_split_long(db_1b_norm$CLASS,
                                   db_1b_norm[,-2],
                                   factors = "CODIGO_DIV1",
                                   split_prop = c(0.7,0.15,0.15))

## A.1 Intervalos usando cuantiles -> Cuantiles 0.15, 0.5 y 0.8
int_cuantiles <- num2ctgr(split_class$Y$Y_train,
                          type = "quantiles", probs = c(0.15, 0.5, 0.8))
int_cuantiles_b <- num2ctgr(split_class_b$Y$Y_train, type = "quantiles",
                            probs = c(0.15, 0.5, 0.8))

### Remplazar los nombres en entrenamiento
niveles <- c("Muy bajo","Bajo","Moderado","Alto")
split_class$Y$Y_train <- ctgr2nmd(int_cuantiles, niveles)
split_class_b$Y$Y_train <- ctgr2nmd(int_cuantiles_b, niveles)

### Remplazar los nombres en validacion
split_class$Y$Y_val <- ctgr2nmd(add2ctgr(split_class$Y$Y_val, int_cuantiles), niveles)
split_class_b$Y$Y_val <- ctgr2nmd(add2ctgr(split_class_b$Y$Y_val, int_cuantiles), niveles)

### Conjuntos escalados usando entrenamiento
# Original
split_class_std <- split_class
std_a <- scale(split_class$X$X_train$ANNO)
split_class_std$X$X_train$ANNO <- as.numeric(std_a)
mn_train <- attr(std_a,"scaled:center")
sd_train <- attr(std_a,"scaled:scale")
split_class_std$X$X_val$ANNO <- ((split_class_std$X$X_val$ANNO)-mn_train)/sd_train
# Aumentado
split_class_b_std <- split_class_b
num_ind <- sapply(split_class_b$X$X_train, is.numeric)
std_b <- lapply(split_class_b$X$X_train[num_ind], scale)
split_class_b_std$X$X_train[num_ind] <- as.numeric(do.call(cbind, std_b))
mn_b_train <- lapply(std_b, function(x)attr(x,"scaled:center"))
sd_b_train <- lapply(std_b, function(x) attr(x,"scaled:scale"))
split_class_b_std$X$X_val[num_ind] <-
  mapply(function(x,y,z) (x-y)/z, x = split_class_b_std$X$X_val[num_ind],
         y = mn_b_train, z = sd_b_train)

#### ACA EMPIEZA LA PRACTICA CON REDES NEURONALES
### Podemos usar Keras o MXNet
## Keras
# Con Keras tenemos el problema de requerir la instalacion de Python/Keras
# manualmente en el equipo de destino
# keras::is_keras_available() esto ayudara a instalar Miniconda de ser necesario
# keras::install_keras() esto dara la asistencia para instalar Py o Miniconda
# de ser necesario y de paso Keras
## MXNet
# La instalacion es complicada y en principio requiere una build old de R
# Se puede ajustar a nuevas builds pero es aun mas complicada la instalacion
# Aca la guia
# https://github.com/apache/incubator-mxnet/blob/981da7ea7a6958f449a69ce5c7ba97ad09952f18/docs/static_site/src/pages/get_started/windows_setup.md#building-mxnet-r-from-source-codecpu

## Conclusion: Usar Keras y tomarse la molestia de instalar Python...

### Usamos la libreria Keras
library(keras)
### Estos son los links a mirar
# La guia oficial
# https://tensorflow.rstudio.com/guide/keras/sequential_model/
# El libro a descargar en el otro pc
# https://www.amazon.com/Deep-Learning-R-Francois-Chollet/dp/161729554X
# Explicacion de softmax
# https://www.r-bloggers.com/2021/04/not-so-soft-softmax/
# Una guia final
# https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/

# Chequear si keras existe...
keras::is_keras_available()
# Puede dar un error de GPU y se puede solucionar considerando su dispo

# Consideramos los siquientes puntos antes de montar la NN
# Con nnet la hidden layer usa una activacion logistica (cuando se lo pide)
# El output en nnet (sin importar si usa hidden layer) es softmax
# NOTA: En keras la activacion logistica se llama sigmoide

# El modelo a usar sera uno secuencial
# Primero que nada debemos "tratar" los datos ya que Keras es mamon y no hace
# el encoding por nosotros.
# Las categorias deben tener niveles numericos de 0 a #niveles-1

## Encoding
# Para X usar
# as.data.frame(model.matrix(~.-1,data = split_class_b$X$X_train))

# Para Y usar
# keras::to_categorical(split_class_b$Y$Y_train %>%as.numeric() - 1)

# Funcion para pasar el split a otra vaina
split_nn <- function(split){
  # Pase de X
  split$X$X_train <- model.matrix(~.-1,data = split$X$X_train)
  split$X$X_val <- model.matrix(~.-1,data = split$X$X_val)
  split$X$X_test <- model.matrix(~.-1,data = split$X$X_test)
  # Pase de Y
  split$Y$Y_train <- keras::to_categorical(as.numeric(split$Y$Y_train) - 1)
  split$Y$Y_val <- keras::to_categorical(as.numeric(split$Y$Y_val) - 1)
  split$Y$Y_test <- keras::to_categorical(as.numeric(split$Y$Y_test) - 1)
  # Retorno
  return(split)
}

# Split estandarizado con todos los juguetes y expandido por categorias
split_keras <- split_one_hot(split_class_b_std)

## Modelo 1
# Tendremos algo como 49 inputs (9 de junio) y una output de dimension cuatro
# Usaremos un modelo fully conected secuencial de 2 capas ocultas
# Primera capa oculta: Activador relu, 49 nodos
# Segunda capa oculta: Activador sigmoid, [49/2] = 24 nodos
# Output: Softmax

# Las input_shape deben ser iguales a numero de columnas de X
modelo_1 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "relu", input_shape = c(49)) %>%
  keras::layer_dense(units = 24, activation = "sigmoid") %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 2
# Tendremos algo como 49 inputs (9 de junio) y una output de dimension cuatro
# Usaremos un modelo fully conected secuencial de 2 capas ocultas
# Primera capa oculta: Activador relu, 49 nodos
# Segunda capa oculta: Activador relu, [49/2] = 24 nodos
# Output: Softmax

modelo_2 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "relu", input_shape = c(49)) %>%
  keras::layer_dense(units = 24, activation = "relu") %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 3
# Tendremos algo como 49 inputs (9 de junio) y una output de dimension cuatro
# Usaremos un modelo fully conected secuencial de 2 capas ocultas
# Primera capa oculta: Activador tanh, 49 nodos
# Segunda capa oculta: Activador tanh, [49/2] = 24 nodos
# Output: Softmax

modelo_3 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "tanh", input_shape = c(49)) %>%
  keras::layer_dense(units = 24, activation = "tanh") %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 4
# El modelo usado por nnet

modelo_4 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 4, activation = "sigmoid", input_shape = c(49)) %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 5
# El modelo usado por nnet pero con mas nodos

modelo_5 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "sigmoid", input_shape = c(49)) %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 6
# El modelo 2 con mas nodos en la segunda capa (3/4 de la primera)

modelo_6 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "relu", input_shape = c(49)) %>%
  keras::layer_dense(units = 37, activation = "relu") %>%
  keras::layer_dense(units = 4, activation = "softmax")

## Modelo 7
# El modelo 2 con dropout en primera capa
modelo_7 <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 49, activation = "relu", input_shape = c(49)) %>%
  keras::layer_dropout(rate = 0.2) %>%
  keras::layer_dense(units = 37, activation = "relu") %>%
  keras::layer_dense(units = 4, activation = "softmax")


## Compilacion de modelos - Usando ADAM

modelo_1 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_2 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_3 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_4 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_5 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_6 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

modelo_7 %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(),
    metrics = c("accuracy")
  )

# NOTA: Ojito aca como se maneja entrenamiento y validacion
# Vamos a usar 200 epoch
# Para batch size me puedo dar el gusto de dejarlo grande por la buena maquina
# Dejarlo a futuro ajustable y bajo para considerar otras maquinas menos potentes

historico_1 <- modelo_1 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 100,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_2 <- modelo_2 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 100,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_3 <- modelo_3 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 100,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_4 <- modelo_4 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 200,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_5 <- modelo_5 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 200,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_6 <- modelo_6 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 200,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

historico_7 <- modelo_7 %>%
  fit(
    split_keras$X$X_train,
    split_keras$Y$Y_train,
    epochs = 200,
    batch_size = 512,
    validation_data = list(split_keras$X$X_val, split_keras$Y$Y_val)
  )

modelo_7 %>% predict(split_keras$X$X_val)
