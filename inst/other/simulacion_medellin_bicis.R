###### Rutina de simulacion - hurto de bicicletas

#### Librerias
library(MASS)
library(test2021)
library(dplyr)
#### Modificar el PATH de ser necesario
PATH <- getwd()
#### Medellin
### Informacion para generar rutinas
##  Proporcionalidad por comunas
df_prop_comuna <- read.csv(paste0(PATH,"/extdata/reporte_medellin_bici.txt"),
                           colClasses = c("character","character","integer"))
df_prop_comuna$NOMBRE_DIV1 <- toupper(df_prop_comuna$NOMBRE_DIV1)
df_prop_comuna$PROP <- prop.table(df_prop_comuna$N)
## Proporcionalidad por sexo (inventado)
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(20, 77, 3))
)
## Proporcionalidad por rango del dia (copiado de Bogota)
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(1868, 2387, 1915, 1791))
)
### Generador de cantidad de hurtos diaria - asumir entre 1 a 5 robos dia
robo_dia <- function(){
  floor(runif(1,1,6))
}
### Kernels para comunaes
# Archivo que almacena los puntos clave para tomar como medias de los ker biv
# Incluye probabilidades para mixturas en comunas
comuna_kernel <- read.csv(paste0(PATH,"/extdata/simu_medellin_bici.txt"))
comuna_kernel$NOMBRE_DIV1 <- toupper(comuna_kernel$NOMBRE_DIV1)
# Funcion para generar las varianzas de los kernels
var_comuna <- function(){
  u_comuna <- comuna_kernel$NOMBRE_DIV1
  result <- lapply(u_comuna,
                   function(x){
                     selection <- comuna_kernel[comuna_kernel$NOMBRE_DIV1 == x,]
                     lapply(1:nrow(selection),
                            function(y){
                              sd_components <- runif(2, 0.005, 0.01)
                              cor_component <- runif(1, -0.99, 0.99)*sd_components[1]*sd_components[2]
                              var_components <- sd_components*sd_components
                              c(var_components[1], cor_component, cor_component,
                                var_components[2])
                            })
                   })
  names(result) <- u_comuna
  result
}
# Varianzas simuladas
set.seed(14)
var_elements <- var_comuna()
# Rutina para simular los puntos diarios
simu_comuna <- function(x, n, var_list){
  selection <- comuna_kernel[comuna_kernel$NOMBRE_DIV1 == x,]
  probs <- selection$PROBA
  if (sum(probs) != 1){
    probs[1] <- 1-sum(probs)
  }
  components <- sample(1:nrow(selection), prob = probs, size = n,
                       replace = TRUE)
  u_components <- unique(components)
  var_sub_list <- get(x, var_list)
  freq_components <- table(factor(components, levels = 1:nrow(selection)))
  # Ciclo sobre localizaciones
  result <- lapply(u_components,
                   function(y){
                     MASS::mvrnorm(n = freq_components[y],
                                   mu = as.numeric(selection[y, 2:3]),
                                   Sigma = matrix(var_sub_list[[y]], 2, 2))
                   })
  # Retornos
  if (inherits(result, "list")){
    do.call(rbind, result)
  } else {
    result
  }
}
# Rutina para simular toda Medellin en un dia
simu_medellin <- function(day, var_list){
  if(!inherits(date,"POSIXct")){
    day <- as.POSIXct(day)
  }
  ncounts <- robo_dia()
  u_comuna <- df_prop_comuna$NOMBRE_DIV1
  probs <- df_prop_comuna$PROP
  components <- sample(1:length(u_comuna), prob = probs, size = ncounts,
                       replace = TRUE)
  u_components <- unique(components)
  freq_components <- table(factor(components, levels = 1:length(u_comuna)))
  # Ciclo sobre comunaes
  result <- lapply(u_components,
                   function(y){
                     simu_comuna(u_comuna[y],
                                    n = freq_components[y],
                                    var_list = var_list)
                   })
  # Ajuste de retorno a formato
  df_result <- data.frame(do.call(rbind, result))
  df_result <- cbind(df_result, rep(day, nrow(df_result)))
  names(df_result) <- c("LAT", "LNG", "FECHA")
  # Retorno
  df_result
}
# Rutina para simular toda Medellin en un rango de dias
simu_medellin_range <- function(start_day, end_day, var_list){
  date_range <- as.POSIXct(c(start_day, end_day))
  dates <- seq(date_range[1], date_range[2], by = "day")
  result <- lapply(dates,
                   function(y){
                     simu_medellin(day = y, var_list = var_list)
                   })
  dplyr::bind_rows(result)
}
### Simulacion actual
# Dia inicial
inicial <- "2010-01-01"
final <- "2021-04-30"
# Simulacion de puntos
set.seed(2021)
med_hurto_bicicletas <- simu_medellin_range(inicial, final, var_elements)
# Simulacion de variables categoricas
med_hurto_bicicletas$SEXO <- sample(df_prop_sexo$SEXO,
                                    prob = df_prop_sexo$PROP,
                                    size = nrow(med_hurto_bicicletas),
                                    replace = TRUE)
med_hurto_bicicletas$RANG_DIA <-  sample(df_prop_rang_dia$RANG_DIA,
                                         prob = df_prop_rang_dia$PROP,
                                         size = nrow(med_hurto_bicicletas),
                                         replace = TRUE)
### Elementos adicionales - comuna y upz
sp_elements <- test2021:::get_location(med_hurto_bicicletas, medellin)
col_sel <- c("CODIGO_DIV1", "CODIGO_DIV2")
med_hurto_bicicletas <- cbind(med_hurto_bicicletas, sp_elements[,col_sel])
med_hurto_bicicletas <- med_hurto_bicicletas[complete.cases(med_hurto_bicicletas),]
### Guardar base de datos
usethis::use_data(med_hurto_bicicletas, overwrite = TRUE)
