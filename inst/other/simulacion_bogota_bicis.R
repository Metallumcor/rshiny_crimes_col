###### Rutina de simulacion - hurto de bicicletas

#### Librerias
library(MASS)
library(test2021)
library(dplyr)
#### Modificar el PATH de ser necesario
PATH <- getwd()
#### Bogota
### Informacion para generar rutinas
##  Proporcionalidad por localidades - Excluir SIN LOCALIZACION y SUMAPAZ
incidencia <- c(145, 296, 752, 33, 291, 240, 1195, 540, 1348,
                172, 367, 190, 101, 156, 1090, 341, 181, 517, 68)
lista_localidades <- unique(bogota@data[,c("CODIGO_DIV1","NOMBRE_DIV1")])
df_prop_localidad <- cbind(lista_localidades[order(lista_localidades$NOMBRE_DIV1),],
                           prop.table(incidencia))
## Proporcionalidad por sexo
df_prop_sexo <- data.frame(
  SEXO = c("FEMENINO", "MASCULINO", "NO REPORTA"),
  PROP = prop.table(c(1579, 6332, 112))
)
## Proporcionalidad por rango del dia
df_prop_rang_dia <- data.frame(
  RANG_DIA = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"),
  PROP = prop.table(c(1868, 2387, 1915, 1791))
)
### Generador de cantidad de hurtos diaria - asumir entre 35 a 41 robos dia
robo_dia <- function(){
  round(runif(1,35,41),0)
}
### Kernels para localidades
# Archivo que almacena los puntos clave para tomar como medias de los ker biv
# Incluye probabilidades para mixturas en localidades
localidad_kernel <- read.csv(paste0(PATH,"/extdata/simu_bogota_bici.txt"))
localidad_kernel$NOMBRE_DIV1 <- toupper(localidad_kernel$NOMBRE_DIV1)
# Funcion para generar las varianzas de los kernels
var_localidad <- function(){
  u_localidad <- unique(localidad_kernel$NOMBRE_DIV1)
  result <- lapply(u_localidad,
         function(x){
           selection <- localidad_kernel[localidad_kernel$NOMBRE_DIV1 == x,]
           lapply(1:nrow(selection),
                  function(y){
                    sd_components <- runif(2, 0.005, 0.01)
                    cor_component <- runif(1, -0.99, 0.99)*sd_components[1]*sd_components[2]
                    var_components <- sd_components*sd_components
                    c(var_components[1], cor_component, cor_component,
                      var_components[2])
                  })
         })
  names(result) <- u_localidad
  result
}
# Varianzas simuladas
set.seed(14)
var_elements <- var_localidad()
# Rutina para simular los puntos diarios
simu_localidad <- function(x, n, var_list){
  selection <- localidad_kernel[localidad_kernel$NOMBRE_DIV1 == x,]
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
# Rutina para simular toda Bogota en un dia
simu_bogota <- function(day, var_list){
  if(!inherits(date,"POSIXct")){
    day <- as.POSIXct(day)
  }
  ncounts <- robo_dia()
  u_localidad <- df_prop_localidad$NOMBRE_DIV1
  probs <- df_prop_localidad$PROP
  components <- sample(1:length(u_localidad), prob = probs, size = ncounts,
                       replace = TRUE)
  u_components <- unique(components)
  freq_components <- table(factor(components, levels = 1:length(u_localidad)))
  # Ciclo sobre localidades
  result <- lapply(u_components,
                   function(y){
                     simu_localidad(u_localidad[y],
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
# Rutina para simular toda Bogota en un rango de dias
simu_bogota_range <- function(start_day, end_day, var_list){
  date_range <- as.POSIXct(c(start_day, end_day))
  dates <- seq(date_range[1], date_range[2], by = "day")
  result <- lapply(dates,
                   function(y){
                     simu_bogota(day = y, var_list = var_list)
                   })
  dplyr::bind_rows(result)
}
### Simulacion actual
# Dia inicial
inicial <- "2010-01-01"
final <- "2021-04-30"
# Simulacion de puntos
set.seed(2021)
bog_hurto_bicicletas <- simu_bogota_range(inicial, final, var_elements)
# Simulacion de variables categoricas
bog_hurto_bicicletas$SEXO <- sample(df_prop_sexo$SEXO,
                                    prob = df_prop_sexo$PROP,
                                    size = nrow(bog_hurto_bicicletas),
                                    replace = TRUE)
bog_hurto_bicicletas$RANG_DIA <-  sample(df_prop_rang_dia$RANG_DIA,
                                         prob = df_prop_rang_dia$PROP,
                                         size = nrow(bog_hurto_bicicletas),
                                         replace = TRUE)
### Elementos adicionales - localidad y upz
sp_elements <- test2021:::get_location(bog_hurto_bicicletas, bogota)
col_sel <- c("CODIGO_DIV1", "CODIGO_DIV2")
bog_hurto_bicicletas <- cbind(bog_hurto_bicicletas, sp_elements[,col_sel])
bog_hurto_bicicletas <- bog_hurto_bicicletas[complete.cases(bog_hurto_bicicletas),]
### Guardar base de datos
usethis::use_data(bog_hurto_bicicletas, overwrite = TRUE)
