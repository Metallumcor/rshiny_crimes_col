### Datos SIEDCO
library(lubridate)
# PATH
PATH <- system.file('extdata',package = 'test2021')

# Lectura
bog_hurto_celulares <- read.csv(paste0(PATH,'/SIEDCO_HURTO.csv'),
                           encoding = 'UTF-8')
# Nombre de columna
col_celulares <- c('ANNO', 'NUM_MES', 'MES', 'NOM_DIA', 'RANG_DIA',
                   'CODIGO_DIV1', 'SEXO', 'DELITO', 'MODALIDAD',
                   'ARMA_EMPLEADA', 'NUM_HECHOS')
names(bog_hurto_celulares) <- col_celulares
# Combinar anno y mes
bog_hurto_celulares$FECHA_MES <- paste(bog_hurto_celulares$ANNO,
                                 bog_hurto_celulares$NUM_MES, sep = "-")
bog_hurto_celulares$FECHA_MES <- format(lubridate::ym(bog_hurto_celulares$FECHA_MES),
                                  '%Y-%m')
# Eliminar columnas
bog_hurto_celulares$MES <- NULL
bog_hurto_celulares$DELITO <- NULL
bog_hurto_celulares$ANNO <- NULL
bog_hurto_celulares$NUM_MES <- NULL
# Eliminar el nombre de la localidad
bog_hurto_celulares$CODIGO_DIV1 <- substr(bog_hurto_celulares$CODIGO_DIV1,1,2)
# (Puede servir) Eliminar modalidad
bog_hurto_celulares$MODALIDAD <- NULL
# Guardado
use_data(bog_hurto_celulares, overwrite = TRUE)
