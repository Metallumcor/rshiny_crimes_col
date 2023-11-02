### Lectura de shapfiles de Colombia - 2020
library(rgdal)
library(rgeos)
library(dplyr)
library(maptools)
library(usethis)
library(rmapshaper)
#
col_shp <- rgdal::readOGR(
  dsn = paste0(getwd(),'/shp/colombia_censo_2018'),
  layer = 'MGN_ANM_MPIOS',
  encoding = 'UTF-8',
  use_iconv = TRUE,
  verbose = FALSE
)
# Filtrado de columnas de valor unico
col_shp@data <- col_shp@data[vapply(col_shp@data,
                                function(x) length(unique(x))>1, logical(1L))]
# Listado de columnas a conservar
key_vals <- c('DPTO_CCDGO', 'MPIO_CCDGO', 'MPIO_CNMBR', 'AREA', 'LATITUD',
              'LONGITUD','STP27_PERS')
# Filtrado de columnas no relevantes
col_shp@data <- col_shp@data[key_vals]
col_shp@data$TOTPOP <- col_shp@data$STP27_PERS
col_shp@data$STP27_PERS <- NULL
# Simplificacion de geometrias
col_mpio <- rgeos::gSimplify(col_shp, tol = 0.0001, topologyPreserve = TRUE)
col_mpio <- sp::SpatialPolygonsDataFrame(col_mpio, col_shp@data)
col_mpio <- rmapshaper::ms_simplify(col_mpio)
# Agregacion por departamentos y reduccion complejidad de poligonos
col_dept <- maptools::unionSpatialPolygons(col_shp, col_shp$DPTO_CCDGO)
df_dept <- col_shp@data %>%
  dplyr::select(DPTO_CCDGO, AREA, TOTPOP) %>%
  dplyr::group_by(DPTO_CCDGO) %>%
  dplyr::summarise(AREA = sum(AREA), TOTPOP = sum(TOTPOP)) %>%
  as.data.frame()
rownames(df_dept) <- df_dept$DPTO_CCDGO
col_dept <- sp::SpatialPolygonsDataFrame(col_dept, df_dept)
col_dept <- rmapshaper::ms_simplify(col_dept)

# Guardar datos
usethis::use_data(col_dept, overwrite = TRUE)
usethis::use_data(col_mpio, overwrite = TRUE)
