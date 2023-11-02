### Lectura de shapfiles de Colombia - 2020
library(rgdal)
library(rgeos)
library(dplyr)
library(maptools)
library(usethis)
#
col_shp <- rgdal::readOGR(
  dsn = system.file('shp', 'colombia_2020', package = 'test2021'),
  layer = 'MGN_MPIO_POLITICO',
  encoding = 'UTF-8',
  use_iconv = TRUE,
  verbose = FALSE
)
# Filtrado de columnas de valor unico
col_shp@data <- col_shp@data[vapply(col_shp@data,
                                function(x) length(unique(x))>1, logical(1L))]
# Filtrado de columnas no relevantes
col_shp@data <- subset(col_shp@data,
                       select = -c(MPIO_CRSLC, MPIO_CCNCT, SHAPE_AREA,
                                   SHAPE_LEN, ORIG_FID))
# Simplificacion de geometrias
col_mpio <- rgeos::gSimplify(col_shp, tol = 0.0001, topologyPreserve = TRUE)
col_mpio <- sp::SpatialPolygonsDataFrame(col_mpio, col_shp@data)
# Agregacion por departamentos y reduccion complejidad de poligonos
col_dept <- maptools::unionSpatialPolygons(col_shp, col_shp$DPTO_CCDGO)
df_dept <- col_shp@data %>%
  dplyr::select(DPTO_CCDGO, DPTO_CNMBR, MPIO_NAREA) %>%
  dplyr::group_by(DPTO_CCDGO, DPTO_CNMBR) %>%
  dplyr::summarise(AREA = sum(MPIO_NAREA)) %>%
  as.data.frame()
rownames(df_dept) <- df_dept$DPTO_CCDGO
col_dept <- sp::SpatialPolygonsDataFrame(col_dept, df_dept)

# Guardar datos
usethis::use_data(col_dept)
usethis::use_data(col_mpio)
