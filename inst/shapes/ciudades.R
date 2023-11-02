##### Mapas: ciudades colombianas
library(rgdal)
library(rgeos)
library(rmapshaper)
library(usethis)
### PATH - Cambiar a la carpeta que contenga los shapefiles
PATH <- getwd()
### Bogota - UPZ y Localidades
bogota <- rgdal::readOGR(
  dsn= paste0(PATH,'/shp/bog_upz') ,
  layer="Upz",
  encoding = 'UTF-8',
  verbose=FALSE
)
# Look-up table para localidad-upz
look_bog_upz_loc <- data.frame(
  CODIGO_DIV1 = c(rep("01",9), rep("02",5),
                        rep("03",5), rep("04", 5),
                        rep("05",7), rep("06",2),
                        rep("07",5), rep("08",12),
                        rep("09",8), rep("10",9),
                        rep("11",12), rep("12",4),
                        rep("13",6), rep("14",2),
                        rep("15",2), rep("16",5),
                        "17", rep("18",5),
                        rep("19",8)),
  CODIGO_DIV2 = c(c("1","9","10","11","12","13","14","15","16"),
                  c("88","89","90","97","99"),
                  c("91", "92", "93", "95", "96"),
                  c("32", "33", "34", "50", "51"),
                  c("52", "56", "57", "58", "59", "60", "61"),
                  c("42", "62"),
                  c("49", "84", "85", "86", "87"),
                  c("44", "45", "46", "47", "48", "78", "79", "80",
                    "81", "82", "83", "113"),
                  c("75", "76", "77", "110", "112", "114", "115", "117"),
                  c("26", "29", "30", "31", "72", "73", "74", "105", "116"),
                  c("2", "3", "17", "18", "19", "20", "23", "24", "25",
                    "27", "28", "71"),
                  c("21", "22", "98", "103"),
                  c("100", "101", "104", "106", "107", "109"),
                  c("37", "102"),
                  c("35", "38"),
                  c("40", "41", "43", "108", "111"),
                  c("94"),
                  c("36", "39", "53", "54", "55"),
                  c("63", "64", "65", "66", "67", "68", "69", "70"))
)
# Tabla nombres de localidades
look_bog_loc_nom <- data.frame(
  CODIGO_DIV1 = c("01", "02", "03", "04", "05", "06", "07", "08", "09",
                        "10", "11", "12", "13", "14", "15", "16", "17",
                        "18", "19"),
  NOMBRE_DIV1 = c("USAQUEN", "CHAPINERO", "SANTA FE",
                        "SAN CRISTOBAL", "USME", "TUNJUELITO", "BOSA",
                        "KENNEDY", "FONTIBON", "ENGATIVA", "SUBA",
                        "BARRIOS UNIDOS", "TEUSAQUILLO", "MARTIRES",
                        "ANTONIO NARINO", "PUENTE ARANDA", "CANDELARIA",
                        "RAFAEL URIBE", "CIUDAD BOLIVAR")
)
# Merge del DF del SpatialPolygonsDataFrame con las look-up tables
names(bogota@data)[2] <- 'CODIGO_DIV2'
ID <- rownames(bogota@data)
df_1 <- merge(look_bog_upz_loc, look_bog_loc_nom, by = "CODIGO_DIV1",
              sort = FALSE)
bogota@data <- merge(bogota@data, df_1, by = "CODIGO_DIV2",
                     sort = FALSE)
rownames(bogota@data) <- ID
# Simplificar geometrias
bogota <- rmapshaper::ms_simplify(bogota, keep_shapes = TRUE,
                                  keep = 0.2)
### Medellin - Barrios y comunas
medellin <- rgdal::readOGR(
  dsn= paste0(PATH,'/shp/med_barrios') ,
  layer="Limite_Barrio_Vereda_Catastral",
  encoding = 'UTF-8',
  verbose=FALSE
)
# Simplificar geometrias
names(medellin@data)[c(4,6)] <- c("CODIGO_DIV1", "CODIGO_DIV2")
names(medellin@data)[c(7,8)] <- c("NOMBRE_DIV2", "NOMBRE_DIV1")
medellin$SHAPEAREA <- 0.0001*medellin$SHAPEAREA
names(medellin@data)[2] <- "AREA_HECTA"
medellin <- rmapshaper::ms_simplify(medellin, keep_shapes = TRUE,
                                   keep = 0.2)
# CORRECCION DE ERRORES EN GEOMETRIAS
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
medellin <- rgeos::gBuffer(medellin, byid=TRUE, width=0)
### Cartagena - Barrios
cartagena <- rgdal::readOGR(
  dsn= paste0(PATH,'/shp/cart_barrios') ,
  layer="barrios_pot_ctgna",
  encoding = 'UTF-8',
  verbose=FALSE
)
# Simplificar geometrias
names(cartagena)[2] <- "CODIGO_DIV1"
names(cartagena)[3] <- "NOMBRE_DIV1"
cartagena <- rmapshaper::ms_simplify(cartagena, keep_shapes = TRUE,
                                    keep = 0.2)
# CORRECCION DE ERRORES EN GEOMETRIAS
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
cartagena <- rgeos::gBuffer(cartagena, byid=TRUE, width=0)
### Guardar archivos
usethis::use_data(bogota, medellin, cartagena, overwrite = TRUE)
