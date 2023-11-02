## Carge de librerias
library(test2021)
## Creacion de londres en formato crime_colombia_city_2
londres <- lon_poly_ward_b
names(londres@data) <- c("NOMBRE_DIV2", "NOMBRE_DIV1", "AREA_HECTA")
londres$CODIGO_DIV2 <- as.character(1:nrow(londres@data))
londres$CODIGO_DIV1 <- as.character(as.numeric(factor(londres$NOMBRE_DIV1)))
londres$NOMBRE_DIV1[londres$NOMBRE_DIV1=="City of Westminster"] <- "Westminster"
londres$NOMBRE_DIV1[londres$NOMBRE_DIV1=="City and County of the City of London"] <- "City of London"
use_data(londres)
## Creacion de base de datos de londres en formato crime_colombia_city2
base_principal <- test2021:::list_to_df(db_crime_london_a)
names(base_principal)[2] <- "FECHA"
to_delete <- " a | de | en | y "
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
## Codigos de divisiones
base_principal$DISTRICT <- NULL
base_principal$latitude <- as.numeric(base_principal$latitude)
base_principal$longitude <- as.numeric(base_principal$longitude)
sp_elements <- test2021:::get_location(base_principal, londres)
col_sel <- c("CODIGO_DIV1", "CODIGO_DIV2")
base_principal <- cbind(base_principal, sp_elements[,col_sel])
base_principal <- base_principal[complete.cases(base_principal),]
## Ciclo para generar los data sets con los nombres correspondientes
for (delito in unique(base_principal$category)) {
  base_filt <- base_principal[base_principal$category == delito,]
  base_filt <- test2021::simular_variables_adicionales(
    df_base = base_filt, df_prop_sexo = df_prop_sexo,
    df_prop_rang_dia = df_prop_rang_dia)
  nombre_guardado <- iconv(delito,to="ASCII//TRANSLIT")
  nombre_guardado <- gsub(to_delete,"_", tolower(nombre_guardado))
  nombre_guardado <- gsub(" ","_",nombre_guardado)
  var_name <- paste0('lon_', nombre_guardado)
  assign(var_name, base_filt)
  do.call(usethis::use_data,list(as.name(var_name), overwrite = TRUE,
                               compress = 'bzip2'))
}
