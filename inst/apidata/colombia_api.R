### Lectura de archivos de crimen - Ponal
library(RSocrata)
library(dplyr)
library(usethis)
library(data.table)

col_ponal_url <- data.frame(
  crime_id = c(
    "sexual",
    "vio_intra",
    "secuestro",
    "terrorismo",
    "homicidios",
    "hurto_1",
    "extorsion",
    "hurto_2",
    "lesiones",
    "hurto_3"
  ),
  crime_type = c(
    "Sexuales",
    "Violencia intrafamiliar",
    "Secuestro",
    "Terrorismo",
    "Homicidios",
    "Hurto 1",
    "Extorsion",
    "Hurto 2",
    "Lesiones personales y accidentes",
    "Hurto 3"
  ),
  crime_url = c(
    "https://www.datos.gov.co/resource/fpe5-yrmw.json",
    "https://www.datos.gov.co/resource/vuyt-mqpw.json",
    "https://www.datos.gov.co/resource/37p5-impc.json",
    "https://www.datos.gov.co/resource/9jhz-f3a7.json",
    "https://www.datos.gov.co/resource/ha6j-pa2r.json",
    "https://www.datos.gov.co/resource/d4fr-sbn2.json",
    "https://www.datos.gov.co/resource/cekg-4xti.json",
    "https://www.datos.gov.co/resource/6sqw-8cg5.json",
    "https://www.datos.gov.co/resource/72sg-cybi.json",
    "https://www.datos.gov.co/resource/9vha-vh9n.json"
  )
)

for(c_id in col_ponal_url$crime_id){
  df <- RSocrata::read.socrata(
    url = dplyr::filter(col_ponal_url,
                       crime_id == c_id)[3]
    )
  df <- as.data.table(df)
  df[,`:=`(fecha_hecho, as.IDate(as.POSIXct(df$fecha_hecho,
                                                format = '%d/%m/%Y')))]
  ### Eliminar columnas de municipio y departamento
  df[,(1:2) := NULL]
  var_name <- paste0('crim_col_',c_id)
  assign(var_name, df)
  as.name(var_name)
  do.call(usethis::use_data,list(as.name(var_name), overwrite = TRUE,
                                 compress = 'bzip2'))
}
