### Cargar librerias requeridas
library(sf)
library(dplyr)
library(rvest)
### Cambiar el PATH de acuerdo a donde esten los shapefiles
PATH <- getwd()
### Dias de la semana para filtrar
week_day <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes",
              "Sabado", "Domingo")
### Sentencia SQL para filtrar y poder procesar rapidamente
QUERY <- "SELECT * FROM \"Velocidades_Bitcarrier_Marzo_2021\" WHERE DIA_SEMANA = "
### Ciclo para lectura por dias y procesamiento del conjunto
### Se agrega por INICIO (dia), HORA, NAME_FROM (pnt partida),
### NAME_TO (pnt llegada) y DIA_SEMANA.
for (day in week_day){
  hway <- sf::st_read(paste0(PATH,"/shp/vias"),
                      layer = "Velocidades_Bitcarrier_Marzo_2021",
                      query = paste0(QUERY,"'",day,"'"))

  agg_hway <- hway %>%
    group_by(INICIO, HORA, NAME_FROM, NAME_TO, DIA_SEMANA) %>%
    summarize(VEL_MEDIA = mean(VEL_PROMED, na.rm = TRUE),
              DISTANCIA_MEDIA = mean(DISTANCE, na.rm = TRUE))
  assign(day, agg_hway)
}
### Segundo ciclo para agregar todos los dias segun las otras variables
### Especial atencion al DIA_SEMANA
for (day in week_day){
  day2 <- get(day) %>%
    group_by(HORA, NAME_FROM, NAME_TO, DIA_SEMANA) %>%
    summarize(VEL_MEDIA = mean(VEL_MEDIA, na.rm = TRUE),
              DISTANCIA_MEDIA = mean(DISTANCIA_MEDIA, na.rm = TRUE))
  assign(paste0(day,"2"), day2)
}
### Base de datos completa para Marzo - por dia
vias_velocidad_marzo <-dplyr::bind_rows(Lunes,Martes,Miercoles,Jueves,Viernes,Sabado,Domingo)
### Base de datos completa para Marzo - dia de la semana
vias_velocidad_dias <-dplyr::bind_rows(Lunes2,Martes2,Miercoles2,Jueves2,Viernes2,Sabado2,Domingo2)

### Grafica para visualizar horas pico
vias_velocidad_dias %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(HORA) %>%
  dplyr::summarise(VEL_MEDIANA_TOTAL = median(VEL_MEDIA)) %>%
  ggplot2::ggplot(ggplot2::aes(x = factor(HORA), y = VEL_MEDIANA_TOTAL)) +
  ggplot2::geom_col()
vias_velocidad_dias %>%
  sf::st_drop_geometry() %>%
  ggplot2::ggplot(ggplot2::aes(x = factor(HORA), y = VEL_MEDIA)) +
  ggplot2::geom_violin()

vias_velocidad_dias %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(NAME_FROM == "AUTONORTE") %>%
  dplyr::filter(DIA_SEMANA == "Lunes") %>%
  ggplot2::ggplot(ggplot2::aes(x = factor(HORA), y = VEL_MEDIA)) +
  ggplot2::geom_violin()

### Lectura de las avenidas principales de Bogota
site <- "https://es.wikipedia.org/wiki/Categor%C3%ADa:Avenidas_de_Bogot%C3%A1"
txt <- read_html(site) %>%
  html_nodes("#mw-pages") %>%
  html_nodes("a") %>%
  html_text() %>%
  iconv(from="UTF-8",to="ASCII//TRANSLIT") %>%
  toupper()
### Tratamiento de texto
# Eliminar parentesis y todo dentro de estos
txt <- gsub("CIUDAD |LA |EL |LOS |LAS |DE |\\s*\\([^\\)]+\\)","",txt)
txt <- gsub("AVENIDA","AV.",txt)
txt <- gsub("CARRERA","KR",txt)
txt <- gsub("AV. KR","KR",txt)
txt <- gsub("AV. NQS","NQS",txt)
txt <- gsub("CALLE","CL",txt)
txt <- gsub("PRIMERO","P.",txt)
txt <- gsub("AUTOPISTA NORTE","AUTONORTE",txt)
txt <- gsub("AV. CALI","KR 68",txt)
txt <- gsub("AV. LIMA","CL 19",txt)
txt <- gsub("SEPTIMA","7",txt)
# Fin - Eliminacion de espacios
txt <- gsub(" ","",txt)
#' NOTA: No todos los nombres se corresponden con los de la base de datos
#' El resto de procedimiento es extenso, pero realizable.
#' Realizar consultas, en caso de duda, con el siguiente comando
#' Cambiar los terminos dentro del parentesis por lo requerido
# unique(grep("^.*?(68).*$", vias_velocidad_dias$NAME_FROM, value = TRUE))
### Matching contra las columnas NAMES_FROM y NAMES_TO
rmat_1 <- grep(paste0("^.*(",paste(txt,collapse = ")|("),")\\D$"),
     vias_velocidad_dias$NAME_FROM)
rmat_2 <- grep(paste0("^.*(",paste(txt,collapse = ")|("),")\\D$"),
               vias_velocidad_dias$NAME_TO)
rmat <- union(rmat_1, rmat_2)
### Grupos horarios
# Valores
RANGO_DIA <- c("VALLE_MADRU", "PICO_MANANA", "VALLE_MAN_TAR",
               "PICO_TAR_NOC", "VALLE_NOCHE")
vias_velocidad_dias$HORA <- cut(vias_velocidad_dias$HORA,
                                breaks = c(0,6,9,15,20,24), right = FALSE)
levels(vias_velocidad_dias$HORA) <- RANGO_DIA
### Nueva reduccion: filtro + agregacion por rango del dia + dia semana
resumen_marzo <- vias_velocidad_dias[rmat,] %>%
  dplyr::group_by(DIA_SEMANA, HORA, NAME_FROM) %>%
  dplyr::summarise(VEL_MEDIA = mean(VEL_MEDIA, na.rm = TRUE),
                   DISTANCIA_MEDIA = mean(DISTANCIA_MEDIA, na.rm = TRUE))
### Limpiar vias que no estan dentro de la ciudad por alguna razon
resumen_marzo <- sf::st_transform(resumen_marzo,
                                  crs = sf::st_crs(test2021::bogota))
bogota_sf <- sf::st_as_sf(test2021::bogota)
resumen_marzo <- sf::st_intersection(resumen_marzo, bogota_sf)
movilidad_marzo_bog <- resumen_marzo
use_data(movilidad_marzo_bog, overwrite = TRUE)
