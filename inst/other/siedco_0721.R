### Principal
library(dplyr)
###
PATH <- "C:/Users/predi/Downloads" # PATH
siedco_files <- list.files(PATH, pattern = "^Siedco_Datos_Detallados") # Ruta
siedco_list <- list() # Lista de archivos
for(i in 1:length(siedco_files)){
  temp <- data.table::fread(paste0(PATH,"/",siedco_files[i]),
                            encoding = "UTF-8",
                            header = TRUE)
  names(temp)[1] <- "Fecha"
  siedco_list[[i]] <- temp
}
siedco_u <- dplyr::bind_rows(siedco_list) # Data.frame
saveRDS(siedco_u,paste0(PATH,"/siedco_full.rds"))
# PENDIENTE: Guardar e integrar con SQL

#### Secundarios. Datos filtrados
# Tratamiento, dentro de R
library(test2021)
siedco_u <- siedco_full
unique(siedco_u$Localidad)
l_drop <- ((!siedco_u$Localidad %in% c("20 - SUMAPAZ","")) & # Drop sumapaz y sin nombre
  !grepl("UPR",siedco_u$UPZ) & # Drop UPRs
  (!siedco_u$UPZ %in% c("SIN LOCALIZACION","-"))) # Drop UPZ sin nombres
siedco_u <- siedco_u[l_drop,]  # Eliminacion de 10% de registros
siedco_f <- siedco_u %>% # Algunos registros eliminados por mal cambios en UPZs
  dplyr::mutate(CODIGO_DIV1 = substr(Localidad,1,2)) %>%
  dplyr::inner_join(test2021::bogota@data[,c("CODIGO_DIV2","NOMBRE","CODIGO_DIV1")],
                   by = c("UPZ" = "NOMBRE", "CODIGO_DIV1" = "CODIGO_DIV1")) %>%
  dplyr::mutate(NOM_DIV1 = substr(Localidad,6,nchar(Localidad)),
                NOM_DIV2 = UPZ,
                SEXO = Sexo,
                FECHA = Fecha,
                RANG_DIA = `Rango del Dia`) %>%
  dplyr::mutate(NOM_DIV1 = iconv(NOM_DIV1,from="UTF-8", to = "ASCII//TRANSLIT"),
                NOM_DIV2 = iconv(NOM_DIV2,from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  subset(select = -c(Localidad, UPZ, Sexo, Fecha, `Rango del Dia`,
                     Año,`Nro del Mes`,`Nombre Dia`,Mes))
#saveRDS(siedco_f,paste0(PATH,"/siedco_redux.rds"))
#readRDS(paste0(PATH,"/siedco_redux.rds"))
#### Datos filtrados con coordenadas. Usar lo anterior
# Funcion muestreo
temp_sp <- function(sp_data, data, name_join, name_count){
  samples <- list()
  for (i in 1:nrow(data)) {
    psub <- sp_data[sp_data@data[[name_join]]== data[[name_join]][i],]
    psamp <- try( sp::spsample(psub, n = data[[name_count]][i], type = "random", iter = 10) )
    samples[[i]] <-
      sp::SpatialPointsDataFrame(psamp, data.frame(rep(psub[[name_join]],length(psamp))))
  }
  samples <- do.call("rbind", samples)
  points_sample <- data.frame(sp::coordinates(samples))
  names(points_sample) <- c("LNG","LAT")
  points_sample
}
# Funcion de agregacion y join
temp_funagg <- function(sp_data, data, name_join, name_sum, name_filt, val_fit){
  #
  data_c <- data.table::copy(data)
  data_c <- data_c %>%
    dplyr::arrange(!!as.name(deparse(name_join)))
  #
  data_agg <- data_c[eval(name_filt) == val_fit,
                     .N,
                     .(eval(name_join))]
  names(data_agg)[1] <- deparse(name_join)
  #
  points_sim <- temp_sp(sp_data, data_agg, deparse(name_join), "N")
  #
  data_c <- data_c %>%
    dplyr::filter(!!as.name(deparse(name_filt)) == val_fit) %>%
    cbind(points_sim)
  data_c
}
# Creacion de objeto en lista
siedco_list <- list()
for(delito in unique(siedco_f$Delito)){
  siedco_list[[delito]] <- temp_funagg(test2021::bogota, siedco_f, quote(CODIGO_DIV2), quote(`Numero Hechos`),
              quote(Delito), delito)
}
siedco_wc <- dplyr::bind_rows(siedco_list)
siedco_wc$Delito[grepl("HURTO DE CELULARES",siedco_wc$Delito)]<-"HURTO DE CELULARES"
usethis::use_data(siedco_wc, overwrite = TRUE)
