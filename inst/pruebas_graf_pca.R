library(ProtoJunio)
## El PCA debe tomar las localidades como individuos
datos_pca <- siedco_full
datos_pca <- datos_pca %>%
  dplyr::mutate(Dia = as.numeric(format(Fecha,"%d"))) %>%
  dplyr::filter(!Delito %in% c("HURTO PIRATERIA TERRESTRE",
                               "HURTO A ENTIDADES FINANCIERAS",
                               "HURTO ABIGEATO",
                               "SECUESTRO",
                               "TERRORISMO")) %>%
  dplyr::filter(!UPZ %in% c("-","SIN LOCALIZACION")) %>%
  dplyr::mutate(Localidad = substr(Localidad,6,nchar(Localidad)))
filtrado_pca <- dplyr::filter(datos_pca, Delito == "HURTO DE CELULARES")
rm(datos_pca)
filtrado_pca <- filtrado_pca[,.(N=sum(`Numero Hechos`)),
                            .(Localidad, UPZ, Fecha)]
filtrado_pca <- fill_long(filtrado_pca$N, filtrado_pca[,-"N"],"Fecha")
base_list <- test2021::split_by_factors(as.data.frame(filtrado_pca),
                                        factor_split = c("Localidad","UPZ"))
base_list <- lapply(base_list, function(x) add_laggs(x,x$y,30))
base_list <- dplyr::bind_rows(base_list)
setDT(base_list)
# Agregar elementos de fechas
base_list[,`:=`(AGNO = year(Fecha), MES = month(Fecha), DIA =  mday(Fecha),
             SEMANA = isoweek(Fecha)),]
base_list[,Fecha := NULL,]
# Agregar elementos de Id
base_df <- as.data.frame(base_list)
rm(filtrado_pca,base_list)
#
pca_siedco <- ProtoJunio::proc_pca(x = base_df[base_df$AGNO == 2012,],
                                   clave = c("UPZ","MES","DIA"),
                                   cuali_sup = c("Localidad"))
ProtoJunio::graf_pca(pca_siedco,100,var_color = "Localidad", var_centroide = "Localidad",interactivo = T)

## Benchmark
system.time({
  temp <- month(siedco_full$Fecha)
  temp <- as.factor(month)
})
microbenchmark::microbenchmark(
  data_table = {
    temp <- month(siedco_full$Fecha)
    temp <- as.factor(temp)
  },
  base = {
    temp <- format(siedco_full$Fecha,"%M")
  },
  times = 10
)
