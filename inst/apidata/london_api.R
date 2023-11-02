library(dplyr)
library(test2021)

# Funcion para llamar datos
london_api <- function(x, y){
  # Validez de fechas
  data_aval <- available_dates()
  # Validez de distritos
  district <- unique(get(y, x))
  # Carga temporal de datos
  temp_list <- list()
  # Ciclo sobre distritos
  for (i in 1:length(districts)){
    # Ciclo sobre fechas dentro de un mismo distrito
    temp_list[[i]] <- lapply(data_aval,
                             function(x) {
                               get_crime_london_unit(districts[i],
                                                     data = lon_poly_ward_b,
                                                     geo_div = 'DISTRICT',
                                                     date = x)
                             }
    )
    # Unir todas los data.frame
    temp_list[[i]] <- dplyr::bind_rows(Filter(Negate(is.null),
                                              temp_list[[i]]))
  }

  temp_list
}

temp_list <- london_api(x = lon_poly_ward_b, y = 'DISTRICT')
districts <- unique(lon_poly_ward_b$DISTRICT)

if(!exists('db_crime_london_a')){
  db_crime_london_a <- temp_list
  names(db_crime_london_a) <- districts
  use_data(db_crime_london_a)
} else {
  if(exists('db_crime_london_a2')){
    warning(message = 'El archivo existe, sera sobreescrito')
  }
  db_crime_london_a2 <- temp_list
  names(db_crime_london_a2) <- districts
  use_data(db_crime_london_a2, overwrite = TRUE)
}
