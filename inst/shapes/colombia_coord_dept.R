### Lectura del diccionario. Departamentos - coordenadas.
library(usethis)

PATH <- system.file('shp', 'colombia_censo_2018', package = 'test2021')
col_coor_dept <- read.csv2(paste0(PATH, '/coordenadas_dept.csv'))
names(col_coor_dept) <- c('DPTO_CCDGO', 'DEP_NOM', 'lat', 'lng')
col_coor_dept$DPTO_CCDGO <- as.character(col_coor_dept$DPTO_CCDGO)
col_coor_dept[col_coor_dept$DPTO_CCDGO == '5',1] <- '05'
col_coor_dept[col_coor_dept$DPTO_CCDGO == '8',1] <- '08'
col_coor_dept[col_coor_dept$DEP_NOM == 'NARIÑO', 'DEP_NOM'] = "NARI\u00D1O"
usethis::use_data(col_coor_dept, overwrite = TRUE)
