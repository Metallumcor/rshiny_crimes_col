# Parametro general
pre_pro <- sp_crime_pre()

# Prueba de retorno correcto
test_that("sp_crime_cat retorna SpPolDF", {
  crime_s = c('Drogas', 'Asalto')
  date_s = c('2020-06', '2019-12')

  for (i in 1:2){
    for (j in 1:2){
      expect_type(sp_crime_cat(crime_s = crime_s[i],
                               date_s = date_s[j], sp_pre = pre_pro),
                  'S4')
    }
  }
})

# Control de fallos - Crimen seleccionado
test_that("sp_crime_cat se detiene por un crimen no encontrado",{
  crime_s <- c('Maracuya', 'Fresas', 'Peras')
  date_s <- '2019-01'
  for (i in 1:3){
    expect_error(sp_crime_cat(crime_s = crime_s[i], date_s = date_s,
                              sp_pre = pre_pro))
  }
})

# Control de fallos - Crimen seleccionado
test_that("sp_crime_cat se detiene por un crimen no encontrado",{
  crime_s <- 'Drogas'
  date_s <- c('1995-01','1999-03', '1998-03')
  for (i in 1:3){
    expect_error(sp_crime_cat(crime_s = crime_s, date_s = date_s[i],
                              sp_pre = pre_pro))
  }
})

