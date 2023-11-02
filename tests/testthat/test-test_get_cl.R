# Prueba de las expresiones regulares para identificar columnas
test_that("get_cl identifica las columnas de lat y lng", {
  df_coords <- data.frame("lat" = rnorm(100), "lng" = rnorm(100))
  df_coords_mod <- df_coords
  lat_names <- c('latitude', 'LAT', 'latitud', 'lati')
  lng_names <- c('long', 'longitude', 'LNG', 'longitud')
  expected <- get_cl(df_coords = df_coords)

  for(i in 1:4){
    for(j in 1:4){
      names(df_coords) <- c(lat_names[i], lng_names[j])
      expect_equal(get_cl(df_coords = df_coords_mod), expected)
    }
  }
})
