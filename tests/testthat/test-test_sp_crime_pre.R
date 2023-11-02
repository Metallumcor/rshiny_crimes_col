# Test para verificar columna de div geopol en el SpPolDF
test_that("sp_crime_pre para archivos sin geo_div en base", {
  bd1 <- lon_poly_ward_a
  bd2 <- lon_poly_ward_b
  error_geo_div <- 'municipio'

  expect_error(sp_crime_pre(city = bd1,
                            geo_div = error_geo_div),
               "geo_div: Categoria no encontrada en los datos")
  expect_error(sp_crime_pre(city = bd2,
                            geo_div = error_geo_div),
               "geo_div: Categoria no encontrada en los datos")
})

# Test para verificar integridad de la salida - SpPolDF
test_that("sp_crime_pre retorna una lista con un SpPolDF",{
  bd <- lon_poly_ward_b
  geo_div <- 'DISTRICT'

  eval_1 <- sp_crime_pre(city = bd, geo_div = geo_div)
  expect_type(eval_1[[1]], 'S4')
})

# Test para verificar integridad de la salida - geo_div
test_that("sp_crime_pre retorna una lista con geo_div",{
  bd <- lon_poly_ward_b
  geo_div <- 'DISTRICT'

  eval_1 <- sp_crime_pre(city = bd, geo_div = geo_div)
  expect_equal(eval_1[[2]], geo_div)
})
