# Parametros generales
list_filter <- list(tipo_de_hurto = 'HURTO RESIDENCIAS',
                    fecha_hecho = data.table::as.IDate('2020-01-01'))
hurtos <- test2021:::dt_filter_crime(crim_col_hurto_1, list_filter)
hurtos <- cod_dane(hurtos, 'codigo_dane')

# Prueba de integridad en el tipo de inputs
test_that("db_sp_merge falla con los tipos de input inadecuados", {
   expect_error(db_sp_merge(col_mpio, hurtos,
   c('DPTO_CCDGO', 'MPIO_CCDGO')))
   expect_error(db_sp_merge(db_crime_london_a, lon_poly_ward_b, 'DISTRICT'))
})
