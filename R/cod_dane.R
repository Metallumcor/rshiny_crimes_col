#' @encoding UTF-8
#' @title Desagregar los códigos DANE en departamento y municipio.
#' @description Función auxiliar cuyo fin es desglosar los códigos
#'     DANE municipales completos de bases de datos geo-estadísticas
#'     en dos columnas de \code{strings} que contienen el código
#'     del departamento y del municipio (3 dígitos).
#'
#' @param db_obj La base de datos en formato \code{data.frame} o
#'     \code{data.table}
#' @param cod_col El nombre que tiene la columna con los códigos
#' @param out_names Un vector de dos entradas que contiene el nombre
#'     de las columnas a crear. Primer argumento corresponde a
#'     departamento y segundo a municipio.
#'
#' @return El mismo \code{db_obj} con las columnas agregadas.
#' @export
#'
#' @examples
#' \donttest{
#' extorsiones <- cod_dane(crim_col_extorsion, 'codigo_dane')
#' }
cod_dane <- function(db_obj, cod_col, out_names = c('DPTO_CCDGO',
                                                    'MPIO_CCDGO')){
  if(!inherits(db_obj,'data.frame')){
    stop('db_obj solo puede ser: data.frame o data.table')
  }
  if(sum(out_names %in% names(db_obj)) != 0){
    stop('Los nombres suministrados en out_names ya existen en db_obj')
  }
  db_obj <- data.table::as.data.table(db_obj)
  selection <- get(cod_col, db_obj)
  db_obj[,(out_names[1]) := substr(selection, 1, 2),]
  db_obj[,(out_names[2]) := substr(selection, 3, 5),]
  db_obj
}
