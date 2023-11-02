#' @encoding UTF-8
#' @title Join de una base datos y un \code{SpatialPolygonsDataFrame}
#'
#' @description Realiza un join a izquierda del objeto
#'     \code{SpatialPolygonsDataFrame} con la base de datos suministrada.
#'
#' @param db_obj Una base de datos en formato \code{data.frame} o
#'     \code{data.table}.
#' @param sp_obj Un objeto \code{SpatialPolygonsDataFrame} del paquete
#'     \code{sp}.
#'
#' @param merge_on Las columnas sobre las cuales se realiza el join.
#'
#' @return Un \code{SpatialPolygonsDataFrame}
#' @export
#'
#' @examples
#' \dontrun{
#' list_filter <- list(tipo_de_hurto = 'HURTO RESIDENCIAS',
#'     fecha_hecho = data.table::as.IDate('2020-01-01'))
#' hurtos <- test2021:::dt_filter_crime(crim_col_hurto_1, list_filter)
#' hurtos <- cod_dane(hurtos, 'codigo_dane')
#' colombia_extorsiones <- db_sp_merge(hurtos, col_mpio,
#' c('DPTO_CCDGO', 'MPIO_CCDGO'))
#' }
db_sp_merge <- function(db_obj, sp_obj, merge_on){
  if(!inherits(db_obj,'data.frame')){
    stop('db_obj solo puede ser: data.frame o data.table')
  }
  if(!inherits(sp_obj, 'SpatialPolygonsDataFrame')){
    stop('sp_oj solo puede ser: SpatialPolygonsDataFrame')
  }
  if((sum(merge_on %in% names(db_obj))) == 0 ||
     sum(merge_on %in% names(sp_obj@data) == 0)){
    stop('merge_on: La columna no existe en ambos objetos')
  }
  # Left join de los objetos con key_vals en merge_on
  id_sp <- rownames(sp_obj@data)
  dt_left <- data.table::as.data.table(sp_obj@data)
  dt_right <- data.table::as.data.table(db_obj)
  # Lo comentado causaba problemas con el orden de los municipios
  #data.table::setkeyv(dt_left, merge_on)
  #data.table::setkeyv(dt_right, merge_on)
  df_join <- as.data.frame(data.table::merge.data.table(dt_left, dt_right,
                                                        by = merge_on,
                                                        all.x = TRUE,
                                                        sort = FALSE))
  # Entrega del nuevo objeto espacial
  sp_merge_obj <- sp_obj
  sp_merge_obj@data <- df_join

  if(nrow(sp_merge_obj@data)!=length(id_sp)){
    warning('Duplicados en la bd, se deben asignar los id manualmente',
            call. = FALSE)
  } else {
    rownames(sp_merge_obj@data) <- id_sp
  }
  sp_merge_obj
}
