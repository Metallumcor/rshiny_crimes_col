#' @encoding UTF-8
#' @title Adicionar columnas simuladas a un conjunto de datos.
#'
#' @description Función auxiliar que realiza un muestreo aleatorio con
#'     reemplazamiento de una variable categórica cuyos valores y probabilidades
#'     (PROP) de ocurrencia se encuentran consignadas en un \code{data.frame}.
#'     La cantidad de muestras tomadas corresponderá al total de filas del
#'     \code{data.frame} declarado como \code{df_base}.
#'
#'     TODO: Ejemplos.
#'
#' @param df_base El \code{data.frame} al cual se le adicionarán las columnas.
#' @param ... Múltiples \code{data.frame} de dos columnas, donde la primera
#'    contiene los niveles/categórias de la variable y la segunda, que debe
#'    llevar por nombre PROP, tendrá las probabilidades de cada una (deben
#'    sumar a uno). Es necesario que cada \code{data.frame} lleve un
#'    nombre (key) en el formato \code{df_prop_***}, donde los asteriscos deben
#'    ser reemplazados con el nombre que se espera obtener la nueva columna.
#'
#' @return El objeto \code{df_base} con la adición de las nuevas columnas.
#' @export
simular_variables_adicionales <- function(df_base, ...){
  dots <- list(...)
  if(length(dots) == 0) return(NULL)
  valid_df_names <- grep("^?(df_prop_).*$", names(dots), value = TRUE)
  if(length(valid_df_names) == 0){
    stop("Los argumentos deben llevar por nombre df_prop_***")
  }
  for(df_name in valid_df_names){
    df_selected <- dots[[df_name]]
    var_name <- toupper(sub("df_prop_","",df_name))
    df_base[[var_name]] <- sample(df_selected[,1],
           prob = df_selected$PROP,
           size = nrow(df_base),
           replace = TRUE)
  }
  df_base
}
