#' @encoding UTF-8
#' @title One-hot encoding para conjuntos de datos mixtos
#'
#' @description Función tomada del paquete \code{mltools}. Originalmente
#'     trabaja con \code{data.table} pero en esta versión acepta
#'     \code{data.frame} y su salida es un \code{data.frame}
#'
#' @param dt Un \code{data.table} o \code{data.frame}
#' @param cols Vector con el nombre de las columnas a ser codificadas. Por
#'     defecto se aplica a todos los factores
#' @param sparsifyNAs Lógico. Convertir los NA en 0
#' @param naCols Lógico. Crear o no columnas que indiquen presencia de NA
#'     (de existir)
#' @param dropCols Lógico. El resultado debe incluir o no las columnas
#'     originales sin codificar
#' @param dropUnusedLevels Lógico. Crear o no columnas de solo ceros para
#'     niveles de factores no usados
#'
#' @return El \code{data.frame} codificado
#' @export
#'
#' @examples
#' one_hot(iris)
one_hot <- function (dt, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE,
          dropCols = TRUE, dropUnusedLevels = FALSE)
{
  if(class(dt) != "data.table") dt <- data.table::as.data.table(dt)
  OHEID <- NULL
  if (cols[1] == "auto")
    cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) &
                                        !is.ordered(x)))]
  if (length(cols) == 0)
    return(dt)
  tempDT <- dt[, cols, with = FALSE]
  tempDT[, `:=`(OHEID, .I)]
  for (col in cols) data.table::set(tempDT, j = col, value = factor(paste(col,
                                                              tempDT[[col]], sep = "_"), levels = paste(col, levels(tempDT[[col]]),
                                                                                                        sep = "_")))
  melted <- data.table::melt(tempDT, id = "OHEID", value.factor = T, na.rm = TRUE)
  if (dropUnusedLevels == TRUE) {
    newCols <- data.table::dcast(melted, OHEID ~ value, drop = T, fun.aggregate = length)
  }
  else {
    newCols <- data.table::dcast(melted, OHEID ~ value, drop = F, fun.aggregate = length)
  }
  newCols <- newCols[tempDT[, list(OHEID)]]
  newCols[is.na(newCols[[2]]), `:=`(setdiff(paste(colnames(newCols)),
                                            "OHEID"), 0L)]
  if (!sparsifyNAs | naCols) {
    na_cols <- character(0)
    for (col in cols) if (any(is.na(tempDT[[col]])))
      na_cols <- c(na_cols, col)
    if (!sparsifyNAs)
      for (col in na_cols) newCols[is.na(tempDT[[col]]),
                                   `:=`(intersect(levels(tempDT[[col]]), colnames(newCols)),
                                        NA_integer_)]
    if (naCols)
      for (col in na_cols) newCols[, `:=`(eval(paste0(col,
                                                      "_NA")), is.na(tempDT[[col]]) * 1L)]
  }
  result <- cbind(dt, newCols[, !"OHEID"])
  possible_colnames <- character(0)
  for (col in colnames(dt)) {
    possible_colnames <- c(possible_colnames, col)
    if (col %in% cols) {
      possible_colnames <- c(possible_colnames, paste0(col,
                                                       "_NA"))
      possible_colnames <- c(possible_colnames, paste(levels(tempDT[[col]])))
    }
  }
  sorted_colnames <- intersect(possible_colnames, colnames(result))
  setcolorder(result, sorted_colnames)
  if (dropCols == TRUE)
    result <- result[, !cols, with = FALSE]
  return(as.data.frame(result))
}
