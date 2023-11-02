#' @title Viridis main function (inner)
#'
#' @description Exported function from viridis package
#' @param n Cantidad de colores
#' @param alpha Transparencia
#' @param begin Inicio de la paleta
#' @param end Fin de la paleta
#' @param direction Direccion de la paleta
#' @param option Interno
#' @export
#' @importFrom grDevices colorRamp
viridis <- function (n, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
{
  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }
  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }
  if (n == 0) {
    return(character(0))
  }
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }
  option <- switch(EXPR = option, A = "A", magma = "A", B = "B",
                   inferno = "B", C = "C", plasma = "C", D = "D", viridis = "D",
                   E = "E", cividis = "E", F = "F", rocket = "F", G = "G",
                   mako = "G", H = "H", turbo = "H", {
                     warning(paste0("Option '", option, "' does not exist. Defaulting to 'viridis'."))
                     "D"
                   })
  map <- viridis.map[viridis.map$opt == option, ]
  map_cols <- grDevices::rgb(map$R, map$G, map$B)
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n))/255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}

#' @title Plasma palette
#'
#' @description Exported object from
#' @param n Cantidad de colores
#' @param alpha Transparencia
#' @param begin Inicio de la paleta
#' @param end Fin de la paleta
#' @param direction Direccion de la paleta
#' @export
plasma <- function (n, alpha = 1, begin = 0, end = 1, direction = 1)
{
  viridis(n, alpha, begin, end, direction, option = "plasma")
}
