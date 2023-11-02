#' @encoding UTF-8
#' @title Creación de categorías (intervalos) a partir de un vector numérico
#'
#' @description Procesa un vector numérico en una serie de intervalos definidos
#'     por el método seleccionado por el usuario, y según el cual se deberán
#'     incluir los argumentos adicionales correspondientes. Para convertir
#'     los intervalos a categorías con nombres se debe aplicar al objeto
#'     devuelto la función \code{ctgr2nmd}
#'
#' @param x El vector numérico a convertir
#' @param type El método usado para crear los intervalos. Ver \code{...}
#' @param ... Argumentos adicionales que son pasados de acuerdo con \code{type}
#'     \describe{
#'         \item{probs}{cuando \code{type = "quantiles"} se debe pasar los
#'         cortes de los cuantiles requeridos (puede omitirse 0 y 1) como
#'         vector numérico}
#'         \item{n}{cuando \code{type = "jenks"} se requiere la cantidad
#'         positiva entera de clases}
#'         \item{breaks}{cuando \code{type = "subjective"} se debe pasar
#'         los puntos de cortes para crear los intervalos como vector
#'         numérico}
#'     }
#' @return Una lista con nombres que contiene los intervalos (\code{intrvls})
#'     y los cortes usados para crearlos (\code{brks})
#' @export
#' @importFrom stats quantile
#'
#' @examples
#' example_vect <- c(-1,-1,-10,1,0,2,3,10,5,11,12,15,20,-4)
#' num2ctgr(x = example_vect, type = "jenks", n = 2)
#' num2ctgr(x = example_vect, type = "quantiles", probs = c(0.3,0.6))
num2ctgr <- function(x,
                     type = c("quantiles", "jenks", "subjective"),
                     ...){
  dots <- list(...)
  list2env(dots, envir = environment())
  if(!type %in% c("quantiles", "jenks", "subjective"))
    stop("Tipo de intervalos no reconocido")
  if(type == "quantiles"){
    if(!exists("probs"))
      stop("Para cuantiles se deben proporcionar el argumento probs")
    if(!0 %in% probs) probs <- c(0,probs)
    if(!1 %in% probs) probs <- c(1,probs)
    brk_x <- sort(stats::quantile(x, probs))
  }
  if(type == "jenks"){
    if(!exists("n"))
      stop("Para jenks se debe proporcionar el argumento n")
    brk_x <- classIntervals(x, n = n, style = "jenks")$brks
  }
  if(type == "subjective"){
    if(!exists("breaks"))
      stop("Para asignacion subjetiva se requiere el argumento breaks")
    if(min(breaks)>min(x) | max(breaks)<max(x))
      stop("breaks definido de forma inconsistente (x no esta totalmente contenido)")
    brk_x <- breaks
  }
  intrvl_x <- cut(x, breaks = brk_x, include.lowest = TRUE)
  t_adv <- table(intrvl_x)
  if(any(t_adv/sum(t_adv)>0.6)) warning("Existe por lo menos una clase con 60 porciento de los casos")
  return(list(intrvls = intrvl_x, brks = brk_x))
}

#' @encoding UTF-8
#' @title Enviar nuevas observaciones a intervalos ya existentes
#'
#' @description Dado un objeto retornado por \code{num2ctgr}, al ingresar
#'     un vector numérico en esta función se asignarán a dichos números
#'     los intervalos correspondientes que se encuentren en \code{intrvls}.
#'     Si sucede que un número sea inferior (o mayor) al intervalo con
#'     los números más pequeños (o grandes) se asumirá pertenencia al
#'     primero (ultimó) de ellos.
#'
#' @param x Un vector de números
#' @param intrvls Un objeto devuelto por la función \code{num2ctgr}
#'
#' @return Una lista con nombres que contiene los intervalos (\code{intrvls})
#'     y los cortes usados para crearlos (\code{brks})
#' @export
#'
#' @examples
#' example_vect <- c(-1,-1,-10,1,0,2,3,10,5,11,12,15,20,-4)
#' example_int <- num2ctgr(x = example_vect, type = "jenks", n = 2)
#' example_vect2 <- c(1,10,100,-20)
#' add2ctgr(example_vect2, example_int)
add2ctgr <- function(x, intrvls){
  brks <- intrvls$brks
  intrvls_look <- unique(sort(intrvls$intrvls))[findInterval(x, brks, all.inside = TRUE)]
  t_adv <- table(intrvls_look)
  if(any(t_adv/sum(t_adv)>0.6)) warning("Existe por lo menos una clase con 60 porciento de los casos")
  return(list(intrvls = intrvls_look, brks = brks))
}

#' @encoding UTF-8
#' @title Asignar nombres a las categorías ordenadas (intervalos)
#'
#' @param intrvls Un objeto devuelto por la función \code{num2ctgr} o
#'     \code{add2ctgr}
#' @param names_intr Los nombres (ordenados) a ser asignados
#'
#' @return Un vector de factores con niveles ordenados (menor a mayor)
#' @export
#'
#' @examples
#' example_vect <- c(-1,-1,-10,1,0,2,3,10,5,11,12,15,20,-4)
#' example_int <- num2ctgr(x = example_vect, type = "jenks", n = 2)
#' ctgr2nmd(example_int, c("Bajo", "Alto"))
ctgr2nmd <- function(intrvls, names_intr){
  if(length(names_intr) != length(levels(intrvls$intrvls)))
    stop("La cantidad de nombres debe coincidir con la cantidad de intervalos")
  named_intrvls <- intrvls$intrvls
  levels(named_intrvls) <- names_intr
  return(named_intrvls)
}
