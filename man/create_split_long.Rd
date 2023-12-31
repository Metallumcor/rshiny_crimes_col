% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_models.R
\encoding{UTF-8}
\name{create_split_long}
\alias{create_split_long}
\title{Creación de particiones entrenamiento-validación-prueba para
    datos longitudinales.}
\usage{
create_split_long(Y, X, factors, split_prop = c(0.9, 0.05, 0.05))
}
\arguments{
\item{Y}{Un vector que contiene las respuestas.}

\item{X}{Un \code{data.frame}, \code{data.table} o \code{matrix} que guarda
la información de características o predictoras.}

\item{factors}{Los factores que determinan los objetos medidos de forma
repetida en el tiempo.}

\item{split_prop}{La proporción (tamaños) de cada conjunto que se ingresan en
el orden: entrenamiento/validación/prueba. Debe cuidarse que la suma de
exactamente uno.}
}
\value{
Una lista que contiene las particiones de Y y X, si la última es
    proporcionada.
}
\description{
Dado que los paquete \code{caret} y \code{tidymodels} no tiene
    una función nativa para crear particiones completas del conjunto de
    datos en las tres partes mencionadas, se proporciona en este paquete
    una rutina que se ajusta a las necesidades del paquete. Este tipo de
    partición amplia el alcance de \code{create_split_ts} al
    admitir las mediciones repetidas por unidad (factor unicó o
    combinaciones)

    *Importante:* Se da por hecho que los datos son organizados por una
    columna temporal. Se asume que todos los factores estan presentes en
    todas las unidades temporales
}
\examples{
# Se carga una fraccion de bog_hurto_motos
df_puntos <- bog_hurto_motos \%>\%
    dplyr::filter(CODIGO_DIV1 == c("01","02")) \%>\%
    data.table::as.data.table()
# Se agrega por fechas y codigo de localidad
df_agregado <- df_puntos[,.N,.(FECHA,CODIGO_DIV1)]
# Definimos Y y X
Y <- df_agregado$N
X <- df_agregado[,-"N"]
# Realizamos la particion
split_long <- create_split_long(Y = Y, X = X, factors = "CODIGO_DIV1",
    split_prop = c(0.6,0.2,0.2))
}
