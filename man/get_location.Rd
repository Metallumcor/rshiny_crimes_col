% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\encoding{UTF-8}
\name{get_location}
\alias{get_location}
\title{Obtener el objeto donde pertenece un punto}
\usage{
get_location(v, sp_ob)
}
\arguments{
\item{v}{Coordenadas del punto (long, lat)}

\item{sp_ob}{Objeto \code{SpatialPolygonsDataFrame}.}
}
\description{
Examina una lista de polígonos y devuelve aquel donde
    pertenece (de existir)
}
