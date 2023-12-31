% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_crime_cat.R
\encoding{UTF-8}
\name{sp_crime_cat}
\alias{sp_crime_cat}
\title{Obtener datos espaciales por tipo de crimen y
    fecha - Londres.}
\usage{
sp_crime_cat(
  crime_s,
  date_s,
  sp_pre,
  df = db_crime_london_a,
  crim_col = "category",
  date_col = "month"
)
}
\arguments{
\item{crime_s}{Nombre de la categoría de crimen. En el caso de
Londres se puede llamar a \code{fast_lon_a} (uso interno, ver
manual) para consultar los posibles valores.}

\item{date_s}{Fecha de filtrado. En los casos estándar de
Londres se espera un formato 'AAAA-MM' en numero.}

\item{sp_pre}{Una lista devolvida por \code{\link{sp_crime_pre}}.}

\item{df}{Un \code{data.frame} con información geo-referenciada
de crimen por categorías.}

\item{crim_col}{Un string que contiene el nombre del campo que
contiene las categorías en \code{df}.}

\item{date_col}{Un string que contiene el nombre del campo que
contiene las fechas en \code{df}.}
}
\value{
Un \code{SpatialPolygonsDataFrame}
}
\description{
Esta función esta dirigida para el uso interno
    de una Shiny App, no obstante, se exporta para otras
    aplicaciones. Mezcla una base de datos de crimen en formato
    \code{list} o \code{df}, con un objeto \code{SpatialPolygonsDataFrame}
    correspondiente al archivo de la ciudad seleccionada, a partir
    de una selección de categoría de crimen y fecha. Se requiere
    una llamada inicial a la función \code{\link{sp_crime_pre}} para
    la lista del argumento \code{sp_pre}.
}
\examples{
\dontrun{
df_pre <- sp_crime_pre()
sp_crime_cat('Drogas', '2020-01', df_pre)
}
}
