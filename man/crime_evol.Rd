% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs_plotly.R
\encoding{UTF-8}
\name{crime_evol}
\alias{crime_evol}
\title{Gráfico de proyecciones ACP delitos}
\usage{
crime_evol(pca_list, selection, axis_names = NULL)
}
\arguments{
\item{pca_list}{Una lista con salidas de princomp}

\item{selection}{Carácter. Nombre de la unidad seleccionada}

\item{axis_names}{Dos carácteres que represetan los nombres de los ejes}
}
\value{
Un gráfico de plotly
}
\description{
Gráfico de proyecciones ACP delitos
}
\examples{
# Obtencion de los pca por localidad
objeto <- pca_crim(data = test2021::siedco_wc[Delito == "HURTO DE CELULARES",],
years = unique(data.table::year(test2021::siedco_wc[Delito == "HURTO DE CELULARES",FECHA])),
id_col = "NOM_DIV1",
date_col = "FECHA",
tz_col = "RANG_DIA",
val_col = "Numero Hechos")
# Projecciones sobre 2012
objeto_2 <- proj_pca(x = objeto$pca_result, year_base = 2012, n_comp = 4)
# Grafico
crime_evol(objeto_2,"PUENTE ARANDA",objeto$axis_names)
}
