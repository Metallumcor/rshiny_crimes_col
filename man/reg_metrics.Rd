% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_models.R
\encoding{UTF-8}
\name{reg_metrics}
\alias{reg_metrics}
\title{Medidas de rendimiento para modelos de regresión}
\usage{
reg_metrics(
  fit,
  split_out,
  objective = c("validation", "train"),
  metrics_out = "all",
  ...
)
}
\arguments{
\item{fit}{El modelos de clasificación ajustado}

\item{split_out}{La partición del conjunto de datos en
entrenamiento/validación/prueba}

\item{objective}{Seleccionar entrenamiento "train" o validación "validation"}

\item{metrics_out}{Vector de caracteres. Incluye los nombres de las
métricas a incluir en la salida
\describe{
    \item{rmse}{Root mean squared error.}
}}

\item{...}{Argumentos adicionales}
}
\value{
Una lista con las métricas seleccionadas o solo el número
    correspondiente a la medida única elegida.
}
\description{
Medidas de rendimiento para modelos de regresión
}
