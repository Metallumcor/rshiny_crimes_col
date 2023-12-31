% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lin_arima_selector.R
\encoding{UTF-8}
\name{lin_arima_selector}
\alias{lin_arima_selector}
\title{Ajuste de modelo experimental en dos etapas lineal + ARIMA
    con validación por RMSE.}
\usage{
lin_arima_selector(
  split_out,
  factor_split,
  dates_col = NULL,
  step_selection = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{split_out}{La partición entrenamiento/validación/prueba de los datos.}

\item{factor_split}{Los nombres de las columnas usadas para identificar
los objetos medidos en el tiempo.}

\item{dates_col}{El nombre de la columna que contiene las fechas.}

\item{step_selection}{¿Se debe probar subconjuntos de variables para el
modelo lineal?}

\item{quiet}{Callar las salidas en consola de los modelos.}
}
\value{
Una lista que incluye los modelos ARIMA de residuos (\code{arima_fit}),
    el modelo lineal (\code{lin_fit}), y los
    RMSE conjuntos obtenidos en entrenamiento y validación
}
\description{
Este es un algoritmo experimental que corre en primer lugar
    los modelos lineales automáticos (\code{lin_mod_selector})
    para el conjunto de datos solicitado
    y a los cuales se les extrae sus residuales para ajustar modelos
    ARIMA (\code{arima_mod_selector}). La variable obtenida
    es una combinación lineal del pronosticó lineal más los residuales
    ARIMA estimados.

    No se realizan múltiples pases de validación.

    TODO: Ejemplos y múltiples pases de validación.
}
