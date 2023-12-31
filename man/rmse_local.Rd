% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmse_map.R
\encoding{UTF-8}
\name{rmse_local}
\alias{rmse_local}
\title{Obtención de RMSE especificó de acuerdo a un factor}
\usage{
rmse_local(
  fit_object,
  type,
  split_out,
  factor_split,
  kind = c("validation", "train"),
  ...
)
}
\arguments{
\item{fit_object}{El objeto retornado por la salida de ajuste automático
correspondiente}

\item{type}{El tipo de modelo usado. Al momento de realización se reconoce
"arima", "arimax", "linear", "arima_linear", "nn_1hl", "nn_2hl" y "var"}

\item{split_out}{La partición de los datos en entrenamiento, validación
y prueba}

\item{factor_split}{El nombre de la columna que contiene el factor usado
para distribuir el RMSE}

\item{kind}{El objetivo del cálculo. Puede ser "train" o "validation"}

\item{...}{Parámetros adicionales}
}
\value{
Un \code{tibble} que incluye el factor, el RMSE por nivel y la
   desviación estandar de la variable objetivo en cada nivel
}
\description{
Los modelos de regresión/predicción de tipo numérica pueden
    son capaces de exponer su medida de desempeño característica (RMSE)
    a través de los niveles de un factor que permite clasificar las
    observaciones en grupos mutuamente excluyentes y esta función permite
    dicha presentación
}
