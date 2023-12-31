% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_mod_selector.R
\encoding{UTF-8}
\name{tree_mod_selector}
\alias{tree_mod_selector}
\title{Ajuste de arboles de decisión para clasificación.}
\usage{
tree_mod_selector(
  split_out,
  scaled = FALSE,
  quiet = FALSE,
  auto_prune = TRUE,
  controls = NULL,
  ...
)
}
\arguments{
\item{split_out}{La partición entrenamiento/validación/prueba de los datos.
Las columnas de factores deben estar en su correspondiente formato.}

\item{scaled}{¿Los datos están escalados? De usar falso se realizará
su escalamiento. *Nota:* El escalamiento se realiza usando la
información disponible en entrenamiento.}

\item{quiet}{Lógico. Callar las salidas en consola de los modelos.}

\item{auto_prune}{Lógico. Realizar selección automática del parámetro de
complejidad para el podado.}

\item{controls}{Nulo o una función \code{rpart.control} con argumentos.
Consultar \code{\link[rpart]{rpart}} y \code{\link[rpart]{rpart.control}}.}

\item{...}{Argumentos adicionales pasados a \code{rpart} y otras funciones.
Uno de los casos especifico es el parámetro \code{cp} cuando
\code{auto_prune} es falso.}
}
\value{
Una lista que incluye el mejor árbol de decisión junto a la
    **accuracy** reportada en entrenamiento y validación.
}
\description{
Se utilizan las rutinas de particiones recursivas \code{rpart}
    del paquete homónimo para el **crecimiento** de arboles de clasificación
    binarios o múltiples. Cada árbol es **podado** para evitar el
    sobre-ajuste que sucede naturalmente en este tipo de clasificadores.
    En la salida se reportan las **accuracy** del árbol simplificado
    en entrenamiento y validación.

    *Nota* en versiones futuras se incluirá ajuste por CV para el
    pesos de los parámetros (usando \code{caret}, por ejemplo)

    TODO: Ejemplos
}
