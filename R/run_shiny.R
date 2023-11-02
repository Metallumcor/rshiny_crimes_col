#' @encoding UTF-8
#' @title Mapas de crimen interactivos
#'
#' @description Las aplicaciones estan descritas en la viñeta
#'     \code{manual_de_uso}.
#'
#' @param to_run Nombre de la carpeta que contienen los archivos de la App.
#'     Llamar la función con cualquier argumento para ver las aplicaciones
#'     disponibles.
#' @export
run_shiny <- function(to_run) {

  valid_ex <- list.files(system.file("shiny", package = "test2021"))

  if(length(valid_ex) == 0){
    stop('Ninguna aplicacion encontrada, intente re-instalar el paquete',
         call.=FALSE)
  }

  valid_ex_msg <-
    paste0(
      "Las App aceptadas son: '",
      paste(valid_ex, collapse = "', '"),
      "'")

  if (missing(to_run) || !nzchar(to_run) ||
      !to_run %in% valid_ex) {
    stop(
      'Usar un nombre de App valido.\n',
      valid_ex_msg,
      call. = FALSE)
  }

  app_dir <- system.file("shiny", to_run, package = "test2021")
  runApp(app_dir, launch.browser = TRUE)
}
