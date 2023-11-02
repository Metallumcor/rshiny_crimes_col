#' @encoding UTF-8
#' @title Mapas de crimen interactivos - Previzualisación en dispositivos moviles
#'
#' @description Las aplicaciones estan descritas en la viñeta
#'     \code{manual_de_uso}. Esta función es un envolvente a
#'     \code{preview_mobile} que evita el trabajo de identificar ruta/archivos
#'     de la aplicación requerida. No obstante es necesario realizar los
#'     pasos restantes en terminal como se describe
#'     [acá](https://rinterface.github.io/shinyMobile/articles/shinyMobile_tools.html).
#'
#' @param to_run Nombre de la carpeta que contienen los archivos de la App.
#'     Llamar la función con cualquier argumento para ver las aplicaciones
#'     disponibles.
#' @param device El nombre del dipositivo movil que será emulado
#' @export
preview_shinyMobile <- function(to_run, device) {

  valid_ex <- list.files(system.file("shiny", package = "test2021"))
  valid_ex <- grep("^?(mobile).*", valid_ex, value = TRUE)

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
  preview_mobile(appPath = app_dir, device = device)
}

#' @title Exported function from shinyMobile
#' @param appPath Direccion de la app
#' @param url URL de la app
#' @param port Puerto
#' @param device Tipo de movil a imitar
#' @param color Color del movil
#' @param landscape Interno
#' @export
preview_mobile <- function (appPath = NULL, url = NULL, port = 3838, device = c("iphoneX",
                                                              "galaxyNote8", "iphone8", "iphone8+", "iphone5s", "iphone5c",
                                                              "ipadMini", "iphone4s", "nexus5", "galaxyS5", "htcOne"),
          color = NULL, landscape = FALSE)
{
  if (!is.numeric(port))
    stop("Port must be numeric")
  if (!is.null(appPath) & !is.null(url))
    stop("Choose either local or online app")
  if (is.null(appPath) & is.null(url))
    stop("Choose at least one option")
  if (!is.null(appPath) & is.null(port))
    stop("Please set a port when running locally")
  appHeight <- switch(device, iphoneX = if (!landscape) 812 else 375,
                      galaxyNote8 = if (!landscape) 822 else 400, iphone8 = if (!landscape) 670 else 375,
                      `iphone8+` = if (!landscape) 736 else 415, iphone5s = if (!landscape) 570 else 325,
                      iphone5c = if (!landscape) 570 else 325, ipadMini = if (!landscape) 770 else 580,
                      iphone4s = if (!landscape) 485 else 325, nexus5 = if (!landscape) 570 else 320,
                      galaxyS5 = if (!landscape) 570 else 320, htcOne = if (!landscape) 570 else 320)
  if (!is.null(appPath)) {
    message("Copy in your terminal (not the R console): R -e \"shiny::runApp('",
            appPath, "', port = ", port, ")\"")
    invisible(readline(prompt = "Press [enter] to continue"))
  }
  iframeApp <- shiny::tags$iframe(width = "100%", src = if (!is.null(appPath)) {
    paste0("http://127.0.0.1:", port)
  }
  else {
    url
  }, allowfullscreen = "", frameborder = "0", scrolling = "no",
  height = paste0(appHeight, "px"))
  ui <- create_app_ui(iframeApp, device, color, landscape)
  server <- function(input, output, session) {
  }
  shiny::runApp(list(ui = ui, server = server), port = port/2,
                launch.browser = TRUE)
  invisible(ui)
}

#' @title Exported function from shinyMobile
#' @importFrom htmltools htmlDependency
#' @param iframe Ver documentacion de \code{shinyMobile}
#' @param device Ver documentacion de \code{shinyMobile}
#' @param color Ver documentacion de \code{shinyMobile}
#' @param landscape Ver documentacion de \code{shinyMobile}
#' @export
create_app_ui<- function (iframe, device, color, landscape)
{
  devices_css_deps <- htmltools::htmlDependency(name = "marvel-devices-css",
                                                version = "1.0.0", src = c(file = system.file("marvel-devices-css-1.0.0",
                                                                                              package = "shinyMobile")), stylesheet = "devices.min.css")
  shiny::fluidPage(shiny::tagList(devices_css_deps, shiny::br()),
                   shiny::br(), shiny::fluidRow(align = "center", create_app_container(iframe,
                                                                                       skin = device, color = color, landscape = landscape)))
}
