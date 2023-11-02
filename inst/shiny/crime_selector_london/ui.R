#' @import leaflet
#' @import shinyWidgets
#' @importFrom shinyjs useShinyjs
ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  # El estilo de la pag
  tags$head(
    tags$link(href = 'https://fonts.googleapis.com/css2?family=Noto+Sans+JP&display=swap',
              rel = 'stylesheet'),
    tags$style(type = "text/css", "html, body {width:100%;height:100%;
               font-family: 'Noto Sans JP', sans-serif;}")
  ),

  # El mapa
  leafletOutput('map', width = '100%', height = '100%'),

  # Panel de seleccion de distrito y fecha
  absolutePanel(
    top = 100, left = 20, draggable = FALSE,
    selectInput('crime', label = 'Selecciona el tipo de crimen',
                choices = test2021:::fast_lon_a$cat_lev),
    shinyWidgets::airDatepickerInput('date', language = 'es', label = 'Elige el año y mes',
                       value = paste(test2021:::fast_lon_a$date_rng[2], '01', sep = '-'),
                       minDate = paste(test2021:::fast_lon_a$date_rng[1], '01', sep = '-'),
                       maxDate = paste(test2021:::fast_lon_a$date_rng[2], '01', sep = '-'),
                       view = 'months', minView = 'months', dateFormat = 'yyyy-mm'),
    actionButton('send', 'Búsqueda')
  ),

  # Boton de reset
  conditionalPanel("isNaN(input.map_shape_click)", uiOutput("controls"))
)
