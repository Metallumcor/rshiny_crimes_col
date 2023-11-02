# UI para mapa de crímen de Londres
#' @import leaflet
#' @import shinyWidgets
ui <- bootstrapPage(
  # El estilo de la pag
  tags$head(
    tags$link(href = 'https://fonts.googleapis.com/css2?family=Noto+Sans+JP&display=swap',
              rel = 'stylesheet'),
    tags$style(type = "text/css", "html, body {width:100%;height:100%;
               font-family: 'Noto Sans JP', sans-serif;}")
  ),

  # El mapa
  leafletOutput('map', width = '100%', height = '100%'),

  # Panel de texto - Titulo
  absolutePanel(
    top = 10, right = 10,
    tags$h1('(Demo) Mapa de Crímen - Londres'),
    tags$h3('PredicTech S.A.S')
  ),

  # Panel de seleccion de distrito y fecha
  absolutePanel(
    top = 100, left = 20, draggable = FALSE,
    #textInput('district', 'Escribe el nombre completo del distrito en inglés',
    #          placeholder = 'E.g. City of London'),
    selectInput('district', label = 'Selecciona el nombre del distrito',
                choices = unique(lon_poly_ward_b$DISTRICT)),
    shinyWidgets::airDatepickerInput('date', language = 'es', label = 'Elige el año y mes',
                       value = format(Sys.Date(), '%Y-%m-01'),
                       minDate = '2010-01-01', maxDate = format(Sys.Date(), '%Y-%m-01'),
                       view = 'months', minView = 'months', dateFormat = 'yyyy-mm'),
    actionButton('send', 'Búsqueda')
  )
)
