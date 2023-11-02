#' @import leaflet
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

  # Panel de seleccion de distrito y fecha
  absolutePanel(
    top = 60, left = 20, draggable = FALSE, height = 'auto',
    bottom = 'auto', right = 'auto',
    width = 350,
    # Titulo
    h2(strong('Mapa de delitos')),
    # Inputs
    selectInput('kind', label = 'Tipo de mapa',
                choices = c('Colores', 'Calor', 'Hexágonos'),
                selected = 'Colores'),
    selectInput('crime', label = 'Tipo de crimen',
                choices = look_colombia_crime$selector[c(1,3,7,8,9,12,13,14)]),
    dateRangeInput('date', label = 'Rango de fechas', language = 'es',
                   separator = 'a', min = '2010-01-01', max = '2021-01-30',
                   startview = 'year', format = 'dd/mm/yyyy',
                   start = '2021-01-30', end = '2021-01-30'),
    actionButton('send', 'Búsqueda')
  )
)
