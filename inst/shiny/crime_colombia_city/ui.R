ui <- dashboardPage(
  dashboardHeader(
    title = 'Mapa de datos'
  ),
  dashboardSidebar(
    sidebarMenu(
      id = 'sidebarid',
      menuItem('Mapas', tabName = 'maps', icon = icon('map-marked-alt')),
      menuItem('Gráficas', tabName = 'plots', icon = icon('chart-bar')),
      conditionalPanel(
        'input.sidebarid == "maps"',
        selectInput('city_sel', label = 'Ciudad',
                   choices = c('Bogotá', 'Medellín', 'Cartagena'),
                  selected = 'Bogotá'),
        shinyWidgets::airDatepickerInput('date', language = 'es', label = 'Año y mes',
                                         value = format(Sys.Date(), '%Y-%m-01'),
                                         minDate = '2010-01-01', maxDate = '2021-03-01',
                                         view = 'months', minView = 'months', dateFormat = 'yyyy-mm'),
        selectInput('kind', label = 'Tipo de mapa',
                   choices = c('Colores', 'Calor', 'Hexágonos'),
                  selected = 'Colores'),
        selectInput('crime', label = 'Delito',
                   choices = 'Hurto de celulares'),
        actionButton('send', 'Búsqueda')
      ),
      conditionalPanel(
        'input.sidebarid == "plots"',
        selectInput('city_sel2', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena'),
                    selected = 'Bogotá'),
        selectInput('crime2', label = 'Delito',
                    choices = 'Hurto de celulares')
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'maps',
              # Mapa
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              leafletOutput('map')
      ),
      tabItem(tabName = 'plots',
              fluidRow(
                box(
                  title = "Evolución en el tiempo", status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("plot_1", height = 200)
                ),
                box(
                  title = "Lugares más peligrosos",
                  status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("plot_2", height = 200)
                )
              ),
              fluidRow(
                box(
                  title = "Víctimas por sexo", status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("plot_3", height = 200)
                ),
                box(
                  title = "Hora del día", status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("plot_4", height = 200)
                )
              )
      )
    )
  )
)
