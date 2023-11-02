ui <- dashboardPage(
  dashboardHeader(
    title = 'Mapa de datos'
  ),
  dashboardSidebar(
    sidebarMenu(
      id = 'sidebarid',
      menuItem('Mapas', tabName = 'maps', icon = icon('map-marked-alt')),
      menuItem('Gráficas', tabName = 'plots', icon = icon('chart-bar')),
      menuItem('¡Reporta un delito!', tabName = 'user_input',
               icon = icon('flag')),
      menuItem('Proyecciones', tabName = 'forecast', icon = icon('forward')),
      conditionalPanel(
        'input.sidebarid == "maps"',
        selectInput('city_sel', label = 'Ciudad',
                   choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                  selected = 'Bogotá'),
        uiOutput("crime_list"),
        selectInput('kind', label = 'Tipo de mapa',
                    choices = c('Colores', 'Calor', 'Calor 2', 'Hexágonos'),
                    selected = 'Colores'),
        dateRangeInput('date', label = 'Rango de fechas', language = 'es',
                       separator = 'a', min = '2010-01-01', max = '2021-04-30',
                       startview = 'year', format = 'dd/mm/yyyy',
                       start = '2021-04-30', end = '2021-04-30'),
        actionButton('send', 'Búsqueda')
      ),
      conditionalPanel(
        'input.sidebarid == "plots"',
        selectInput('city_sel2', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        uiOutput("crime_list2")
      ),
      conditionalPanel(
        'input.sidebarid == "user_input"',
        selectInput('city_sel3', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        uiOutput("crime_list3"),
        actionButton('locate', 'Encuentrame'),
        conditionalPanel("isNaN(input.map_user_click)", uiOutput("save_button"))
      ),
      conditionalPanel(
        'input.sidebarid == "forecast"',
        selectInput('city_sel4', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        uiOutput("units_list4"),
        uiOutput("crime_list4"),
        actionButton('send4', 'Crear pronosticó')
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
      ),
      tabItem(tabName = 'user_input',
        fluidRow(

        ),
        tags$style(type = "text/css", "#map_user {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map_user"),
      ),
      tabItem(tabName = 'forecast',
              fluidRow(
                plotly::plotlyOutput("plot_forecast", height = 400)
              )
      )
    )
  )
)
