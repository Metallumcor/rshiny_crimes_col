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
        selectizeInput("crime_list", label = "Variable resumen", choices = NULL),
        selectInput('kind', label = 'Tipo de mapa',
                    choices = c('Colores', 'Calor', 'Calor 2', 'Hexágonos'),
                    selected = 'Colores'),
        dateRangeInput('date', label = 'Rango de fechas', language = 'es',
                       separator = 'a', min = '2010-01-01', max = '2021-06-30',
                       startview = 'year', format = 'dd/mm/yyyy',
                       start = '2021-04-30', end = '2021-06-30'),
        actionButton('send', 'Búsqueda')
      ),
      conditionalPanel(
        'input.sidebarid == "plots"',
        selectInput('city_sel2', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        selectizeInput("crime_list2", label = "Variable resumen", choices = NULL)
      ),
      conditionalPanel(
        'input.sidebarid == "user_input"',
        selectInput('city_sel3', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        selectizeInput("crime_list3", label = "Variable resumen", choices = NULL),
        actionButton('locate', 'Encuentrame'),
        conditionalPanel("isNaN(input.map_user_click)", uiOutput("save_button"))
      ),
      conditionalPanel(
        'input.sidebarid == "forecast"',
        selectInput('city_sel4', label = 'Ciudad',
                    choices = c('Bogotá', 'Medellín', 'Cartagena', 'Londres'),
                    selected = 'Bogotá'),
        selectizeInput("units_list4", label = "Variable resumen", choices = NULL),
        selectizeInput("crime_list4", label = "Variable resumen", choices = NULL),
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
                  width = 12,
                  wellPanel(
                    column(
                      width = 6,
                      selectizeInput("rango_dia", label = strong("Rango horario"), choices = NULL)
                    ),
                    column(
                      width = 6,
                      selectizeInput("dia_sem", label = strong("Momento de la semana"),
                                     choices = c("Toda la semana","Fin de semana","Entre semana"))
                    )
                  )
                ),
                box(
                  title = "Serie mensual", status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("plot_1", height = 200)
                ),
                box(
                  title = "Lugares con más reportes",
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
              ),
              fluidRow(
                box(
                  width = 12,
                  wellPanel(
                    column(
                      width = 12,
                      dateRangeInput('date_cor', label = 'Rango de fechas', language = 'es',
                                     separator = 'a', min = '2010-01-01', max = '2021-06-30',
                                     startview = 'year', format = 'dd/mm/yyyy',
                                     start = '2020-01-01', end = '2021-06-30')
                    ),
                    column(
                      width = 6,
                      selectizeInput("sel_div", label = strong("Sector referencia"), choices = NULL)
                    ),
                    column(
                      width = 6,
                      numericInput("n_lags", label = strong("Meses de rezago"),value = 0,
                                   min = 0, max = 24, step = 1)
                    )
                  )
              ),
              box(
                width = 12,
                title = "Esparcimiento del delito - Matriz de correlación",
                solidHeader = TRUE,
                plotOutput("plot_5", height = 350)
              ),
              box(
                width = 12,
                title = "Esparcimiento del delito - Mapa de correlación",
                solidHeader = TRUE,
                leafletOutput('map_corr')
              )
            ),
            fluidRow(
              box(
                width = 12,
                wellPanel(
                  column(
                    width = 12,
                    selectizeInput("div_pca", label = strong("Seleccionar localización"), choices = NULL)
                  )
                )
              ),
              box(
                width = 12,
                title = "Análisis de componetes principales",
                solidHeader = TRUE,
                plotly::plotlyOutput("plot_6", height = 350)
              )
            )
      ),
      tabItem(tabName = 'user_input',
        tags$style(type = "text/css", "#map_user {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map_user")
      ),
      tabItem(tabName = 'forecast',
              fluidRow(
                plotly::plotlyOutput("plot_forecast", height = 400)
              )
      )
    )
  )
)
