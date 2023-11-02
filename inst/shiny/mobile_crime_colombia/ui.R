ui <- shinyMobile::f7Page(
  title = "Demo de App - Tabs",
  tags$head(
    tags$meta(name = "viewport",
             content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
  ),
  shinyMobile::f7TabLayout(
    panels = tagList(
      shinyMobile::f7Panel(
        title = "Opciones",
        side = "left",
        theme = "light",
        effect = "cover",
        shinyMobile::f7Select('city_sel', label = 'Ciudad',
                  choices = c('Bogotá', 'Medellín', 'Cartagena'),
                  selected = 'Bogotá'),
        shinyMobile::f7Select('crime', label = 'Delito',
                   choices = 'Hurto de bicicletas'),
        shinyMobile::f7DatePicker('date', label = 'Fecha inicial', direction = "vertical",
                       min = '2010-01-01', max = '2021-03-30'),
        shinyMobile::f7Button('locate', 'Encuentrame'),
        shinyMobile::f7Button('report', 'Reportar')
      )
    ),
    navbar = shinyMobile::f7Navbar(
      title = "Mapa de datos",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = FALSE
    ),
    shinyMobile::f7Tabs(
      id = "tabsapp",
      animated = TRUE,
      shinyMobile::f7Tab(
        tabName = "Mapas",
        icon = shinyMobile::f7Icon("map_fill"),
        active = TRUE,
        shinyMobile::f7Shadow(
          intensity = 10,
          hover = TRUE,
          shinyMobile::f7Card(
            leafletOutput("map")
          )
        )
      )
    )
  )
)
