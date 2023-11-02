#' @import leaflet
server <- function(input, output, session){

  # Pre-procesamiento
  df_pre <- test2021:::london_prerend
  # Paleta de colores
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  # Bolean de cloropleta
  # Variable para guardar df_crime
  rv <- reactiveValues(df_crime = NULL)

  # Mapa renderizado sobre Londres
  output$map <- renderLeaflet({
    test2021:::london_map
  })

  # Observador de eventos asociados al boton
  observeEvent(input$send,{
    rv$district_f <- NULL
    withProgress(
      message = 'Cargando datos...',
      value = 1/5,
      expr = {
        validate(
          need(input$crime, message = 'Se requiere un Distrito')
        )

        crime <- input$crime
        date_sel <- format(input$date,'%Y-%m')

        incProgress(1/5)

        tryCatch(
          expr = {
            r_df <- test2021:::list_to_df(db_crime_london_a)
            rv$df_crime <- as.data.frame(
              test2021:::dt_filter_crime(x = as.data.table(r_df),
                                         list('category' = crime,
                                              'month' = date_sel))
            )
            rv$df_crime$latitude <- as.numeric(rv$df_crime$latitude)
            rv$df_crime$longitude <- as.numeric(rv$df_crime$longitude)
            obs_df <- get_cl(rv$df_crime, grid_size = c(500,500))
              },
          error = function(x){
            obs_df <- NULL
          }
        )

        incProgress(1/5)

        tryCatch(
          expr = {

            leafletProxy('map', data = obs_df$sp_obj) %>%
              clearShapes() %>%
              clearControls() %>%
              clearMarkers() %>%
              clearMarkerClusters() %>%
              setView(lng = -0.135278,lat = 51.494720, zoom = 10) %>%
              addPolygons(
                color = heat.colors(obs_df$cl_nlev, 0.5)[obs_df$cl_lev],
                weight = 2,
                fillOpacity = 0.4, smoothFactor = 0.5
                ) %>%
              addLegend("bottomright",
                        colors = heat.colors(obs_df$cl_nlev, NULL),
                        title = paste0('Ocurrencias de "',
                                       input$crime, '"'),
                        labels = paste0("Hasta ",unique(obs_df$cl_lev))
                        )

            incProgress(1/5)
          },
          error = function(e){
            showModal(modalDialog(title = 'Lo sentimos!',
                                  tags$p('La solicitud no puede ser procesada de momento')))
          }
        )

        incProgress(1/5)
      }
    )
  })

}
